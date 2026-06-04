library(tidyr)
library(dplyr)
library(purrr)
library(broom)

# --- Chetty data ---
clean_chetty <- as_tibble(read.csv(
    "C:/Users/kritc/Downloads/health_ineq_online_table_16.csv"
)) |>
    mutate(
        cohort   = yod - age_at_d,
        quintile = floor(indv_pctile / 20) * 20
    ) |>
    select(-indv_pctile) |>
    group_by(gnd, age_at_d, quintile, cohort, yod) |>
    summarise(
        total_deaths = sum(deaths),
        total_count  = sum(count),
        .groups = "drop"
    ) |>
    mutate(mort_rate = total_deaths / total_count)

# --- SSA cohort life tables ---
df_ssa <- read.csv(
    "C:/Users/kritc/Downloads/CohLifeTables_M_Alt2_TR2024.csv"
) |>
    mutate(SEX = 1) |>
    rbind(
        read.csv(
            "C:/Users/kritc/Downloads/CohLifeTables_F_Alt2_TR2024.csv"
        ) |> mutate(SEX = 2)
    ) |>
    as_tibble() |>
    mutate(
        gnd    = if_else(SEX == 1, "M", "F"),
        cohort = Year - x
    )

ssa_rates <- df_ssa |>
    rename(age_at_d = x, mort_rate_ssa = q.x.) |>
    select(gnd, age_at_d, cohort, mort_rate_ssa)

year_in <- clean_chetty |> distinct(cohort) |> pull()

# --- Per-group Gompertz (observed Chetty cohorts, extrapolates 77-100) ---
models <- clean_chetty |>
    group_by(gnd, quintile, cohort) |>
    nest() |>
    mutate(
        fit   = map(data, ~ glm(mort_rate ~ age_at_d, data = .x,
            family = binomial(link = "log"), start = c(-10, 0.1))),
        coefs = map(fit, tidy)
    ) |>
    unnest(coefs) |>
    select(gnd, quintile, cohort, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    rename(intercept = `(Intercept)`, slope = age_at_d)

# SSA rates for ages < 40, replicated across quintiles (no income gradient)
young_mort <- df_ssa |>
    filter(x < 40) |>
    mutate(quintile = list(seq(0, 80, by = 20))) |>
    unnest(quintile) |>
    rename(age_at_d = x) |>
    select(cohort, age_at_d, mort_rate = q.x., gnd, quintile)

# Blend income gradient in at ages 35-45, anchor level to SSA
normalize_to_ssa <- function(df) {
    df |>
        left_join(ssa_rates, by = c("gnd", "age_at_d", "cohort")) |>
        group_by(gnd, age_at_d, cohort) |>
        mutate(
            rel_diff  = mort_rate / mean(mort_rate, na.rm = TRUE),
            w         = pmin(pmax((age_at_d - 35) / 10, 0), 1),
            blended   = rel_diff^w,
            mort_rate = mort_rate_ssa * blended / mean(blended, na.rm = TRUE)
        ) |>
        ungroup() |>
        select(gnd, age_at_d, quintile, cohort, mort_rate)
}

# --- Observed cohorts: SSA < 40, Chetty 40-76, Gompertz 77+ ---
full_mort_observed <- expand_grid(
    gnd      = c("M", "F"),
    age_at_d = 0:100,
    quintile = seq(0, 80, by = 20),
    cohort   = year_in
) |>
    left_join(
        young_mort |> select(-quintile),
        by = c("gnd", "age_at_d", "cohort")
    ) |>
    rename(mort_ssa = mort_rate) |>
    left_join(
        clean_chetty |> select(gnd, age_at_d, quintile, cohort, mort_rate),
        by = c("gnd", "age_at_d", "quintile", "cohort")
    ) |>
    left_join(models, by = c("gnd", "quintile", "cohort")) |>
    mutate(mort_rate = case_when(
        age_at_d < 40     ~ mort_ssa,
        !is.na(mort_rate) ~ mort_rate,
        TRUE              ~ pmin(exp(intercept + slope * age_at_d), 1)
    )) |>
    select(gnd, age_at_d, quintile, cohort, mort_rate) |>
    normalize_to_ssa() |>
    distinct()

# --- Future cohorts: pooled Gompertz with cohort time trend ---
models_pooled <- clean_chetty |>
    group_by(gnd) |>
    nest() |>
    mutate(fit = map(data, ~ glm(
        mort_rate ~ age_at_d * cohort + age_at_d * quintile,
        data    = .x,
        family  = binomial(link = "log"),
        weights = total_count
    )))

future_cohorts <- setdiff(
    unique(df_ssa$cohort[df_ssa$cohort > max(year_in)]),
    year_in
)

full_mort_future <- expand_grid(
    gnd      = c("M", "F"),
    age_at_d = 0:100,
    quintile = seq(0, 80, by = 20),
    cohort   = future_cohorts
) |>
    group_by(gnd) |>
    nest(grid = c(age_at_d, quintile, cohort)) |>
    left_join(models_pooled |> select(gnd, fit), by = "gnd") |>
    mutate(
        pred = map2(fit, grid, ~ predict(.x, newdata = .y, type = "response")),
        grid = map2(grid, pred, ~ mutate(.x, mort_rate = .y))
    ) |>
    select(gnd, grid) |>
    unnest(grid) |>
    normalize_to_ssa() |>
    na.omit()

# --- Combined ---
full_mort <- bind_rows(full_mort_observed, full_mort_future)
