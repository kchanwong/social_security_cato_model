library(ipumsr)
library(dplyr)
setwd("C:/Users/kchanwong/Documents/TEST")
set_ipums_api_key("", save = TRUE)
extract <- define_extract_micro(
  description = "ACS 2008 synthetic pop base",
  samples     = "us2008a",
  collection = 'usa',
  variables   = c(
    "SERIAL",
    "HHWT",
    "PERNUM",
    "PERWT",
    "FAMUNIT",
    "SCHOOL",
    "AGE",
    "SEX",
    "EDUC",
    'INCSS',
    "LABFORCE",
    "EMPSTAT",
    "INCWAGE",
    "WORKEDYR"
  )
)
submitted <- submit_extract(extract)
downloadable <- wait_for_extract(submitted)
files <- download_extract(downloadable, download_dir = "C:/Users/kchanwong/Documents/TEST")
raw <- read_ipums_micro("C:/Users/kchanwong/Documents/TEST/usa_00176.xml")
acs <- raw |>
  transmute(
    serial  = SERIAL,
    famunit = paste0(SERIAL, "_", FAMUNIT),
    perwt   = PERWT,
    hhwt    = HHWT,
    age = AGE,
    school = case_when(
      SCHOOL == 1 ~ "no",
      TRUE ~ "yes"
    ),
    sex = case_when(
      SEX == 1 ~ 1,
      SEX == 2 ~ 2
    ),
    educ = case_when(
      EDUCD < 65          ~ "hs",
      EDUCD %in% 65:100   ~ "some_college",
      EDUCD > 100         ~ "ba_plus",
      .default = NA_character_
    ),
    labforce = case_when(
      LABFORCE == 2 ~ "yes",
      LABFORCE == 1 ~ "no",
      .default = NA_character_
    ),
    employed = case_when(
      EMPSTAT == 1 ~ "yes",
      EMPSTAT %in% c(2,3) ~ "no",
      .default = NA_character_
    ),
    retired = case_when(
      EMPSTAT == 3 & age >= 62 & WORKEDYR == 3 ~ "yes",
      TRUE ~ "no"
    ),
    incwage = if_else(INCWAGE %in% c(999998, 999999), NA_real_, as.double(INCWAGE)),
    receive_ss = ifelse(INCSS %in% c(999998, 999999) | INCSS == 0, "no", "yes"),
    cohort = ((2008 - AGE) %/% 10) * 10
  )

res_tbl <- acs |>
  group_by(famunit) |>
  summarise(
    n_res = n(),
    omega = mean(perwt),
    .groups = "drop"
  )

tn_tbl <- res_tbl |>
  group_by(n_res) |>
  summarise(sum_omega = sum(omega), .groups = "drop") |>
  mutate(T_n = sum_omega / sum(sum_omega))

SMALL_GROUP <- max(res_tbl$n_res) + 1L

res_tbl <- res_tbl |>
  left_join(tn_tbl |> select(n_res, T_n), by = "n_res") |>
  mutate(size_group = if_else(T_n >= 0.05, as.integer(n_res), SMALL_GROUP))

group_tn <- res_tbl |>
  group_by(size_group) |>
  summarise(sum_omega = sum(omega), .groups = "drop") |>
  mutate(T_n_group = sum_omega / sum(sum_omega))

N_target <- 50000
res_with_group <- res_tbl |>
  left_join(group_tn |> select(size_group, T_n_group), by = "size_group")

bootstrap_ids <- res_with_group |>
  group_by(size_group) |>
  group_map(function(df, key) {
    n_draw <- max(1L, round(unique(df$T_n_group) * N_target))
    tibble(famunit = sample(df$famunit, size = n_draw, replace = TRUE,
                            prob = df$omega / sum(df$omega)))
  }) |>
  bind_rows() |>
  mutate(draw_id = row_number())

sim_pop <- bootstrap_ids |>
  left_join(acs, by = "famunit") |>
  mutate(famunit = draw_id) |>
  select(-draw_id, -serial)

sim_pop <- sim_pop %>%
  group_by(famunit) %>%
  mutate(perwt = mean(hhwt)) %>%
  ungroup()
### Decompose Wage Components ###
wage_model <- readRDS("wage_model.rds")
sim_pop %>%
  mutate(predict_hat = exp(predict(
    wage_model,
    newdata = sim_pop %>% rename(
      AGE        = age,
      SEX        = sex,
      EDUC_GROUPS = educ,
      COHORT     = cohort,
      RECEIVE_SS = receive_ss
    )
  ))) %>%
  mutate(
    ped = ifelse(employed == "yes" & age > 18 & incwage > 0, log(incwage+1) - log(predict_hat), 0))  %>%
    write.csv('initial_simulation.csv', row.names = FALSE)

