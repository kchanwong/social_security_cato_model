# =============================================================================
# Education Transition Probabilities
# P(educ moves up one level | t, age_grp, sex, marst)
# educ levels: "hs" < "some_college" < "ba_plus"  (matches initial_simulation.csv)
# =============================================================================

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

readRenviron("C:/Users/kritc/OneDrive/Documents/API_KEYS.Renviron")
set_ipums_api_key(Sys.getenv("IPUMS_API"), save = TRUE)

# =============================================================================
# 1. PULL CPS ASEC
# =============================================================================

cps_extract <- define_extract_cps(
  description = "Education transitions by age/sex/marst",
  samples     = paste0("cps", 1990:2023, "_03s"),
  variables   = c("YEAR", "ASECWT", "AGE", "SEX", "MARST", "EDUC", "ASECFLAG")
)

submitted    <- submit_extract(cps_extract)
downloadable <- wait_for_extract(submitted)
files        <- download_extract(downloadable)
raw <- read_ipums_micro(files)

# =============================================================================
# 2. RECODE — educ levels match initial_simulation.csv exactly
# =============================================================================

EDUC_LEVELS <- c("hs", "some_college", "ba_plus")   # ordered low → high

clean <- raw |>
  # AGE >= 19: education is locked to "hs" before 18; 19+ is the first year
  # someone could plausibly have upgraded (finished HS at 18, started college at 19)
  filter(ASECFLAG == 1, AGE >= 19, AGE <= 89) |>
  mutate(
    year  = as.integer(YEAR),
    sex   = as.integer(SEX),
    marst = case_when(
      MARST %in% 1:2 ~ "married",
      MARST %in% 3:4 ~ "divorced",
      MARST == 5     ~ "widowed",
      MARST == 6     ~ "single",
      TRUE           ~ NA_character_
    ),
    age_grp = cut(
      as.integer(AGE),
      breaks = c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      labels = c("19-25","26-30","31-35","36-40","41-45","46-50",
                 "51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90"),
      right  = TRUE
    ),
    educ = case_when(
      EDUC <= 73              ~ "hs",
      EDUC %in% c(80,81,90,
                  91,92)     ~ "some_college",
      EDUC >= 100             ~ "ba_plus",
      TRUE                   ~ NA_character_
    ),
    educ = factor(educ, levels = EDUC_LEVELS, ordered = TRUE),
    wt   = as.numeric(ASECWT)
  ) |>
  filter(!is.na(marst), !is.na(educ), !is.na(age_grp)) |>
  select(year, sex, marst, age_grp, educ, wt)

# =============================================================================
# 3. ESTIMATE f AND F PER CELL
# =============================================================================

cell_distributions <- clean |>
  group_by(year, sex, marst, age_grp) |>
  summarise(
    pmf = list({
      wts <- tapply(wt, educ, sum, default = 0)
      as.numeric(wts[EDUC_LEVELS] / sum(wts))   # f(e | t, a, g)  length-3 vector
    }),
    .groups = "drop"
  ) |>
  mutate(cdf = map(pmf, cumsum))                 # F(e | t, a, g)

# =============================================================================
# 4. COMPUTE TRANSITION PROBABILITIES
# =============================================================================

age_levels <- levels(clean$age_grp)
edu_transitions <- expand_grid(
  year    = sort(unique(clean$year))[-length(sort(unique(clean$year)))],
  sex     = c(1L, 2L),
  marst   = c("married", "divorced", "widowed", "single"),
  age_grp = age_levels,
  educ    = EDUC_LEVELS[-length(EDUC_LEVELS)]    # "hs", "some_college" only (ba_plus can't go higher)
) |>
  mutate(
    educ_next = EDUC_LEVELS[match(educ, EDUC_LEVELS) + 1L],
    age_grp_t1 = {
      idx <- match(age_grp, age_levels)
      if_else(idx < length(age_levels), age_levels[idx + 1L], NA_character_)
    }
  ) |>
  filter(!is.na(age_grp_t1)) |>
  rowwise() |>
  mutate(
    e_idx      = match(educ,      EDUC_LEVELS),
    e1_idx     = match(educ_next, EDUC_LEVELS),

    row_now  = list(cell_distributions[
                  cell_distributions$year    == year     &
                  cell_distributions$sex     == sex      &
                  cell_distributions$marst   == marst    &
                  cell_distributions$age_grp == age_grp, ]),
    row_next = list(cell_distributions[
                  cell_distributions$year    == year + 1L &
                  cell_distributions$sex     == sex       &
                  cell_distributions$marst   == marst     &
                  cell_distributions$age_grp == age_grp_t1, ]),

    f_et       = if (nrow(row_now)  > 0) row_now$pmf[[1L]][e_idx]  else NA_real_,
    F_et_now   = if (nrow(row_now)  > 0) row_now$cdf[[1L]][e_idx]  else NA_real_,
    F_et1_next = if (nrow(row_next) > 0) row_next$cdf[[1L]][e1_idx] else NA_real_,

    prob_upgrade = if_else(
      !is.na(f_et) & f_et > 0,
      pmax(0, pmin(1, (F_et_now - F_et1_next) / f_et)),
      NA_real_
    )
  ) |>
  ungroup() |>
  select(year, sex, marst, age_grp, educ, prob_upgrade)

# =============================================================================
# 5. SAVE
# =============================================================================

write_csv(edu_transitions, "data/edu_transition_probs.csv")
message("Saved  (", nrow(edu_transitions), " rows, ",
        sum(!is.na(edu_transitions$prob_upgrade)), " non-missing)")
