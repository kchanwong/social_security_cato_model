# =============================================================================
# ASEC Labor Force Transitions: 2007–2024
# Estimates annual transition probabilities by SEX, MARST, EDUC, and LF_{t-1}
# Compatible with initial_simulation.csv codings
#
# Requires: ipumsr, dplyr, tidyr, purrr, readr
# IPUMS API key: set via set_ipums_api_key() or IPUMS_API_KEY env variable
# =============================================================================

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
readRenviron("C:/Users/kritc/OneDrive/Documents/API_KEYS.Renviron")
# =============================================================================
# PART 1 — LOAD AND RECODE
# =============================================================================

ddi <- read_ipums_ddi("C:/Users/kritc/Downloads/cps_00128.xml")
transitions_raw <- read_ipums_micro(
  ddi
) |>
  filter(
    AGE_1      >= 16,
    LABFORCE_1 %in% c(1L, 2L),
    LABFORCE_2 %in% c(1L, 2L),
    ASECFLAG_1 == 1,              # keep ASEC records only
    ASECFLAG_2 == 1
  ) |>
  mutate(
    year_t = as.integer(YEAR_1),
 
    # --- SEX (use _1; should be time-invariant) ---
    sex_t = as.integer(SEX_1),
 
    # --- MARST at t ---
    marst_t = case_when(
      MARST_1 %in% 1:2 ~ "married",
      MARST_1 %in% 3:4 ~ "divorced",
      MARST_1 == 5     ~ "widowed",
      MARST_1 == 6     ~ "single",
      TRUE             ~ NA_character_
    ),
 
    # --- EDUC at t ---
    educ_t = case_when(
      EDUC_1 <= 73              ~ "hs",
      EDUC_1 %in% c(80, 81, 90,
                    91, 92)     ~ "some_college",
      EDUC_1 >= 100             ~ "ba_plus",
      TRUE                      ~ NA_character_
    ),
 
    # --- LABFORCE at t and t+1 ---
    lf_t  = if_else(LABFORCE_1 == 2L, "yes", "no"),
    lf_t1 = if_else(LABFORCE_2 == 2L, "yes", "no"),
 
    # --- AGE at t ---
    age_t = as.integer(AGE_1),
 
    # --- Weight: use year-t ASEC person weight ---
    wt_t = as.numeric(ASECWT_1)
  ) |>
  filter(
    !is.na(marst_t),
    !is.na(educ_t),
    # Guard against likely mis-links
    SEX_1 == SEX_2,
    !(EDUC_1 >= 100 & EDUC_2 <= 73)
  ) |>
  select(year_t, sex_t, age_t, marst_t, educ_t, lf_t, lf_t1, wt_t)
 
# =============================================================================
# PART 3 — ESTIMATE TRANSITION PROBABILITIES
# =============================================================================
 
transition_probs <- transitions_raw |>
  group_by(
    year_t,
    sex    = sex_t,
    age    = age_t,
    lf_lag = lf_t
  ) |>
  summarise(
    n_total  = n(),
    n_wt     = sum(wt_t, na.rm = TRUE),
    n_to_yes = sum(wt_t[lf_t1 == "yes"], na.rm = TRUE),
    n_to_no  = sum(wt_t[lf_t1 == "no"],  na.rm = TRUE),
    .groups  = "drop"
  ) |>
  mutate(
    prob_lf_yes = n_to_yes / n_wt,
    prob_lf_no  = n_to_no  / n_wt
  )
# =============================================================================
# PART 4 — SAVE OUTPUTS
# =============================================================================
transition_probs %>% 
    select(year_t, sex, age, lf_lag, contains('prob')) %>%
    write.csv('lf_transition_prob.csv', row.names = FALSE)
