# =============================================================================
# Education Transition Probabilities  (longitudinal CPS matched pairs)
# Uses years-of-education so within-category accumulation is captured.
# Upgrade definitions:
#   hs_upgrade      : yoe_1 < 12  &  yoe_2 >= 12
#   college_upgrade : 12 <= yoe_1 < 16  &  yoe_2 >= 16
# =============================================================================

library(ipumsr)
library(dplyr)
library(tidyr)
library(readr)

readRenviron("C:/Users/kchanwong/Documents/API_KEY.Renviron")
set_ipums_api_key(Sys.getenv("IPUMS_API"), save = TRUE)

# =============================================================================
# 1. LOAD
# =============================================================================
df <- read_ipums_micro("C:/Users/kchanwong/Downloads/cps_00130.xml")

# =============================================================================
# 2. RECODE — EDUC code → years of education
# =============================================================================

educ_to_yoe <- function(x) {
  case_when(
    x <= 2   ~  0,
    x == 10  ~  4,
    x == 20  ~  6,
    x == 30  ~  8,
    x == 40  ~  9,
    x == 50  ~ 10,
    x == 60  ~ 11,
    x == 71  ~ 11,   # 12th grade, no diploma
    x == 73  ~ 12,   # HS diploma / GED
    x == 80  ~ 13,
    x == 81  ~ 13,
    x == 90  ~ 14,   # AA degree
    x == 91  ~ 14,
    x == 92  ~ 14,
    x == 100 ~ 16,   # BA/BS
    x == 110 ~ 18,   # MA/MS
    x == 111 ~ 18,   # Professional
    x == 123 ~ 21,   # PhD
    x == 124 ~ 21,
    TRUE     ~ NA_real_
  )
}

clean <- df |>
  filter(ASECFLAG_1 == 1, AGE_1 >= 19L, AGE_1 <= 89L) |>
  mutate(
    year      = as.integer(YEAR_1),
    age       = as.integer(AGE_1),
    sex       = as.integer(SEX_1),
    cohort_grp = cut(
      year - age,
      breaks = seq(1895, 2010, by = 5),
      labels = paste0(seq(1895, 2005, by = 5), "-", seq(1899, 2009, by = 5)),
      right  = TRUE
    ),
    age_grp = cut(
      age,
      breaks = c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
      labels = c("19-25","26-30","31-35","36-40","41-45","46-50",
                 "51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90"),
      right  = TRUE
    ),
    yoe_1 = educ_to_yoe(as.integer(EDUC_1)),
    yoe_2 = educ_to_yoe(as.integer(EDUC_2)),
    wt    = as.numeric(ASECWT_1)
  ) |>
  filter(!is.na(yoe_1), !is.na(yoe_2), !is.na(age_grp), !is.na(cohort_grp)) |>
  select(year, sex, cohort_grp, age_grp, yoe_1, yoe_2, wt)

# =============================================================================
# 3. TRANSITION PROBABILITIES
#    hs_upgrade      : among yoe_1 < 12,          prob(yoe_2 >= 12)
#    college_upgrade : among 12 <= yoe_1 < 16,    prob(yoe_2 >= 16)
# =============================================================================

by_vars <- c("year", "sex", "cohort_grp", "age_grp")

hs_probs <- clean |>
  filter(yoe_1 < 12) |>
  group_by(across(all_of(by_vars))) |>
  summarise(
    prob_upgrade = sum(wt[yoe_2 >= 12], na.rm = TRUE) / sum(wt),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(educ = "hs", prob_upgrade = pmax(0, pmin(1, prob_upgrade)))

college_probs <- clean |>
  filter(yoe_1 >= 12, yoe_1 < 16) |>
  group_by(across(all_of(by_vars))) |>
  summarise(
    prob_upgrade = sum(wt[yoe_2 >= 16], na.rm = TRUE) / sum(wt),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(educ = "some_college", prob_upgrade = pmax(0, pmin(1, prob_upgrade)))

edu_transitions <- bind_rows(hs_probs, college_probs) |>
  select(year, sex, cohort_grp, age_grp, educ, prob_upgrade, n)

edu_transitions |>
  filter(prob_upgrade > 0) |>
  group_by(year, sex, age_grp, educ) %>%
  summarise(prob_upgradse = sum(prob_upgrade)) %>%
  print(n = 100)

# =============================================================================
# 4. SAVE
# =============================================================================
write_csv(edu_transitions, "edu_transition_probs_cohort.csv")
