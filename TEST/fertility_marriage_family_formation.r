rm(list = ls())
library(ipumsr)
library(dplyr)
library(tidyr)
library(broom)
library(demography)
library(forecast)
setwd("C:/Users/kritc/OneDrive/Documents/GitHub/social_security_cato_model/TEST")
initial_sample <- read.csv('initial_simulation.csv')
initial_sample <- as_tibble(initial_sample)
### Fertility ###
birth_by_age_raw <- read.csv("C:/Users/kritc/Downloads/Natality, 2007-2024 (1).csv") 
birth_by_age_raw <- as_tibble(birth_by_age_raw) %>%
  select(-Notes) %>%
  select(Age.of.Mother.9, Marital.Status, Year, Births) %>%
  na.omit() %>%
  mutate(Marital.Status = ifelse(Marital.Status == 'Not Reported', 
  'Unmarried', Marital.Status)) %>%
  group_by(Age.of.Mother.9, Marital.Status, Year) %>%
  summarise(Births = sum(Births)) %>%
  rename(Age_Band = Age.of.Mother.9, Marital_Status = Marital.Status)
### Raw Population Counts ###
raw_population <- read.csv("C:/Users/kritc/Downloads/SSPopJan_TR2023.csv")
grouped_population <- as_tibble(raw_population) %>% 
  mutate(Age_Band = case_when(
    Age < 15              ~ "Under 15 years",
    Age >= 15 & Age < 20  ~ "15-19 years",
    Age >= 20 & Age < 25  ~ "20-24 years",
    Age >= 25 & Age < 30  ~ "25-29 years",
    Age >= 30 & Age < 35  ~ "30-34 years",
    Age >= 35 & Age < 40  ~ "35-39 years",
    Age >= 40 & Age < 45  ~ "40-44 years",
    Age >= 45 & Age < 50  ~ "45-49 years",
    Age >= 50             ~ "50 years and over"
  )) %>%
  group_by(Year, Age_Band) %>%
  summarise(Total_Married = sum(F.Mar + F.Wid),
            Total_Single = sum(F.Tot) - sum(F.Mar + F.Wid),
            .groups = "drop") %>%
  pivot_longer(cols = c(Total_Married, Total_Single),
               names_to = "Marital_Status",
               values_to = "Population") %>%
  mutate(Marital_Status = recode(Marital_Status,
                                 Total_Married = "Married",
                                 Total_Single  = "Unmarried"))

### ASFR Calculate ###
asfr <- grouped_population %>%
  filter(Year >= 2007) %>%
  left_join(birth_by_age_raw) %>%
  mutate(Births = ifelse(is.na(Births), 0, Births)) %>%
  mutate(ASFR = Births/Population)
# ── Hyndman-Ullah FDA ─────────────────────────────────────────────────────────
# Age band midpoints — fdm() needs a numeric age axis
age_midpoints <- c(17, 22, 27, 32, 37, 42, 47)
age_band_levels <- c("15-19 years","20-24 years","25-29 years",
                     "30-34 years","35-39 years","40-44 years","45-49 years")

# Pivot to matrix: rows = age (ordered), cols = year
asfr_wide <- asfr |>
  filter(ASFR > 0 ) |>
  filter(Age_Band %in% age_band_levels) |>
  mutate(Age_Band = factor(Age_Band, levels = age_band_levels)) |>
  select(Year, Age_Band, Marital_Status, ASFR) |>
  pivot_wider(names_from = c(Marital_Status, Year), values_from = ASFR) |>
  arrange(Age_Band) |>
  select(-Age_Band) |>
  as.matrix()
rownames(asfr_wide) <- age_midpoints
years_obs <- as.integer(sub(".*_(\\d+)$", "\\1", colnames(asfr_wide)))

make_fert_obj <- function(mat, yrs) {
  demogdata(
    data  = mat,
    pop   = matrix(1, nrow = nrow(mat), ncol = ncol(mat)),
    ages  = age_midpoints,
    years = yrs,
    type  = "fertility",
    label = "USA",
    name  = "ASFR"
  )
}

extract_forecast <- function(fc, marital_status, max_obs_year) {
  as.data.frame(fc$rate$asfr) |>
    as_tibble() |>
    mutate(Age_Band = age_band_levels) |>
    pivot_longer(-Age_Band, names_to = "Year", values_to = "ASFR") |>
    mutate(Year = as.integer(gsub("ASFR\\.", "", Year)),
           Marital_Status = marital_status)
}

married_cols   <- grep("^Married_",   colnames(asfr_wide))
unmarried_cols <- grep("^Unmarried_", colnames(asfr_wide))

asfr_wide_married   <- asfr_wide[, married_cols]
asfr_wide_unmarried <- asfr_wide[, unmarried_cols]

years_married   <- as.integer(sub("^Married_(\\d+)$",   "\\1", colnames(asfr_wide_married)))
years_unmarried <- as.integer(sub("^Unmarried_(\\d+)$", "\\1", colnames(asfr_wide_unmarried)))

fert_obj_married   <- make_fert_obj(asfr_wide_married,   years_married)
fert_obj_unmarried <- make_fert_obj(asfr_wide_unmarried, years_unmarried)

fit_fdm_married   <- fdm(fert_obj_married,   order = 2)
fit_fdm_unmarried <- fdm(fert_obj_unmarried, order = 2)

h <- 2100 - max(years_married)
fc_fdm_married   <- forecast(fit_fdm_married,   h = h)
fc_fdm_unmarried <- forecast(fit_fdm_unmarried, h = h)
    
FORECASTED_ASFR <- bind_rows(
  extract_forecast(fc_fdm_married,   "Married",   max(years_married)),
  extract_forecast(fc_fdm_unmarried, "Unmarried", max(years_unmarried))
)
MARRIAGE_RATE <- as_tibble(raw_population) %>%
  mutate(MAR_RATE = (F.Mar + M.Mar)/(F.Tot + M.Tot)) %>%
  select(Year, Age, MAR_RATE)
MAR_RATE_BAND <- MARRIAGE_RATE %>%
  filter(Age >= 15, Age < 50) %>%
  mutate(Age_Band = case_when(
    Age < 20 ~ "15-19 years",
    Age < 25 ~ "20-24 years",
    Age < 30 ~ "25-29 years",
    Age < 35 ~ "30-34 years",
    Age < 40 ~ "35-39 years",
    Age < 45 ~ "40-44 years",
    Age < 50 ~ "45-49 years"
  )) %>%
  group_by(Year, Age_Band) %>%
  summarise(prop_married = mean(MAR_RATE), .groups = "drop")
FINAL <- bind_rows(
  asfr |> select(Age_Band, Year, Marital_Status, ASFR),
  FORECASTED_ASFR
) %>% na.omit() %>%
  inner_join(MAR_RATE_BAND, by = c("Year", "Age_Band")) %>%
  mutate(share = ifelse(Marital_Status == "Unmarried", 1 - prop_married, prop_married),
         ASFR_per = ASFR * share)
TFR_UNWEIGHTED <- FINAL %>%
  group_by(Year, Age_Band) %>%
  summarise(ASFR_combined = sum(ASFR_per), .groups = "drop") %>%
  group_by(Year) %>%
  summarise(TFR = sum(ASFR_combined) * 5, .groups = "drop")
###
NORMALIZE_TO_ASSUMPTIONS <- function(NEW_TFR_PROJ, FINAL){
  ### Argument takes forecasted ASFR in FINAL
  ### and new external TFR projections in 
  ### NEW_TFR_PROJ, where colnames are Year and TFR_NEW
  MULTIPLIER <- TFR_UNWEIGHTED %>%
    inner_join(
      NEW_TFR_PROJ, by = 'Year'
    ) %>% 
    mutate(MULTIPLIER = ifelse(Year >= 2026, TFR_NEW/TFR, 1)) 
FINAL %>% 
  inner_join(MULTIPLIER) %>%
  mutate(ASFR = ASFR * MULTIPLIER) %>%
  select(Age_Band, Marital_Status, Year, ASFR)
}
SSA_TFR_PROJ <- read.csv("C:/Users/kritc/OneDrive/Documents/tfr_ssa.csv")
SSA_FERT_NORM <- NORMALIZE_TO_ASSUMPTIONS(SSA_TFR_PROJ %>% 
    rename(TFR_NEW = TFR), FINAL)
###
MAKE_BABIES <- function(sample, yr) {
  with_fertility <- sample %>%
    rename(Year = year) %>%
    mutate(
      Age_Band = case_when(
        age %in% 15:19 ~ "15-19 years",
        age %in% 20:24 ~ "20-24 years",
        age %in% 25:29 ~ "25-29 years",
        age %in% 30:34 ~ "30-34 years",
        age %in% 35:39 ~ "35-39 years",
        age %in% 40:44 ~ "40-44 years",
        age %in% 45:49 ~ "45-49 years",
        TRUE ~ NA_character_
      ),
      Marital_Status = case_when(marst == "married" ~ "Married", TRUE ~ "Unmarried")
    ) %>%
    left_join(
      SSA_FERT_NORM %>% filter(Year == yr),
      by = c("Marital_Status", "Year", "Age_Band")
    ) %>%
    mutate(
      PR_BABY   = runif(n()),
      PR_BABY   = ifelse(sex == 1 | !age %in% 15:44, NA, PR_BABY),
      HAVE_BABY = ifelse(PR_BABY < ASFR, 1, 0)
    )

  babies <- with_fertility %>%
    filter(HAVE_BABY == 1) %>%
    transmute(
      famunit    = famunit,
      perwt      = perwt,
      hhwt       = hhwt,
      marst      = "single",
      age        = 0L,
      school     = "no",
      sex        = sample(c(1L, 2L), n(), replace = TRUE, prob = c(0.48, 0.52)),
      educ       = "hs",
      labforce   = "no",
      employed   = "no",
      retired    = "no",
      incwage    = 0,
      receive_ss = "no",
      cohort     = (Year %/% 10L) * 10L,
      relate     = "child",
      year       = Year
    )

  bind_rows(sample, babies)
}


WITH_NEW_BABIES <- MAKE_BABIES(initial_sample, 2008)


DF_DEATH_PR <- read.csv('death_probability.csv')
DF_DEATH_PR <- as_tibble(DF_DEATH_PR) 
DEATH_2008 <- DF_DEATH_PR %>%
  filter(year == 2008) %>%
  arrange(age) %>%
  mutate(sex = if_else(sex == "M", 1, 2))
pop_base <- WITH_NEW_BABIES %>%
  group_by(age, sex) %>%
  mutate(pctile = ntile(ped, 100)) %>%
  mutate(pctile = ifelse(age <= 18, 0, pctile)) %>%
  mutate(pr_die = runif(n())) %>%
  ungroup()
pop_outside <- pop_base %>%
  filter(!between(age, 40, 76)) %>%
  left_join(DEATH_2008 %>% distinct(age, sex, q),
            by = c("age", "sex")) %>%
  rename(q_pctile = q)
pop_inside <- pop_base %>%
  filter(between(age, 40, 76)) %>%
  left_join(DEATH_2008 %>% select(age, sex, pctile, q_pctile),
            by = c("age", "sex", "pctile")) %>%
  left_join(DEATH_2008 %>% filter(pctile == 5) %>% select(age, sex, q_pctile) %>% rename(q_mid = q_pctile),
            by = c("age", "sex")) %>%
  mutate(q_pctile = coalesce(q_pctile, q_mid)) %>%
  select(-q_mid)

WITH_DEATH <- bind_rows(pop_outside, pop_inside)
WITH_DEATH <- WITH_DEATH %>%
  mutate(DIE = ifelse(pr_die < 0.8 * q_pctile, 1, 0))

dead_famunits <- WITH_DEATH %>%
  filter(DIE == 1) %>%
  pull(famunit)


WITH_DEATH %>%
  filter(DIE == 1) %>% nrow()

WITH_DEATH %>%
  mutate(
    marst      = ifelse(famunit %in% dead_famunits & relate == "spouse", "widowed", marst),
    receive_ss = ifelse(famunit %in% dead_famunits & relate == "spouse", "yes",     receive_ss)
  ) %>%
  filter(DIE == 0) %>% nrow()
initial_sample %>% nrow()

tr %>% filter(is.na(q)) %>% nrow()
tr %>% filter(year == 2008, sex == "M", age == 50) %>% pull(q)

q_full <- q_full %>%
  group_by(year, age, sex) %>%
  mutate(q_pctile = q * rho / mean(rho)) %>%
  ungroup()
DF_DEATH_PR %>%
  filter(year == 2008, sex == "M") %>%
  group_by(age) %>%
  summarise(
    q_agg   = mean(q),           # should match TR2024 directly
    q_p50   = q_pctile[pctile == 50],
    q_p1    = q_pctile[pctile == 1],
    q_p100  = q_pctile[pctile == 100],
    rho_p1  = rho[pctile == 1],
    rho_p100= rho[pctile == 100]
  ) %>%
  print(n = 40)
WITH_DEATH %>%
  group_by(age) %>%
  summarise(share = sum(perwt)) %>% 
  ungroup() %>%
  mutate(share_perc = share/sum(share)) %>% 
  print(n = 100)
