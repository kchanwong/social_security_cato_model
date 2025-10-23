`###
library(haven)
library(stringr)
library(tidyr)
library(dplyr)
library(e1071)
library(ipumsr)
library(readr)
library(purrr)
library(survey)
library(forecast)
library(DBI)
###
load("C:/Users/kchanwong/Downloads/data_population.Rdata")
load("C:/Users/kchanwong/Downloads/data_fertility.Rdata")
load("C:/Users/kchanwong/Downloads/data_mortality.Rdata")
load("C:/Users/kchanwong/Downloads/data_immigration.RData")
load("C:/Users/kchanwong/Downloads/ocact_assumptions.RData")
credits <- read_csv(
  "C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model_1/credits_coverage.csv"
) 
credits <- credits %>%
  add_row(
    credits %>% 
      filter(Year == 2025) %>%
      mutate(Year = list(2026:2100)) %>%
      unnest(Year) %>%
      inner_join(
        df_econ_assumptions %>%
          filter(ALTERNATIVE %in% c(0,2)) %>%
          mutate(AWI_GROWTH = 250 * AWI/9226.48) %>%
          select(Year = REFYEAR, AWI_GROWTH) %>%
          na.omit(),
        by = 'Year'
      ) %>%
      select(Year, Earnings  = AWI_GROWTH)
  )

MAX_INCOME <- data.frame(
  REFYEAR = 1951:1971,
  MAX_INCOME = c(
    rep(3600, 4),
    rep(4200, 4),
    rep(4800, 7),
    rep(6600, 2),
    rep(7800, 4))
) %>%
  add_row(
    read.csv("C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model/df_max_income_1993.csv") %>%
      mutate(MAX_INCOME = as.integer(MAX_INCOME))
  ) %>%
  add_row(
    df_econ_assumptions %>%
      filter(REFYEAR > 1994) %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      mutate(
        BASE = lag(AWI, 2),
        YEAR = lag(REFYEAR, 2),
        BASE_CONTRIB  = 300 * round((1/300) * ((lag(AWI, 2) * 60600)/22935.42))
      ) %>%
      select(REFYEAR, MAX_INCOME = BASE_CONTRIB)
  ) %>%
  na.omit()
###
save.image("C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/baseline_data_prep.RData")`