#
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
##
load("C:/Users/kchanwong/Downloads/data_population.Rdata")
load("C:/Users/kchanwong/Downloads/data_fertility.Rdata")
load("C:/Users/kchanwong/Downloads/data_mortality.Rdata")
load("C:/Users/kchanwong/Downloads/data_immigration.RData")
load("C:/Users/kchanwong/Downloads/ocact_assumptions.RData")
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
income_dist <- read.csv("C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model_1/income_distribution_2022.csv")
LFPR <- read.csv(
  "C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model_1/labor_force_participation_rate.csv") %>% 
  pivot_longer(
    -c(Year, Sex),          # everything except these becomes “long”
    names_to  = "AgeRange",
    values_to = "Value"
  ) %>% 
  mutate(AgeRange = str_remove(AgeRange, "^X") %>% str_replace_all("\\.", "-")) %>%
  mutate(AgeRange = case_when(
    AgeRange == '16-17' ~ list(16:17),
    AgeRange == '18-19' ~ list(18:19),
    AgeRange == '20-24' ~ list(20:24),
    AgeRange == '25-29' ~ list(25:29),
    AgeRange == '30-34' ~ list(30:34),
    AgeRange == '35-39' ~ list(35:39),
    AgeRange == '40-44' ~ list(40:44),
    AgeRange == '45-49' ~ list(45:49),
    AgeRange == '50-54' ~ list(50:54),
    AgeRange == '55-59' ~ list(55:59),
    AgeRange == '60-64' ~ list(60:64),
    AgeRange == '65-69' ~ list(65:69),
    AgeRange == '70' ~ list(70:100)
  )) %>%
  unnest(AgeRange)
LFPR <- LFPR %>%
  add_row(
    LFPR %>%
      filter(Year == 2096) %>%
      mutate(Year = list(2097:2100)) %>%
      unnest(Year)
  )
dfFert06_08 <-
  read.csv("C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model/child_birth_w_order.csv") %>%
  rename(YEAR = year,
         AGE = age,
         FERT_RATE = Total) %>%
  mutate(FERT_PER_CAPITA = FERT_RATE/1000) %>%
  select(YEAR, AGE, FERT_PER_CAPITA)
dfFert06_08 <- dfFert06_08 %>% add_row(dfFert06_08 %>%
                                         inner_join(df_pop_ssa %>%
                                                      rename(YEAR = year, AGE = age) %>%
                                                      select(YEAR, AGE, f_tot),
                                                    by = c('YEAR', 'AGE')) %>%
                                         mutate(NO_KIDS = FERT_PER_CAPITA * f_tot) %>%
                                         group_by(YEAR) %>%
                                         mutate(PERC_KIDS = FERT_PER_CAPITA / sum(FERT_PER_CAPITA)) %>%
                                         filter(YEAR == 2009) %>% mutate(YEAR = list(2010:2100)) %>% unnest(YEAR) %>% arrange(YEAR) %>%
                                         inner_join(df_pop_ssa %>% filter(age == 0) %>% select(YEAR = year, TOTAL_BABIES = total)) %>%
                                         mutate(NO_BABIES = TOTAL_BABIES * PERC_KIDS) %>%
                                         inner_join(df_pop_ssa %>% select(YEAR = year, AGE = age, TOTAL_PEOPLE = f_tot),
                                                    by = c('YEAR', 'AGE')) %>% mutate(FERT_PER_CAPITA = NO_BABIES/TOTAL_PEOPLE) %>%
                                         select(YEAR, AGE, FERT_PER_CAPITA))
dfInitSamps <- readRDS("C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model_1/projected_2007_1.rds") %>%
  mutate(SEX = as.integer(as_factor(SEX))) %>%
  mutate(SERIAL = str_pad(as.character(SERIAL), width = 7, pad = '0')) %>%
  # mutate(INCWAGE = ifelse(INCWAGE < 22500 & LABFORCE == 1, 22500, INCWAGE)) %>%
  mutate(LABFORCE = ifelse(INCWAGE != 0, 1, 0))
cohort_life_tables <- cohort_life_tables %>%
  mutate(YEAR_LIVING = year + age)
#load("C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model_1/sample_microsims_data.RData")
transition <- read_csv(
  "C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/jg_model_1/transition_matrix.csv"
)
transition <- transition %>%
  add_row(
    transition %>% filter(YEAR == 2024) %>%
      mutate(YEAR = list(2025:2100)) %>%
      unnest(YEAR)
  )
transition <- transition  %>%
  select(YEAR, SEX, AGE, PROB_EXIT_LF = NILF_TO_NILF) %>%
  mutate(LABFORCE = 0, END = 0) %>%
  ungroup() %>%
  add_row(
    transition %>%
      select(YEAR, SEX, AGE, PROB_EXIT_LF = ILF_TO_NILF ) %>%
      mutate(LABFORCE = 1, END = 0)
  ) %>%
  arrange(YEAR, AGE, LABFORCE)
transition <- transition %>%
  mutate(PROB_EXIT_LF =
           ifelse(LABFORCE == 0 & END == 0 & AGE > 70, 1, PROB_EXIT_LF)) %>%
  mutate(PROB_EXIT_LF = ifelse(LABFORCE == 1 & END == 0 & AGE > 70, 0.5, PROB_EXIT_LF))
### EXPORT AS RDS ###
save.image("C:/Users/kchanwong/OneDrive - Cato Institute/Documents/social_security/init_sim_data.RData")