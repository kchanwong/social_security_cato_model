### Estimate Thirty Five Year Earnings Equation ###
library(ipumsr)
library(dplyr)
library(fixest)
library(fredr)
setwd("C:/Users/kchanwong/Documents/TEST")
# Authenticate — set your API key once per session
set_ipums_api_key("", save = TRUE)
fredr_set_key("")
# -------------------------------------------------------------------
# Define the extract
# 35 years of March CPS ASEC: income years 1975–2009
# (survey years 1976–2010 in IPUMS CPS nomenclature)
# -------------------------------------------------------------------
help(define_extract_micro)
cps_extract <- define_extract_micro(
  description = "CBOLT earnings equation replication: 35yr CPS ASEC",
  collection = 'cps',
  samples = paste0("cps", 1976:2010, "_03s"),   # March ASEC, survey years 1976-2010
  
  variables = c(
    # ── Identifiers & weights ──────────────────────────────────────
    "YEAR",       # Survey year
    "SERIAL",     # Household serial number
    "PERNUM",     # Person number within household
    "ASECWT",     # Person-level ASEC weight
    
    # ── Earnings (to construct FTE) ────────────────────────────────
    "INCWAGE",    # Wage and salary income (prior year)
    "WKSWORK1",   # Weeks worked last year (continuous)
    
    # ── Demographics ───────────────────────────────────────────────
    "AGE",        # Age at survey date
    "SEX",        # Sex
    
    # ── Education ─────────────────────────────────────────────────
    "EDUC",      # Detailed education (maps to 4 groups: <HS, HS, some college, BA+)
    
    # ── Marital status ─────────────────────────────────────────────
    "MARST",      # Marital status (married/never married/other)
    
    # ── Children under 6 (women's equation) ───────────────────────
    "NCHILD",     # Own children under age 5 in HH (closest available to <6)
    # ── Social Security beneficiary status ────────────────────────
    "INCSS",      # Social Security income (>0 flags SS beneficiary)
    
    # ── Labor force / class of worker (sample restriction) ────────
    "CLASSWKR",   # Class of worker (wage/salary vs. self-employed)
    "EMPSTAT"     # Employment status
  )
)
submitted <- submit_extract(cps_extract)
downloadable <- wait_for_extract(submitted)
files <- download_extract(downloadable, download_dir = "C:/Users/kchanwong/Documents/TEST")
raw <- read_ipums_micro(files)
### Inflaton Adjustment ###
MULTIPLIER_ADJUSTING <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1947-01-01"),
  observation_end = as.Date("2011-01-01")) %>% 
  mutate(YEAR = as.integer(format(date, "%Y"))) %>%
  mutate(MONTH = as.integer(format(date, "%m"))) %>% 
  filter(MONTH == 3) %>% 
  mutate(MULTIPLIER_2008 = value/value[YEAR == 2008]) %>% 
  select(YEAR, MULTIPLIER_2008)
###
EDUC_EDIT <- raw %>% mutate(EDUC_GROUPS = case_when(
  EDUC %in% c(000:073)        ~ "hs",   # NIU through 12th no diploma
  EDUC %in% c(080:092)        ~ "some_college",    # 1yr college through Associate's
  EDUC %in% c(100:125)        ~ "ba_plus",         # Bachelor's and above
  .default = NA_character_                          # 999 Missing/Unknown
)
) %>% 
    mutate(YEAR_BORN = YEAR - AGE) %>%
    mutate(COHORT    = (YEAR_BORN %/% 10) * 10)  %>%
    mutate(RECEIVE_SS = ifelse(INCSS == 999999 | INCSS < 0, "Yes", "No")) %>%
    mutate(INCWAGE = ifelse(INCWAGE == 99999999 | INCWAGE == 99999998, 0, INCWAGE)) %>%
    inner_join(MULTIPLIER_ADJUSTING, by = 'YEAR') %>%
    mutate(REAL_INCWAGE = INCWAGE * MULTIPLIER_2008) %>%
    filter(REAL_INCWAGE > 4200)
wage_model <- feols(
  REAL_INCWAGE ~ AGE + COHORT| SEX + EDUC_GROUPS + RECEIVE_SS,
  data = EDUC_EDIT
)
saveRDS(wage_model, "wage_model.rds")

