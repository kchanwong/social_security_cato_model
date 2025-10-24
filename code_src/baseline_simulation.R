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
library(DBI)

load("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/data_population.Rdata")
load("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/data_fertility.Rdata")
load("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/data_mortality.Rdata")
load("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/data_immigration.RData")
load("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/ocact_assumptions.RData")

credits <- read_csv("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/credits_coverage.csv") 

# Add future years using 2025 data and economic assumptions  
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
      select(Year, Earnings = AWI_GROWTH)
  )

dfSamps <- readRDS("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/initial_sim.rds") %>%
  mutate(INCWAGE = INCWAGE)

# Create base data frame manually for 1951-1971
MAX_INCOME <- data.frame(
  REFYEAR = 1951:1971,
  MAX_INCOME = c(
    rep(3600, 4),
    rep(4200, 4),
    rep(4800, 7),
    rep(6600, 2),
    rep(7800, 4))
  ) %>%
  
  # Add rows for 1972-1993 from csv
  add_row(read.csv("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/df_max_income_1993.csv") %>%
            mutate(MAX_INCOME = as.integer(MAX_INCOME))) %>%
  
  # Add rows for >1994
  add_row(
    
    # MAX_INCOME for 1994 onwards
    df_econ_assumptions %>%
      filter(REFYEAR > 1994,
             ALTERNATIVE %in% c(0,2)) %>%
      
      # Uses AWI value from 2 years prior
      mutate(BASE = lag(AWI, 2),
             YEAR = lag(REFYEAR, 2),
             
             # Computing adjusted max taxable income
             BASE_CONTRIB = 300 * round((1/300) * (lag(AWI, 2) * 60600) / 22935.42)) %>%
      
      select(REFYEAR, MAX_INCOME = BASE_CONTRIB)
  ) %>%
  na.omit()

# Keeps only non-null, unique IDs
SSA_ID <- dfSamps %>%
  distinct(SSA_ID) %>%
  na.omit() %>%
  pull()

# Combine SSA_ID into comma-separated string
vector_string <- paste(SSA_ID, 
                       collapse = ',')
query <- paste("select * from social_security_research.puf_earnings_2006 where ID in", 
               paste('(', vector_string, ')', 
                     sep = ''), 
               sep = ' ')

# DELETE BEFORE UPLOADING !!!!!!!!!!!!!!!!!!!
db <- dbConnect()

earnings <- dbGetQuery(db, query) %>%
  rename(SSA_ID = ID) %>%
  mutate(SSA_ID = as.integer(SSA_ID))

# Data frame with earnings records for those with valid SSA_ID and original sample records
dfSamps_joined <- dfSamps %>%
  mutate(INCWAGE = INCWAGE) %>%
  ungroup() %>%
  mutate(SSA_ID = as.integer(SSA_ID)) %>%
  filter(!is.na(SSA_ID)) %>%
  distinct(ID, SSA_ID) %>%
  
  # Earnings data merged where SSA_ID matches
  left_join(earnings,
            by = 'SSA_ID',
            relationship = 'many-to-many') %>%
  
  mutate(ANNUAL_EARNINGS = as.double(ANNUAL_EARNINGS),
         YEAR_EARN = as.double(YEAR_EARN)) %>%
  
  select(-SSA_ID, -ANNUAL_QTRS) %>%
  add_row(dfSamps %>%
            mutate(INCWAGE = INCWAGE) %>%
            select(ID,
                   YEAR_EARN = YEAR,
                   ANNUAL_EARNINGS = INCWAGE)) 

# Joins birth year data, retirement data, add Full Retirement Age 
onlyRetired <- dfSamps_joined %>%
  
  # Dataset with one YEAR_BORN per individual
  inner_join(
    dfSamps %>%
      ungroup() %>%
      mutate(YEAR_BORN = YEAR - AGE) %>%
      distinct(ID, YEAR_BORN) %>%
      group_by(ID) %>%
      mutate(COUNT = n()) %>%
      filter(COUNT == 1),
    by = 'ID'
  ) %>%
  mutate(AGE = YEAR_EARN - YEAR_BORN) %>%
  
  # Dataset of each person's first year of retirement 
  inner_join(
    dfSamps %>%
      filter(RETIRED == 1) %>%
      group_by(ID) %>%
      summarise(RETIRE_YEAR = min(YEAR)) %>%
      arrange(RETIRE_YEAR),
    by = 'ID'
  ) %>% mutate(
    FRA =
      case_when(
        YEAR_BORN <= 1942 ~ 65,
        YEAR_BORN %in% c(1943:1960) ~ 66,
        TRUE ~ 67
      )
  )

onlyRetired <- onlyRetired %>%
  mutate(ANNUAL_EARNINGS = ANNUAL_EARNINGS)

# Building AWI dataset by combining historical data with projected
AWI <- rbind(
  data.frame(
    YEAR_EARN = 1951:1969,
    AWI = c(2799.16, 2973.32,
            3139.44, 3155.64,
            3301.44, 3532.26,
            3641.72, 3673.80,
            3855.80, 4007.12,
            4086.76, 4291.40,
            4396.64, 4576.32,
            4658.72, 4938.36,
            5213.44, 5571.76,
            5893.76)),
  df_econ_assumptions %>%
    filter(ALTERNATIVE %in% c(0,2)) %>%
    select(YEAR_EARN = REFYEAR, AWI) %>%
    na.omit()
)

# Cost of Living Adjustment dataset merging historical and future values
cola <- read.csv("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/cola_75_23.csv") %>%
  rename(YEAR = Year) %>%
  mutate(COLA = 1 + COLA / 100) %>%
  add_row(
    data.frame(
      YEAR = 2025:2100,
      COLA = 1.026
    )
  )

# Convert monthly bend points to annual
family_bend_points <- read.csv(
  "C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/family_bend_points.csv") %>%
  mutate(across(contains('bp'), ~ . * 12)) 

family_bend_points <- family_bend_points %>%
  rename(
    first_bp_fam = first_bp,
    second_bp_fam = second_bp,
    third_bp_fam = third_bp,
    YEAR = year
  ) %>%
  
  # Create new years 2026-2100 from base year; join wage growth assumptions
  add_row(
    family_bend_points %>%
      filter(year == 2025) %>%
      mutate(year = list(2026:2100)) %>%
      unnest(year) %>%
      rename(YEAR = year) %>%
      
      # Compute cumulative nominal wage growth after 2025
      inner_join(
        df_econ_assumptions %>%
          filter(ALTERNATIVE %in% c(0,2)) %>%
          select(REFYEAR, GROWTH_WAGE_NOMINAL) %>%
          na.omit() %>%
          mutate(PERC_GROWTH = 1 + (GROWTH_WAGE_NOMINAL/100)) %>%
          filter(REFYEAR >= 2025) %>%
          mutate(PERC_GROWTH = ifelse(REFYEAR == 2025, 1, PERC_GROWTH)) %>%
          mutate(PERC_CUM_GROWTH = cumprod(PERC_GROWTH)) %>%
          select(REFYEAR, PERC_CUM_GROWTH) %>%
          filter(REFYEAR > 2025) %>%
          rename(YEAR = REFYEAR)
      ) %>%
      mutate(across(contains('_bp'), ~ round(. * PERC_CUM_GROWTH))) %>%
      rename(
        first_bp_fam = first_bp,
        second_bp_fam = second_bp,
        third_bp_fam = third_bp
      ) %>%
      select(-contains('GROWTH'))
  )

# Read in past SS bend points and project for future years using AWI
dfBendPoint2025 <- read.csv("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/bend_point.csv") %>%
  rename(YEAR = year) %>%
  mutate(first_bp = first_bp * 12,
         second_bp = second_bp * 12) %>%
  filter(YEAR <= 2021) %>%
  select(-X) %>%
  add_row(
    data.frame(
      YEAR = 2020:2100,
      first_bp = 2160,
      second_bp = 13020
    ) %>%
      left_join(
        
        # Extract AWI from dataset of economic assumptions
        df_econ_assumptions %>%
          filter(ALTERNATIVE %in% c(0, 2)) %>%
          select(YEAR = REFYEAR, AWI) %>%
          na.omit()
      ) %>%
      
      # 9779.44 is AWI for 1977, base year from SSA formula
      mutate(AWI_ADJUST = lag(AWI, 2)/9779.44) %>%
      
      # Apply adjustment to bend points
      mutate(first_bp = 10 * round((1/10) * first_bp * AWI_ADJUST),
             second_bp = 10 * round((1/10) * second_bp * AWI_ADJUST)) %>%
      select(YEAR, first_bp, second_bp) %>%
      filter(YEAR >= 2022)
    )

# Calculate Primary Insurance Amounts
# Combine individual simulation data with economic assumptions
PIA <-  onlyRetired %>%
  mutate(BIRTH_YEAR = YEAR_BORN) %>%
  
  # Earnings required to earn one Social Security credit
  inner_join(credits %>%
               mutate(CREDIT = Earnings) %>%
               mutate(CREDIT = 10 * round((1/10) * CREDIT)) %>%
               rename(YEAR_EARN = Year) %>%
               select(YEAR_EARN, CREDIT)) %>%
  mutate(CREDIT_EARNED = pmin(round(ANNUAL_EARNINGS/CREDIT), 4)) %>%
  mutate(
    AGE_AT_2007 = 2007 - YEAR_BORN
  ) %>%
  group_by(ID) %>%
  mutate(CREDITS = sum(CREDIT_EARNED)) %>%
  mutate(AGE_RETIRE = RETIRE_YEAR - YEAR_BORN) %>%
  mutate(MIN_RA = 62,
         MAX_RA = 70) %>%
  filter(AGE >= 18) %>%
  left_join(
    AWI,
    by = c('YEAR_EARN')
  ) %>%
  mutate(INDEX_START = YEAR_BORN + 60) %>%
  left_join(
    AWI %>%
      rename(INDEX_START = YEAR_EARN, INDEX_AWI_START = AWI),
    by = c('INDEX_START')
  ) %>%
  inner_join(
    MAX_INCOME %>%
      select(YEAR_EARN = REFYEAR, MAX_INCOME)
  ) %>%
  
  # Only workers with at least 40 credits
  filter(CREDITS  >= 40) %>%
  
  # Cap earnings at max taxable income for that year
  mutate(ANNUAL_EARNINGS = ifelse(ANNUAL_EARNINGS > MAX_INCOME, MAX_INCOME, ANNUAL_EARNINGS)) %>%
  select(-MAX_INCOME) %>%
  
  # Compute indexing factor to adjust year earnings using AWI
  mutate(INDEX_FACTOR = INDEX_AWI_START/AWI) %>%
  mutate(INDEX_FACTOR = ifelse(AGE >= 60, 1, INDEX_FACTOR)) %>%
  mutate(BEND_POINT_YEAR = YEAR_BORN  + 62) %>%
  select(-AWI, -INDEX_START, -INDEX_AWI_START) %>%
  
  # Inflation-adjusted earnings for each working year
  mutate(INDEXED_EARNINGS = ANNUAL_EARNINGS * INDEX_FACTOR) %>%
  select(-AGE_AT_2007, -MIN_RA, -MAX_RA) %>%
  
  # Set post-retirement earnings to 0
  mutate(ANNUAL_EARNINGS = ifelse(AGE >= 70 | RETIRE_YEAR <= YEAR_EARN, 0, ANNUAL_EARNINGS)) %>%
  filter(YEAR_EARN < RETIRE_YEAR) %>%
  arrange(ID, -INDEXED_EARNINGS) %>%
  group_by(ID, BIRTH_YEAR, RETIRE_YEAR, BEND_POINT_YEAR, AGE_RETIRE, FRA) %>%
  slice_head(n = 35) %>%
  summarise(AIME = mean(INDEXED_EARNINGS),
            CREDITS_EARNED = sum(CREDIT_EARNED)) %>%
  inner_join(dfBendPoint2025 %>%
               select(BEND_POINT_YEAR = YEAR,
                      first_bp, second_bp)) %>%
  arrange(RETIRE_YEAR) %>%
  mutate(PIA = case_when(
    AIME  < first_bp ~ 0.9 * AIME ,
    AIME  < second_bp ~ 0.9 * first_bp + 0.32 * (AIME  - first_bp),
    AIME  > second_bp ~ 0.9 * first_bp + 0.32 * (second_bp - first_bp) + 0.15 * (AIME  - second_bp)
  )
  ) %>%
  inner_join(
    dfSamps %>%
      filter(RETIRED == 1) %>%
      group_by(ID) %>%
      summarise(DEATH_YEAR = max(YEAR))
  ) %>%
  mutate(LIFE_SPAN = DEATH_YEAR - BIRTH_YEAR) %>%
  mutate(YEARS_NOT_FRA = 12 * (AGE_RETIRE - FRA)) %>%
  mutate(CREDITS =
           case_when(
             YEARS_NOT_FRA == 0 ~ 1,
             -36 <= YEARS_NOT_FRA & YEARS_NOT_FRA <= 0 ~ YEARS_NOT_FRA * (5/9)*(0.01),
             -60 <= YEARS_NOT_FRA & YEARS_NOT_FRA < -36 ~ ((36) * (5/9)*(-0.01) - (YEARS_NOT_FRA+36) * (5/12)*(-0.01)),
             YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% c(1917:1924) ~ (1/12) * YEARS_NOT_FRA/12 * (0.03),
             YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% seq(1925, 1941, by = 2) ~ YEARS_NOT_FRA/12 * (0.03 + 0.0025 * ((BIRTH_YEAR + 1)-1924)),
             YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% seq(1926, 1942, by = 2)  ~ YEARS_NOT_FRA/12 * (0.03 + 0.0025 * ((BIRTH_YEAR)-1924)),
             TRUE ~ YEARS_NOT_FRA/12 * 0.08
           )
  ) %>%
  mutate(PIA_ADJ =
           ifelse(
             CREDITS == 1,
             PIA,
             PIA * (1+CREDITS )
           ))


# ------------------------------------------------------

PIA_BY_YEAR <- PIA %>%
  mutate(YEAR = list(seq(RETIRE_YEAR, DEATH_YEAR))) %>%
  unnest(YEAR) %>%
  ungroup() %>%
  select(ID, PIA = PIA_ADJ, AIME, RETIRE_YEAR, BIRTH_YEAR, YEAR) %>%
  mutate(AGE = YEAR - BIRTH_YEAR) %>%
  select(-BIRTH_YEAR) %>%
  inner_join(cola) %>%
  mutate(COLA = ifelse(RETIRE_YEAR == YEAR, 1, COLA)) %>%
  group_by(ID) %>%
  mutate(GROWTH_FACTOR = cumprod(COLA)) %>%
  mutate(PIA_COLA = PIA * GROWTH_FACTOR) %>%
  select(ID, YEAR, AGE, PIA_COLA, GROWTH_FACTOR) %>%
  filter(YEAR >= 2007) %>%
  inner_join(
    dfSamps %>% select(ID, YEAR, WEIGHTS), by = c('ID', 'YEAR')
  )
PIA_W_DEMOS <- PIA %>%
  mutate(YEAR = list(seq(RETIRE_YEAR, DEATH_YEAR))) %>%
  unnest(YEAR) %>%
  ungroup() %>%
  select(ID, PIA = PIA_ADJ, AIME, RETIRE_YEAR, BIRTH_YEAR, DEATH_YEAR, YEAR) %>%
  mutate(AGE = YEAR - BIRTH_YEAR) %>%
  select(-BIRTH_YEAR) %>%
  inner_join(cola) %>%
  mutate(COLA = ifelse(RETIRE_YEAR == YEAR, 1, COLA)) %>%
  group_by(ID) %>%
  mutate(GROWTH_FACTOR = cumprod(COLA)) %>%
  mutate(PIA_COLA = PIA * GROWTH_FACTOR) %>%
  inner_join(
    dfSamps %>% select(ID, YEAR, WEIGHTS, ID_SP, WIDOWED), by = c('ID', 'YEAR')
  ) %>%
  select(ID, ID_SP, YEAR, AGE, PIA_COLA, WEIGHTS, WIDOWED)
fam <- PIA %>%
  mutate(
    SPOUSE = 0.5 * PIA,
    WIDOWER = PIA,
    CHILD = 0.625 * PIA) %>%
  ungroup() %>%
  inner_join(
    family_bend_points %>%
      rename(BEND_POINT_YEAR = YEAR),
    by = 'BEND_POINT_YEAR'
  ) %>%
  mutate(
    MAX_FAMILY_PIA =
      case_when(
        PIA <= first_bp_fam ~ PIA * 1.5,
        PIA > first_bp_fam & PIA <= second_bp_fam ~ 1.5 * (first_bp_fam) + 2.72 * (PIA -  second_bp_fam),
        PIA > second_bp_fam & PIA <= third_bp_fam ~ 1.5 * (first_bp_fam) + 2.72 * (second_bp_fam - first_bp_fam) +
          1.34 * (PIA - second_bp_fam),
        TRUE ~ 1.5 * (first_bp_fam) + 2.72 * (second_bp_fam - first_bp_fam) +
          1.34 * (third_bp_fam - second_bp_fam) + 1.75 * (PIA - third_bp_fam)
      )
  ) %>%
  select(-contains('bp_fam')) %>%
  inner_join(
    dfSamps %>%
      filter(!is.na(ID_SP)) %>%
      group_by(ID, ID_SP) %>%
      summarise(DEATH_YEAR_SP = max(YEAR)) %>%
      filter(DEATH_YEAR_SP != 2100),
    by = c('ID')
  )
auxiliary_benefits <- fam %>%
  mutate(YEAR = map2(RETIRE_YEAR, DEATH_YEAR_SP, seq)) %>%
  select(ID, ID_SP, RETIRE_YEAR, DEATH_YEAR, DEATH_YEAR_SP, YEAR, SPOUSE, WIDOWER, CHILD, MAX_FAMILY_PIA) %>%
  unnest(YEAR) %>%
  inner_join(
    cola,
    by = 'YEAR'
  ) %>%
  filter(DEATH_YEAR_SP >= DEATH_YEAR) %>%
  mutate(COLA = ifelse(DEATH_YEAR == YEAR, 1, COLA)) %>%
  group_by(ID) %>%
  mutate(COLA = cumprod(COLA)) %>%
  mutate(
    SPOUSE = SPOUSE * COLA,
    WIDOWER = WIDOWER * COLA,
    CHILD = CHILD * COLA,
    MAX_FAMILY_PIA = MAX_FAMILY_PIA * COLA
  )
auxiliary_benefits <- fam %>%
  mutate(YEAR = map2(RETIRE_YEAR, DEATH_YEAR_SP, seq)) %>%
  select(ID, ID_SP, RETIRE_YEAR,
         DEATH_YEAR, DEATH_YEAR_SP, YEAR,
         SPOUSE, WIDOWER, CHILD,
         MAX_FAMILY_PIA) %>%
  unnest(YEAR) %>%
  inner_join(
    cola,
    by = 'YEAR'
  ) %>%
  filter(DEATH_YEAR_SP >= DEATH_YEAR) %>%
  mutate(COLA = ifelse(DEATH_YEAR == YEAR, 1, COLA)) %>%
  group_by(ID) %>%
  mutate(COLA = cumprod(COLA)) %>%
  mutate(
    SPOUSE = SPOUSE * COLA,
    WIDOWER = WIDOWER * COLA,
    CHILD = CHILD * COLA,
    MAX_FAMILY_PIA = MAX_FAMILY_PIA * COLA
  ) %>%
  left_join(
    dfSamps %>%
      group_by(SERIAL, YEAR) %>%
      summarise(NCHILD = sum(RELATE == 301)) %>%
      inner_join(
        dfSamps %>% distinct(SERIAL, YEAR, ID, ID_SP),
        by = c('SERIAL', 'YEAR')
      ),
    by = c('YEAR', 'ID', 'ID_SP')
  ) %>%
  select(-SERIAL) %>%
  mutate(NCHILD = ifelse(is.na(NCHILD), 0, NCHILD)) %>%
  ungroup()
WITH_AUX <- PIA_W_DEMOS %>%
  left_join(auxiliary_benefits %>%
              select(ID, YEAR, SPOUSE, WIDOWER, CHILD, NCHILD, MAX_FAMILY_PIA),
            by = c('ID', 'YEAR')) %>%
  mutate(across(c('WIDOWED', 'SPOUSE', 'WIDOWER', 'CHILD', 'NCHILD', 'MAX_FAMILY_PIA'), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(
    WIDOWED_BENEFITS = WIDOWER * WIDOWED,
    SPOUSE_BENEFITS = SPOUSE,
    CHILD_BENEFITS = CHILD * NCHILD
  ) %>%
  mutate(across(contains("BENEFITS"), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(SPOUSE_BENEFITS = ifelse(WIDOWED_BENEFITS > 0, 0, SPOUSE_BENEFITS)) %>%
  mutate(TOTAL_BENEFITS = WIDOWED_BENEFITS + SPOUSE_BENEFITS + CHILD_BENEFITS) %>%
  mutate(across(c('WIDOWED_BENEFITS', 'SPOUSE_BENEFITS', 'CHILD_BENEFITS'),
                ~ ifelse(TOTAL_BENEFITS > MAX_FAMILY_PIA,
                         MAX_FAMILY_PIA * . / TOTAL_BENEFITS,
                         .))) %>%
  mutate(TOTAL_BENEFITS =
           ifelse(WIDOWED == 1, WIDOWED_BENEFITS + CHILD_BENEFITS,
                  SPOUSE_BENEFITS + CHILD_BENEFITS)) %>%
  mutate(TOTAL_BENEFITS = ifelse(is.na(TOTAL_BENEFITS), 0, TOTAL_BENEFITS)) %>%
  mutate(PIA_COLA = ifelse(PIA_COLA > TOTAL_BENEFITS,
                           PIA_COLA,
                           TOTAL_BENEFITS))
#### CALCULATE TOTAL OUTLAYS
OLD_AGE_OUTLAYS <- PIA_W_DEMOS %>%
  filter(AGE < 100) %>%
  group_by(YEAR) %>%
  summarise(
    TOTAL_PIA = sum(PIA_COLA),
    TOTAL = sum(PIA_COLA),
  ) %>%
  inner_join(
    dfSamps %>%
      group_by(YEAR) %>%
      summarise(N = n()) %>%
      inner_join(
        df_pop_ssa %>%
          group_by(year) %>%
          summarise(TOTAL = sum(total)) %>%
          select(YEAR = year, TOTAL)
      ) %>%
      mutate(FACTOR = TOTAL/N) %>%
      select(YEAR, FACTOR),
    by = c('YEAR')
  ) %>%
  mutate(OUTLAYS_OLD_AGE = (1/1e9) * (TOTAL_PIA * FACTOR),
         OUTLAYS_OLD_AGE_NW = (1/1e9) * (TOTAL * FACTOR))
### Spouse Outlays
SPOUSE_OUTLAYS <- dfSamps_joined %>%
  inner_join(
    dfSamps %>%
      filter(RELATE == 201) %>%
      select(YEAR_EARN = YEAR, ID, ID_SP, WEIGHTS),
    by = c('YEAR_EARN', 'ID')
  ) %>%
  inner_join(PIA %>%
               select(ID, RETIRE_YEAR, DEATH_YEAR)) %>%
  inner_join(
    PIA %>%
      ungroup() %>%
      select(
        ID_SP = ID, PIA, RETIRE_YEAR_SP = RETIRE_YEAR
      ),
    by = 'ID_SP'
  ) %>%
  filter(YEAR_EARN >= RETIRE_YEAR_SP & YEAR_EARN < RETIRE_YEAR) %>%
  arrange(ID) %>%
  inner_join(
    dfSamps %>%
      group_by(ID) %>%
      summarise(YEAR_BORN = min(YEAR-AGE)),
    by = 'ID'
  ) %>%
  mutate(AGE = YEAR_EARN - YEAR_BORN) %>%
  left_join(
    dfSamps %>%
      group_by(SERIAL, YEAR) %>%
      summarise(NCHILD = sum(RELATE == 301)) %>%
      inner_join(
        dfSamps %>% distinct(SERIAL, YEAR, ID, ID_SP),
        by = c('SERIAL', 'YEAR')
      ) %>% rename(YEAR_EARN = YEAR),
    by = c('ID', 'YEAR_EARN')
  ) %>%
  filter(AGE >= 62 | NCHILD > 0) %>%
  inner_join(
    cola %>% rename(YEAR_EARN = YEAR),
    by = 'YEAR_EARN'
  ) %>%
  group_by(ID) %>%
  mutate(COLA = ifelse(YEAR_EARN == min(YEAR_EARN), 1, COLA)) %>%
  mutate(GROWTH_PIA = cumprod(COLA)) %>%
  mutate(PIA_COLA_SPOUSE = 0.5 * PIA * GROWTH_PIA,
         PIA_COLA_CHILD = 0.75 * NCHILD * PIA * GROWTH_PIA) %>%
  select(ID, YEAR = YEAR_EARN,
         AGE, contains('PIA_COLA'), WEIGHTS) %>%
  group_by(YEAR) %>%
  summarise(PIA_COLA = sum((PIA_COLA_CHILD + PIA_COLA_SPOUSE))) %>%
  inner_join(
    df_pop_ssa %>%
      group_by(year) %>%
      summarise(TOTAL = sum(total)) %>%
      rename(YEAR = year)
  ) %>%
  inner_join(
    dfSamps %>%
      group_by(YEAR) %>%
      summarise(N = n()),
    by = 'YEAR'
  ) %>%
  mutate(AUX_BENEFITS = (1/1e9) * PIA_COLA * (TOTAL/N)) %>%
  head(-1) %>%
  select(YEAR, AUX_BENEFITS)
### 
## Combined Outlays
OLD_AGE_OUTLAYS %>%
  left_join(SPOUSE_OUTLAYS, by = 'YEAR') %>%
  select(YEAR, OUTLAYS_OLD_AGE, AUX_BENEFITS) %>%
  mutate(AUX_BENEFITS = ifelse(is.na(AUX_BENEFITS), 0, AUX_BENEFITS)) %>%
  mutate(TOTAL_OUTLAYS = OUTLAYS_OLD_AGE + AUX_BENEFITS) %>%
  write.csv(
    "C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/baseline.csv"
  )
BASELINE <- OLD_AGE_OUTLAYS %>%
  left_join(SPOUSE_OUTLAYS, by = 'YEAR') %>%
  select(YEAR, OUTLAYS_OLD_AGE, AUX_BENEFITS) %>%
  mutate(AUX_BENEFITS = ifelse(is.na(AUX_BENEFITS), 0, AUX_BENEFITS)) %>%
  mutate(TOTAL_OUTLAYS = OUTLAYS_OLD_AGE + AUX_BENEFITS) %>%
  inner_join(
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(YEAR = REFYEAR, NGDP) %>%
      na.omit(),
    by = 'YEAR'
  ) %>%
  inner_join(
    dfSamps %>%
      inner_join(MAX_INCOME %>% rename(YEAR = REFYEAR)) %>%
      mutate(INCWAGE = ifelse(INCWAGE > MAX_INCOME, MAX_INCOME, INCWAGE)) %>%
      group_by(YEAR) %>%
      summarise(INCWAGE = sum(INCWAGE),
                N = n()) %>%
      inner_join(
        df_pop_ssa %>%
          rename(YEAR = year) %>%
          group_by(YEAR) %>%
          summarise(TOTAL = sum(total))
      ) %>%
      mutate(TAXABLE_PAYROLL = (1/1e9) * INCWAGE * (TOTAL/N)) %>%
      select(YEAR, TAXABLE_PAYROLL)
  ) %>%
  mutate(PERC_TAXABLE = (TOTAL_OUTLAYS + AUX_BENEFITS)/TAXABLE_PAYROLL,
         PERC_GDP = (TOTAL_OUTLAYS + AUX_BENEFITS)/NGDP) %>%
  print(n = 1000)
dfECON <- 
  read.csv("C:/Users/UmanaAhmed/OneDrive - Cato Institute/Social Security Files/OASI_DI_PROJ_2023.csv")
BASELINE <- BASELINE %>%
  select(YEAR, contains('PERC')) %>%
  mutate(PERC_TAXABLE = 100 * PERC_TAXABLE) %>%
  print(n = 100) %>%
  inner_join(dfECON %>%
               filter(ALTERNATIVE %in% c(0,2)) %>%
               select(YEAR, OASI_COST_RATE)) %>%
  mutate(DIFF_SSA = OASI_COST_RATE - PERC_TAXABLE) %>%
  print(n = 100)
BASELINE %>%
  mutate(MEAN = mean(abs(DIFF_SSA))) %>%
  print(n = 100)
plot(BASELINE$YEAR, BASELINE$PERC_TAXABLE, type = 'l', lwd = 3, lty = 1,
     ylab = 'Percent',
     xlab = 'Year',
     ylim = c(0, 20))
lines(BASELINE$YEAR, BASELINE$OASI_COST_RATE, col = 'darkgreen',
      lwd = 3)
