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

set.seed(2025)
dfSamps <- readRDS("initial_simulation.RDS") %>%
  mutate(INCWAGE = INCWAGE)
load("data_prep/baseline_data_prep.RData")

dfSamps %>%
  
  # Set incomes of those outside labor force to 0
  mutate(INCWAGE = ifelse(LABFORCE == 0, 0, INCWAGE)) %>%
  mutate(INCWAGE = INCWAGE) %>% 
  
  # Joins taxable maximums
  inner_join(
    MAX_INCOME %>% 
      rename(YEAR = REFYEAR)
    ) %>%
  
  # Cap wages at taxable maximum
  mutate(INCWAGE_NEW = ifelse(INCWAGE > MAX_INCOME, MAX_INCOME, INCWAGE)) %>%
  
  # Create summaries using weights
  group_by(YEAR) %>%
  summarise(
    TOTAL_PAYROLL = sum(INCWAGE_NEW * WEIGHTS),
    MEAN = mean(INCWAGE[INCWAGE > 0] * WEIGHTS[INCWAGE > 0] ),
    LFPR = sum(WEIGHTS[LABFORCE == 1])/sum(WEIGHTS),
    N = n()
    ) %>%
  
  # Join with SSA population totals
  inner_join(
    df_pop_ssa %>%
      group_by(year) %>%
      summarise(TOTAL = sum(total)) %>%
      rename(YEAR = year)
  ) %>%
  
  # Makes estimate of total taxable payroll
  mutate(TOTAL = (1/1e9) * TOTAL_PAYROLL * (TOTAL/N)) %>%
  
  # Join with SSA official economic assumptions
  inner_join(
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(YEAR = REFYEAR, AWI, TAXABLE_PAYROLL) %>%
      na.omit()
  ) %>%
  mutate(PERC_DIFF = (TAXABLE_PAYROLL-TOTAL) / TOTAL) %>%
  print(n = 100)

# Keeps only non-null, unique IDs
SSA_ID <- dfSamps %>%
  distinct(SSA_ID) %>%
  na.omit() %>%
  pull ()

# Combine SSA_ID into comma-separated string
vector_string <- paste(SSA_ID, collapse = ', ')

query <- paste(
  "select * from social_security_research.puf_earnings_2006 where ID in",
  paste('(', vector_string, ')', sep = ''),
  sep = ' '
)

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
  
  # Merge earnings data where SSA_ID matches
  left_join(
    earnings,
    by = 'SSA_ID',
    relationship = 'many-to-many'
    ) %>%
  
  mutate(
    ANNUAL_EARNINGS = as.double(ANNUAL_EARNINGS),
    YEAR_EARN = as.double(YEAR_EARN)
    ) %>%
  select(
    -SSA_ID, 
    -ANNUAL_QTRS
    ) %>%
  add_row(
    dfSamps %>%
      mutate(INCWAGE = INCWAGE) %>%
      select(
        ID,
        YEAR_EARN = YEAR,
        ANNUAL_EARNINGS = INCWAGE)
    ) 

# Join birth year data, retirement data, add Full Retirement Age 
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
  ) %>% 
  mutate(
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
            5893.76)
    ),
  df_econ_assumptions %>% 
    filter(ALTERNATIVE %in% c(0,2)) %>%
    select(
      YEAR_EARN = REFYEAR, 
      AWI
      ) %>%
    na.omit()
)

# Cost of Living Adjustment dataset merging historical and future values
cola <- read.csv("cola_75_23.csv") %>%
  rename(YEAR = Year) %>%
  mutate(COLA = 1 + COLA / 100) %>%
  add_row(
    data.frame(
      YEAR = 2025:2100,
      COLA = 1.026
    )
  )

# Compute cumulative nominal wage growth after 2025
econ_assumptions_refyear <- df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(REFYEAR, GROWTH_WAGE_NOMINAL) %>%
      na.omit() %>%
      mutate(PERC_GROWTH = 1 + (GROWTH_WAGE_NOMINAL / 100)) %>%
      filter(REFYEAR >= 2025) %>%
      mutate(
        PERC_GROWTH = ifelse(REFYEAR == 2025, 1, PERC_GROWTH),
        PERC_CUM_GROWTH = cumprod(PERC_GROWTH)
        ) %>%
      select(
        REFYEAR, 
        PERC_CUM_GROWTH
        ) %>%
      filter(REFYEAR > 2025) %>%
      rename(YEAR = REFYEAR)
  
# Convert monthly bend points to annual
family_bend_points <- read.csv("family_bend_points.csv") %>%
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
      inner_join(econ_assumptions_refyear) %>%
      mutate(across(contains('_bp'), 
                    ~ round(. * PERC_CUM_GROWTH))) %>%
      rename(
        first_bp_fam = first_bp,
        second_bp_fam = second_bp,
        third_bp_fam = third_bp
        ) %>%
      select(-contains('GROWTH'))
      )

# Read in past SS bend points and project for future years using AWI
dfBendPoint2025 <- read.csv("bend_point.csv") %>%
  rename(YEAR = year) %>%
  mutate(
    first_bp = first_bp * 12,
    second_bp = second_bp * 12) %>%
  filter(YEAR <= 2021) %>%
  select(-X) %>%
  add_row(
    data.frame(
      YEAR = 2020:2100,
      first_bp = 2160,
      second_bp = 13020
    ) %>%
    
      # Extract AWI from dataset of economic assumptions
      left_join(
        df_econ_assumptions %>%
          filter(ALTERNATIVE %in% c(0, 2)) %>%
          select(YEAR = REFYEAR, AWI) %>%
          na.omit()
        ) %>%
      
      # 9779.44? 
      mutate(AWI_ADJUST = lag(AWI, 2)/9779.44) %>%
      
      # Apply adjustment to bend points
      mutate(
        first_bp = 10 * round((1/10) * first_bp * AWI_ADJUST),
        second_bp = 10 * round((1/10) * second_bp * AWI_ADJUST)
        ) %>%
      select(
        YEAR, 
        first_bp, 
        second_bp
        ) %>%
      filter(YEAR >= 2022)
    )

PIA <-  onlyRetired %>%
  mutate(BIRTH_YEAR = YEAR_BORN) %>%
  
  # Earnings required to earn one Social Security credit
  inner_join(
    credits %>%
      mutate(CREDIT = Earnings) %>%
      mutate(CREDIT = 10 * round((1/10) * CREDIT)) %>%
      rename(YEAR_EARN = Year) %>%
      select(YEAR_EARN, CREDIT)
    ) %>%
  
  # Compute how many credits an individual earned
  mutate(CREDIT_EARNED = pmin(round(ANNUAL_EARNINGS/CREDIT), 4)) %>%
  
  # Calculate reference age, total credits, and retirement age limits
  mutate(AGE_AT_2007 = 2007 - YEAR_BORN) %>%
  group_by(ID) %>%
  mutate(CREDITS = sum(CREDIT_EARNED)) %>%
  mutate(AGE_RETIRE = RETIRE_YEAR - YEAR_BORN) %>%
  mutate(
    MIN_RA = 62,
    MAX_RA = 70
    ) %>%
  filter(AGE >= 18) %>%
  
  # Join Average Wage Index 
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
  filter(CREDITS  >= 40) %>%
  
  # Adjust earnings based on AWI to compute Average Indexed Monthly Earnings (AIME):
  # Set to taxable max if annual earnings are greater than max income
  mutate(ANNUAL_EARNINGS = 
           ifelse(ANNUAL_EARNINGS > MAX_INCOME, MAX_INCOME, ANNUAL_EARNINGS)
         ) %>%
  select(-MAX_INCOME) %>%
  
  # Compute index factor
  mutate(INDEX_FACTOR = INDEX_AWI_START/AWI) %>%
  mutate(INDEX_FACTOR = 
           ifelse(AGE >= 60, 1, INDEX_FACTOR)
         ) %>%
  mutate(BEND_POINT_YEAR = YEAR_BORN  + 62) %>%
  select(
    -AWI, 
    -INDEX_START, 
    -INDEX_AWI_START
    ) %>%
  
  # Adjust past wages to "real" wage terms
  mutate(INDEXED_EARNINGS = ANNUAL_EARNINGS * INDEX_FACTOR) %>%
  select(
    -AGE_AT_2007, 
    -MIN_RA, 
    -MAX_RA
    ) %>%
  mutate(ANNUAL_EARNINGS = 
           ifelse(AGE >= 70 | RETIRE_YEAR <= YEAR_EARN, 0, ANNUAL_EARNINGS)
         ) %>%
  filter(YEAR_EARN < RETIRE_YEAR) %>%
  arrange(ID, -INDEXED_EARNINGS) %>%
  
  # Select 35 highest earning years
  group_by(
    ID, 
    BIRTH_YEAR, 
    RETIRE_YEAR, 
    BEND_POINT_YEAR, 
    AGE_RETIRE, 
    FRA
    ) %>%
  slice_head(n = 35) %>%
  
  # Compute Average Indexed Monthly Earnings
  summarise(
    AIME = mean(INDEXED_EARNINGS),
    CREDITS_EARNED = sum(CREDIT_EARNED)
    ) %>%
  
  inner_join(
    dfBendPoint2025 %>%
      select(BEND_POINT_YEAR = YEAR,
             first_bp, second_bp)
    ) %>%
  
  # Compute the Primary Insurance Amount from AIME, applying SSA bend point formula
  arrange(RETIRE_YEAR) %>%
  mutate(
    PIA = case_when(
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
  
  # Adjusts benefits for retiring early or late
  mutate(LIFE_SPAN = DEATH_YEAR - BIRTH_YEAR) %>%
  mutate(YEARS_NOT_FRA = 12 * (AGE_RETIRE - FRA)) %>%
  mutate(
    CREDITS =
      case_when(
        YEARS_NOT_FRA == 0 ~ 1,
        YEARS_NOT_FRA >= -36 & YEARS_NOT_FRA <= 0 ~ YEARS_NOT_FRA * (5/9)*(0.01),
        YEARS_NOT_FRA >= -60 & YEARS_NOT_FRA < -36 ~ ((36) * (5/9)*(-0.01) - (YEARS_NOT_FRA+36) * (5/12)*(-0.01)),
        YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% c(1917:1924) ~ (1/12) * YEARS_NOT_FRA/12 * (0.03),
        YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% seq(1925, 1941, by = 2) ~ YEARS_NOT_FRA/12 * (0.03 + 0.0025 * ((BIRTH_YEAR + 1)-1924)),
        YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% seq(1926, 1942, by = 2)  ~ YEARS_NOT_FRA/12 * (0.03 + 0.0025 * ((BIRTH_YEAR)-1924)),
        TRUE ~ YEARS_NOT_FRA/12 * 0.08)
    ) %>%
  mutate(
    PIA_ADJ =
      ifelse(
        CREDITS == 1,
        PIA,
        PIA * (1 + CREDITS)
        )
    )

# Estimate the Primary Insurance Amount by Year based on COLA - find yearly benefit values
PIA_BY_YEAR <- PIA %>%
  
  # Sequence from retirement to death year
  mutate(YEAR = list(seq(RETIRE_YEAR, DEATH_YEAR))) %>%
  unnest(YEAR) %>%
  ungroup() %>%
  
  select(
    ID, 
    PIA = PIA_ADJ, 
    AIME, 
    RETIRE_YEAR, 
    BIRTH_YEAR, 
    YEAR
    ) %>%
  mutate(AGE = YEAR - BIRTH_YEAR) %>%
  select(-BIRTH_YEAR) %>%
  
  # Join cost of living adjustment; no adjustment for first year of retirement
  inner_join(cola) %>%
  mutate(
    COLA = 
      ifelse(RETIRE_YEAR == YEAR, 1, COLA)
    ) %>%
  group_by(ID) %>%
  mutate(GROWTH_FACTOR = cumprod(COLA)) %>%
  mutate(PIA_COLA = PIA * GROWTH_FACTOR) %>%
  select(
    ID, 
    YEAR, 
    AGE, 
    PIA_COLA, 
    GROWTH_FACTOR
    ) %>%
  filter(YEAR >= 2007) %>%
  inner_join(dfSamps %>% 
      select(ID, 
             YEAR, 
             WEIGHTS), 
    by = c('ID', 'YEAR'))

# Adds demographic content and marital status
PIA_W_DEMOS <- PIA %>%
  mutate(YEAR = list(seq(RETIRE_YEAR, DEATH_YEAR))) %>%
  unnest(YEAR) %>%
  ungroup() %>%
  
  select(
    ID, 
    PIA = PIA_ADJ, 
    AIME, 
    RETIRE_YEAR, 
    BIRTH_YEAR, 
    DEATH_YEAR, 
    YEAR
    ) %>%
  
  mutate(AGE = YEAR - BIRTH_YEAR) %>%
  select(-BIRTH_YEAR) %>%
  inner_join(cola) %>%
  mutate(
    COLA = 
      ifelse(RETIRE_YEAR == YEAR, 1, COLA)
    ) %>%
  group_by(ID) %>%
  
  # Uses all COLA to get a cumulative growth factor
  mutate(GROWTH_FACTOR = cumprod(COLA)) %>%
  mutate(PIA_COLA = PIA * GROWTH_FACTOR) %>%
  inner_join(
    dfSamps %>% 
      select(ID,
             YEAR, 
             WEIGHTS, 
             ID_SP, 
             WIDOWED), 
    by = c('ID', 'YEAR')
    ) %>%
  
  select(
    ID, 
    ID_SP, 
    YEAR, 
    AGE, 
    PIA_COLA, 
    WEIGHTS, 
    WIDOWED
    )

# Find benefits each individual's family could receive
fam <- PIA %>%
  mutate(
    SPOUSE = 0.5 * PIA,
    WIDOWER = PIA,
    CHILD = 0.625 * PIA
    ) %>%
  ungroup() %>%
  inner_join(
    family_bend_points %>%
      rename(BEND_POINT_YEAR = YEAR),
    by = 'BEND_POINT_YEAR'
    ) %>%
  
  # Calculate maximum family benefit based on PIA and bend points 
  mutate(
    MAX_FAMILY_PIA =
      case_when(
        PIA <= first_bp_fam ~ PIA * 1.5,
        PIA > first_bp_fam & PIA <= second_bp_fam ~ 1.5 * (first_bp_fam) + 2.72 * (PIA -  second_bp_fam),
        PIA > second_bp_fam & PIA <= third_bp_fam ~ 1.5 * (first_bp_fam) + 2.72 * (second_bp_fam - first_bp_fam) +
          1.34 * (PIA - second_bp_fam),
        TRUE ~ 1.5 * (first_bp_fam) + 2.72 * (second_bp_fam - first_bp_fam) +
          1.34 * (third_bp_fam - second_bp_fam) + 1.75 * (PIA - third_bp_fam))
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

# Benefits for dependents (spouse, widower, child) 
auxiliary_benefits <- fam %>%
  mutate(YEAR = map2(RETIRE_YEAR, DEATH_YEAR_SP, seq)) %>%
  select(
    ID, 
    ID_SP, 
    RETIRE_YEAR,
    DEATH_YEAR, 
    DEATH_YEAR_SP, 
    YEAR,
    SPOUSE, 
    WIDOWER, 
    CHILD,
    MAX_FAMILY_PIA
    ) %>%
  unnest(YEAR) %>%
  
  # Merge in Cost of Living Adjustment
  inner_join(
    cola,
    by = 'YEAR'
  ) %>%
  
  # Spouse outlives individual (or dies in the same year)
  filter(DEATH_YEAR_SP >= DEATH_YEAR) %>%
  
  # COLA stops compounding in death year
  mutate(COLA = 
           ifelse(DEATH_YEAR == YEAR, 1, COLA)
         ) %>%
  group_by(ID) %>%
  mutate(COLA = cumprod(COLA)) %>%
  
  # Inflation adjusts all benefits each year
  mutate(
    SPOUSE = SPOUSE * COLA,
    WIDOWER = WIDOWER * COLA,
    CHILD = CHILD * COLA,
    MAX_FAMILY_PIA = MAX_FAMILY_PIA * COLA
  ) %>%
  
  # Counts children in household eligible for benefits
  left_join(
    dfSamps %>%
      group_by(SERIAL, YEAR) %>%
      summarise(NCHILD = sum(RELATE == 301)) %>%
      inner_join(
        dfSamps %>% 
          distinct(SERIAL, YEAR, ID, ID_SP),
        by = c('SERIAL', 'YEAR')
      ),
    by = c('YEAR', 'ID', 'ID_SP')
  ) %>%
  select(-SERIAL) %>%
  mutate(
    NCHILD = 
      ifelse(is.na(NCHILD), 0, NCHILD)
    ) %>%
  ungroup()

# Combining auxiliary benefits with main dataset
WITH_AUX <- PIA_W_DEMOS %>%
  left_join(
    auxiliary_benefits %>%
      select(
        ID, 
        YEAR, 
        SPOUSE, 
        WIDOWER, 
        CHILD, 
        NCHILD, 
        MAX_FAMILY_PIA
        ),
    by = c('ID', 'YEAR')
    ) %>%
  mutate(
    across(c('WIDOWED', 'SPOUSE', 'WIDOWER', 'CHILD', 'NCHILD', 'MAX_FAMILY_PIA'), 
           ~ ifelse(is.na(.), 0, .))
    ) %>%
  
  # Calculates payouts from benefits
  mutate(
    WIDOWED_BENEFITS = WIDOWER * WIDOWED,
    SPOUSE_BENEFITS = SPOUSE,
    CHILD_BENEFITS = CHILD * NCHILD
  ) %>%
  mutate(
    across(contains("BENEFITS"), 
           ~ ifelse(is.na(.), 0, .))
    ) %>%
  
  # Stop paying spouse benefits when widowed
  mutate(
    SPOUSE_BENEFITS = 
      ifelse(WIDOWED_BENEFITS > 0, 0, SPOUSE_BENEFITS)
    ) %>%
  
  # Total auxiliary benefits
  mutate(TOTAL_BENEFITS = WIDOWED_BENEFITS + SPOUSE_BENEFITS + CHILD_BENEFITS) %>%
  
  # Ensures total aux benefits do not exceed family maximum cap
  mutate(
    across(c('WIDOWED_BENEFITS', 'SPOUSE_BENEFITS', 'CHILD_BENEFITS'),
           ~ ifelse(TOTAL_BENEFITS > MAX_FAMILY_PIA,
                    MAX_FAMILY_PIA * . / TOTAL_BENEFITS,.))
    ) %>%
  
  # Recompute total auxiliary benefits based on marital and child status
  mutate(
    TOTAL_BENEFITS =
      ifelse(WIDOWED == 1, WIDOWED_BENEFITS + CHILD_BENEFITS,
             SPOUSE_BENEFITS + CHILD_BENEFITS)
    ) %>%
  mutate(
    TOTAL_BENEFITS = 
      ifelse(is.na(TOTAL_BENEFITS), 0, TOTAL_BENEFITS)
    ) %>%
  
  # If individual COLA-adjusted PIA is greater than total family benefit, keep individual
  mutate(
    PIA_COLA = 
      ifelse(PIA_COLA > TOTAL_BENEFITS, PIA_COLA, TOTAL_BENEFITS)
    )

# Calculate national total outlays in billions of dollars
OLD_AGE_OUTLAYS <- PIA_W_DEMOS %>%
  filter(AGE < 100) %>%
  group_by(YEAR) %>%
  summarise(
    TOTAL_PIA = sum(PIA_COLA),
    TOTAL = sum(PIA_COLA),
  ) %>%
  
  # Create population weighting factor that scales your sample up to represent U.S. population 
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
  
  # Apply scaling, convert to billions - Calculate national total outlays 
  mutate(
    OUTLAYS_OLD_AGE = (1/1e9) * (TOTAL_PIA * FACTOR), 
    OUTLAYS_OLD_AGE_NW = (1/1e9) * (TOTAL * FACTOR)
    )

# Estimates benefits given to dependents 
SPOUSE_OUTLAYS <- dfSamps_joined %>%
  
  # Adds spouse identifiers and weights to each worker
  inner_join(
    dfSamps %>%
      filter(RELATE == 201) %>%
      select(YEAR_EARN = YEAR, 
             ID, 
             ID_SP,
             WEIGHTS),
    by = c('YEAR_EARN', 'ID')
  ) %>%
  
  # Add worker's retirement and death year
  inner_join(
    PIA %>%
      select(ID, 
             RETIRE_YEAR, 
             DEATH_YEAR)
    ) %>%
  
  # Add PIA data of spouse to dataset
  inner_join(
    PIA %>%
      ungroup() %>%
      select(
        ID_SP = ID, 
        PIA,
        RETIRE_YEAR_SP = RETIRE_YEAR
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
  
  # Add number of children in each household to dataset
  left_join(
    dfSamps %>%
      group_by(
        SERIAL, 
        YEAR
        ) %>%
      summarise(NCHILD = sum(RELATE == 301)) %>%
      inner_join(
        dfSamps %>% distinct(SERIAL, YEAR, ID, ID_SP),
        by = c('SERIAL', 'YEAR')
      ) %>% rename(YEAR_EARN = YEAR),
    by = c('ID', 'YEAR_EARN')
  ) %>%
  
  # Eligible households: workers over 62 or workers with at least one child
  filter(AGE >= 62 | NCHILD > 0) %>%
  
  # Add in COLA for each year
  inner_join(
    cola %>% rename(YEAR_EARN = YEAR),
    by = 'YEAR_EARN'
  ) %>%
  
  group_by(ID) %>%
  mutate(COLA = ifelse(YEAR_EARN == min(YEAR_EARN), 1, COLA)) %>%
  mutate(GROWTH_PIA = cumprod(COLA)) %>%
  
  # Apply benefit rules to get adjusted benefits:
  # Spouse gets 50% of individual's PIA, child gets 75%
  mutate(
    PIA_COLA_SPOUSE = 0.5 * PIA * GROWTH_PIA,
    PIA_COLA_CHILD = 0.75 * NCHILD * PIA * GROWTH_PIA
    ) %>%
  
  select(ID, 
         YEAR = YEAR_EARN,
         AGE, 
         contains('PIA_COLA'), 
         WEIGHTS
         ) %>%
  
  # Aggregate to yearly totals
  group_by(YEAR) %>%
  summarise(PIA_COLA = sum((PIA_COLA_CHILD + PIA_COLA_SPOUSE))) %>%
  
  # Scale to estimated population total
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

# Combines primary retired worker benefits with benefits for dependents
OLD_AGE_OUTLAYS %>%
  left_join(SPOUSE_OUTLAYS, by = 'YEAR') %>%
  select(
    YEAR, 
    OUTLAYS_OLD_AGE, 
    AUX_BENEFITS
    ) %>%
  mutate(
    AUX_BENEFITS = 
      ifelse(is.na(AUX_BENEFITS), 0, AUX_BENEFITS)
    ) %>%
  mutate(TOTAL_OUTLAYS = OUTLAYS_OLD_AGE + AUX_BENEFITS) %>%
  write.csv("baseline.csv")

# Build baseline scenario 
BASELINE <- OLD_AGE_OUTLAYS %>%
  
  # Add auxiliary benefits for each year
  left_join(SPOUSE_OUTLAYS, by = 'YEAR') %>%
  select(
    YEAR, 
    OUTLAYS_OLD_AGE, 
    AUX_BENEFITS
    ) %>%
  
  # Years without spouse/child data have zero aux benefits
  mutate(
    AUX_BENEFITS = 
      ifelse(is.na(AUX_BENEFITS), 0, AUX_BENEFITS)
    ) %>%
  mutate(
    TOTAL_OUTLAYS = 
      OUTLAYS_OLD_AGE + AUX_BENEFITS
    ) %>%
  
  # Joins in nominal GDP
  inner_join(
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(YEAR = REFYEAR, NGDP) %>%
      na.omit(),
    by = 'YEAR'
  ) %>%
  
  # Computes total taxable payroll
  inner_join(
    dfSamps %>%
      inner_join(
        MAX_INCOME %>% 
          rename(YEAR = REFYEAR)
        ) %>%
      
      # Cap each worker's earnings
      mutate(
        INCWAGE = 
          ifelse(INCWAGE > MAX_INCOME, MAX_INCOME, INCWAGE)
        ) %>%
      group_by(YEAR) %>%
      summarise(INCWAGE = sum(INCWAGE),
                N = n()) %>%
      
      # Scale to full population 
      inner_join(
        df_pop_ssa %>%
          rename(YEAR = year) %>%
          group_by(YEAR) %>%
          summarise(TOTAL = sum(total))
      ) %>%
      
      # Compute total taxable payroll
      mutate(TAXABLE_PAYROLL = (1/1e9) * INCWAGE * (TOTAL / N)) %>%
      select(
        YEAR, 
        TAXABLE_PAYROLL
        )
  ) %>%
  
  # Social Security metrics
  mutate(
    PERC_TAXABLE = (TOTAL_OUTLAYS + AUX_BENEFITS) / TAXABLE_PAYROLL,
    PERC_GDP = (TOTAL_OUTLAYS + AUX_BENEFITS) / NGDP
    ) %>%
  print(n = 1000)

dfECON <- 
  read.csv("OASI_DI_PROJ_2023.csv")

# Test model against the SSA baseline
BASELINE <- BASELINE %>%
  select(YEAR, contains('PERC')) %>%
  mutate(PERC_TAXABLE = 100 * PERC_TAXABLE) %>%
  print(n = 100) %>%
  
  inner_join(
    dfECON %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(YEAR, OASI_COST_RATE)
    ) %>%
  
  mutate(DIFF_SSA = OASI_COST_RATE - PERC_TAXABLE) %>%
  print(n = 100)

# Compare models using mean absolute difference
BASELINE %>%
  mutate(MEAN = mean(abs(DIFF_SSA))) %>%
  print(n = 100)

# Plot comparison
plot(
  BASELINE$YEAR, BASELINE$PERC_TAXABLE, type = 'l', lwd = 3, lty = 1,
  ylab = 'Percent',
  xlab = 'Year',
  ylim = c(0, 20)
  )

lines(BASELINE$YEAR, BASELINE$OASI_COST_RATE, col = 'darkgreen',
      lwd = 3)
