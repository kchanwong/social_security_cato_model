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
library(arrow)
library(RMariaDB)

options(scipen = 999)

set.seed(2025)

dfSamps <- readRDS("initial_simulation.RDS") %>%
  mutate(INCWAGE = INCWAGE)

load("baseline_data_prep.RData")

### ====== Revision on dfSamps (Start) ====== ###

dfSamps_r100 <- dfSamps %>%
  
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
### ====== Revision on dfSamps (End) ====== ###

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
# db <- dbConnect()

db <- dbConnect()

earnings <- dbGetQuery(db, query) %>%
  rename(SSA_ID = ID) %>%
  mutate(SSA_ID = as.integer(SSA_ID))


### ===== dfSamps_joined dataframe ver (start) =====

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
  #  add_row(
  bind_rows(
    dfSamps %>%
      mutate(INCWAGE = INCWAGE) %>%
      select(
        ID,
        YEAR_EARN = YEAR,
        ANNUAL_EARNINGS = INCWAGE)
  ) %>%
  distinct()

### ===== dfSamps_joined dataframe ver (end) =====

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
  ) %>%
  distinct()

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
    # Use Actual data for past years (before 2022)
    # Use Medium cost (Alternative 2) for future predicted data (after 2022)
    filter(ALTERNATIVE %in% c(0,2)) %>%
    select(
      YEAR_EARN = REFYEAR, 
      AWI
    ) %>%
    na.omit()
)

# Notes on ALTERNATIVE: 
# = 0: History/Actual Data or No Alternative
# = 1: Low Cost (Alternative 1)
# = 2: Medium Cost (Alternative 2)
# = 3: High Cost (Alternative 3)

# Cost of Living Adjustment dataset merging historical and future values
cola <- read.csv("cola_75_23.csv") %>%
  rename(YEAR = Year) %>%
  mutate(COLA = 1 + COLA / 100) %>%
  bind_rows(
    data.frame(
      YEAR = 2025:2100,
      COLA = 1.026
    )
  ) %>%
  # ! KEY CHANGES: COLA NEEDS TO BE LAGGED HERE
  mutate(COLA_ADJ = lag(COLA, 1))

# Compute cumulative nominal wage growth after 2025
econ_assumptions_refyear <- df_econ_assumptions %>%
  filter(ALTERNATIVE %in% c(0,2)) %>%
  select(REFYEAR, GROWTH_WAGE_NOMINAL) %>%
  na.omit() %>%
  mutate(PERC_GROWTH = 1 + (GROWTH_WAGE_NOMINAL / 100)) %>%
  filter(REFYEAR >= 2025) %>%
  mutate(
    PERC_GROWTH = ifelse(REFYEAR == 2025, 1, PERC_GROWTH),
    # PERC_GROWTH: a calculation of cumulative growth
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
  # Convert monthly income into annual/yearly income
  mutate(across(contains('bp'), ~ . * 12)) 

family_bend_points <- family_bend_points %>%
  rename(
    first_bp_fam = first_bp,
    second_bp_fam = second_bp,
    third_bp_fam = third_bp,
    YEAR = year
  ) %>%
  
  # Create new years 2026-2100 from base year; join wage growth assumptions
  #  add_row(
  bind_rows(
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
      # first_bp & second_bd: bp from 1977
      first_bp = 2160,
      second_bp = 13020
    ) %>%
      
      # Extract AWI from dataset of economic assumptions
      left_join(
        df_econ_assumptions %>%
          filter(ALTERNATIVE %in% c(0, 2)) %>%
          select(YEAR = REFYEAR, AWI) %>%
          na.omit(),
        by = "YEAR"
      ) %>%
      
      # 9779.44: the AWI for year 1977 (2 years before 1979 when social security started)
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
    # The year-credit dataframe 
    credits %>%
      mutate(CREDIT = Earnings) %>%
      # round the credit to the closest 10
      mutate(CREDIT = 10 * round((1/10) * CREDIT)) %>%
      # rename the year to keep all variable names constant
      rename(YEAR_EARN = Year) %>%
      select(YEAR_EARN, CREDIT),
    by = "YEAR_EARN"
  ) %>%
  
  # Compute how many credits an individual earned
  # Change Made: Choose between the floor value of (ANNUAL_EARNINGS/CREDIT) & 4
  #  mutate(CREDIT_EARNED = pmin(round(ANNUAL_EARNINGS/CREDIT), 4)) %>%
  mutate(CREDIT_EARNED = pmin(floor(ANNUAL_EARNINGS/CREDIT), 4)) %>%
  
  # Calculate reference age, total credits, and retirement age limits
  mutate(AGE_AT_2007 = 2007 - YEAR_BORN) %>%
  group_by(ID) %>%
  mutate(CREDITS = sum(CREDIT_EARNED)) %>%
  mutate(AGE_RETIRE = RETIRE_YEAR - YEAR_BORN) %>%
  # Change: Should ungroup the data here, in case the group command leads to unintented results
  ungroup() %>%
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
  # INDEX_START: the year when a person turns 60
  mutate(INDEX_START = YEAR_BORN + 60) %>%
  left_join(
    AWI %>%
      # INDEX_AWI_START: the start AWI (when the person is 60)
      rename(INDEX_START = YEAR_EARN, INDEX_AWI_START = AWI),
    by = c('INDEX_START')
  ) %>%
  inner_join(
    MAX_INCOME %>%
      # MAX_INCOME: the maximum taxable income for that year
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
  # INDEX_FACTOR: (AWI when the person is 60) / (The current AWI)
  # Would be used to calculate the AIME(Average Indexed Monthly Earning)
  mutate(INDEX_FACTOR = INDEX_AWI_START/AWI) %>%
  mutate(INDEX_FACTOR = 
           # INDEX_FACTOR: euqals to the actual value when age < 60;
           # equals to 1 when age > 60
           ifelse(AGE >= 60, 1, INDEX_FACTOR)
  ) %>%
  # BEND_POINT_YEAR : the year when a person turns 62
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
           # Change to case_when: a safer way
           case_when(
             AGE >= 70 ~ 0,
             RETIRE_YEAR <= YEAR_EARN ~ 0,
             RETIRE_YEAR > YEAR_EARN ~ ANNUAL_EARNINGS
           )
         #            ifelse(AGE >= 70 | RETIRE_YEAR <= YEAR_EARN, 0, ANNUAL_EARNINGS)
  ) %>%
  filter(YEAR_EARN < RETIRE_YEAR) %>%
  #  arrange(ID, -INDEXED_EARNINGS) %>%
  arrange(ID, desc(INDEXED_EARNINGS)) %>%
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
  # Change: do not needs to keep the group now
  ungroup() %>%
  inner_join(
    dfBendPoint2025 %>%
      select(BEND_POINT_YEAR = YEAR,
             first_bp, second_bp),
    by = "BEND_POINT_YEAR"
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
      summarise(DEATH_YEAR = max(YEAR)) %>%
      # Change: ungroup to avoid unitended results
      ungroup()
  ) %>%
  
  # Adjusts benefits for retiring early or late
  mutate(LIFE_SPAN = DEATH_YEAR - BIRTH_YEAR) %>%
  # Transform # of years into # of months
  mutate(YEARS_NOT_FRA = 12 * (AGE_RETIRE - FRA)) %>%
  mutate(
    CREDITS =
      # Calculate the % of their IPA they could get
      case_when(
        # Retire when 67 --- 100% PIA
        YEARS_NOT_FRA == 0 ~ 1,
        YEARS_NOT_FRA >= -36 & YEARS_NOT_FRA <= 0 ~  YEARS_NOT_FRA * (5/9)*(0.01),
        YEARS_NOT_FRA >= -60 & YEARS_NOT_FRA < -36 ~ ((36) * (5/9)*(-0.01) - (YEARS_NOT_FRA+36) * (5/12)*(-0.01)),
        # Change: No need to include the 1/12 here
        YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% c(1917:1924) ~ (YEARS_NOT_FRA/12) * (0.03),
        # YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% c(1917:1924) ~ (1/12) * (YEARS_NOT_FRA/12) * (0.03),
        YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% seq(1925, 1941, by = 2) ~ (YEARS_NOT_FRA/12) * (0.03 + 0.0025 * ((BIRTH_YEAR + 1)-1924)),
        YEARS_NOT_FRA > 0 & BIRTH_YEAR %in% seq(1926, 1942, by = 2)  ~ (YEARS_NOT_FRA/12) * (0.03 + 0.0025 * ((BIRTH_YEAR)-1924)),
        # 8% delayed retirement credit per year.
        TRUE ~ (YEARS_NOT_FRA/12) * 0.08)
  ) %>%
  mutate(
    # PIA_ADJ: PIA adjusted based on retired age
    PIA_ADJ =
      ifelse(
        CREDITS == 1,
        PIA,
        PIA * (1 + CREDITS)
      )
  )

# Check whether there is ID_SP not in the dataset: Do Not need to run
# id_sp_list <- dfSamps %>%
#   filter(is.na(ID_SP) == FALSE) %>%
#   select(ID_SP, YEAR) %>%
#   distinct() %>%
#   rename(ID = ID_SP,
#          YEAR_SP = YEAR)
# id_list <- dfSamps %>%
#   select(ID, YEAR) %>%
#   distinct()
# id_sp_listex <- id_sp_list %>%
#   anti_join(id_list,
#             by = "ID")
# dfSamps_spex <- dfSamps %>%
#   semi_join(id_sp_listex %>%
#               rename(ID_SP = ID,
#                      YEAR = YEAR_SP),
#             by = c("ID_SP", "YEAR"))

# df2 <- dfSamps_spex %>%
#   group_by(ID) %>%
#   summarise(widowed_c = min(WIDOWED),
#             YEAR_APP = min(YEAR))
# For all the spouse not in the dataset, they died before 2007 
# Therefore, for all the people, with a possibility to be eligible for aux benefit for their spouse,
# would be among the ids


### ==== Death Year ====
death_year_df <- dfSamps %>%
  group_by(ID) %>%
  summarize(death_yr = max(YEAR), 
            .groups = "drop")

### ==== fam revised ==== 

fam2 <- PIA %>%
  ungroup() %>%
  rename(PIA_OLD = PIA) %>%
  rename(PIA = PIA_ADJ) %>%
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
        PIA > first_bp_fam & PIA <= second_bp_fam ~ 1.5 * (first_bp_fam) + 2.72 * (PIA -  first_bp_fam),
        PIA > second_bp_fam & PIA <= third_bp_fam ~ 1.5 * (first_bp_fam) + 2.72 * (second_bp_fam - first_bp_fam) +
          1.34 * (PIA - second_bp_fam),
        TRUE ~ 1.5 * (first_bp_fam) + 2.72 * (second_bp_fam - first_bp_fam) +
          1.34 * (third_bp_fam - second_bp_fam) + 1.75 * (PIA - third_bp_fam))
  ) %>%
  select(-contains('bp_fam')) 

### ==== PIA_BY_YEAR revised =====

# Estimate the Primary Insurance Amount by Year based on COLA - find yearly benefit values
PIA_BY_YEAR <- fam2 %>%
  # Sequence from retirement to death year
  # Change: Original code is not running
  #  mutate(YEAR = list(seq(RETIRE_YEAR, DEATH_YEAR))) %>%
  #  unnest(YEAR) %>%
  mutate(YEAR = map2(RETIRE_YEAR, DEATH_YEAR, seq)) %>%
  unnest(YEAR)%>%
  ungroup() %>%
  
  select(
    ID, 
    PIA,
    MAX_FAMILY_PIA,
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
    # Set the first year of retirement as the base/reference
    COLA_ADJ = 
      ifelse(RETIRE_YEAR == YEAR, 1, COLA_ADJ)
  ) %>%
  group_by(ID) %>%
  # Calculate the cumulative COLA with the base year
  mutate(GROWTH_FACTOR = cumprod(COLA_ADJ)) %>%
  # Calculated the adjusted benefit using PIA and GROWTH_FACTOR
  mutate(PIA_COLA = PIA * GROWTH_FACTOR,
         MAXFAM_PIA_COLA = MAX_FAMILY_PIA * GROWTH_FACTOR) %>%
  ungroup() %>%
  select(
    ID, 
    YEAR, 
    AGE, 
    PIA_COLA, 
    MAXFAM_PIA_COLA,
    #    MAX_FAMILY_PIA,
    GROWTH_FACTOR
  ) %>%
  filter(YEAR >= 2007) %>%
  inner_join(dfSamps %>% 
               select(ID, 
                      YEAR, 
                      WEIGHTS), 
             by = c('ID', 'YEAR'))

### ==== pia_w_demos ======

# Adds demographic content and marital status
PIA_W_DEMOS <- PIA %>%
  mutate(YEAR = map2(RETIRE_YEAR, DEATH_YEAR, seq)) %>%
  unnest(YEAR)%>%
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
  #  select(-BIRTH_YEAR) %>%
  inner_join(cola) %>%
  mutate(
    # Set the first year of retirement as the base/reference
    COLA_ADJ = 
      ifelse(RETIRE_YEAR == YEAR, 1, COLA_ADJ)
  ) %>%
  #  inner_join(cola) %>%
  #  mutate(
  #    COLA = 
  #      ifelse(RETIRE_YEAR == YEAR, 1, COLA)
  #  ) %>%
  group_by(ID) %>%
  
  # Uses all COLA to get a cumulative growth factor
  #  mutate(GROWTH_FACTOR = cumprod(COLA)) %>%
  mutate(GROWTH_FACTOR = cumprod(COLA_ADJ)) %>%
  mutate(PIA_COLA = PIA * GROWTH_FACTOR) %>%
  ungroup() %>%
  inner_join(
    dfSamps %>% 
      select(ID,
             AGE_D = AGE,
             SERIAL,
             RELATE,
             YEAR, 
             WEIGHTS, 
             ID_SP, 
             RETIRED_SP,
             RETIRED,
             WIDOWED,
             MARST), 
    by = c('ID', 'YEAR')
  ) %>%
  
  select(
    ID, 
    ID_SP, 
    YEAR, 
    RETIRE_YEAR,
    BIRTH_YEAR,
    DEATH_YEAR,
    AGE, 
    AGE_D,
    PIA_COLA, 
    WEIGHTS, 
    WIDOWED,
    MARST,
    SERIAL,
    RELATE,
    RETIRED,
    RETIRED_SP
  )

### === PIA_famdf =======
PIA_famdf <- PIA_W_DEMOS %>%
  select(-AGE_D) %>%
  left_join(death_year_df,
            by = "ID") %>%
  filter(is.na(ID_SP) == FALSE) %>%
  left_join(dfSamps %>%
              select(ID_SP = ID,
                     AGE_SP = AGE,
                     YEAR),
            by = c("ID_SP", "YEAR")) %>%
  # use AGE_SP = -1 to indicate that the spouse had died at that time (why there is no age) %>%
  # mutate(AGE_SP = replace_na(AGE_SP, -1)) %>%
  left_join(death_year_df %>%
              select(ID_SP = ID,
                     death_yr_sp = death_yr),
            by = "ID_SP") %>%
  # we assume that all the spouse who do not have a death year to "died in 2006" to simplify
  # because no death year for valid spouse indicate that the spouse had died before 2007
  mutate(death_yr_sp = replace_na(death_yr_sp, 2006)) %>%
  left_join(PIA_BY_YEAR %>%
              select(ID_SP = ID,
                     PIA_SP = PIA_COLA,
                     YEAR),
            by = c("ID_SP", "YEAR")) %>%
  bind_rows(PIA_W_DEMOS %>%
              select(-AGE_D) %>%
              left_join(death_year_df,
                        by = "ID") %>%
              filter(is.na(ID_SP) == TRUE)) %>%
  left_join(PIA_BY_YEAR %>%
              select(ID, YEAR, MAXFAM_PIA_COLA),
            by = c("ID", "YEAR")) %>%
  left_join(
    dfSamps %>%
      group_by(
        SERIAL, 
        YEAR
      ) %>%
      # All the people in the dataset recorded as RELATE == 301 is under 18
      summarise(NCHILD = sum(RELATE == 301)) %>%
      ungroup() %>%
      inner_join(
        dfSamps %>% distinct(SERIAL, YEAR, ID, ID_SP),
        by = c('SERIAL', 'YEAR')
      ) %>%
      select(-ID_SP, -SERIAL),
    by = c('ID', 'YEAR')
  ) %>%
  mutate(PIA_SP_A = case_when(
    is.na(ID_SP) == TRUE ~ 0,
    AGE_SP < 62 ~ 0,
    death_yr_sp > YEAR & is.na(PIA_SP) == TRUE ~ 0,
    TRUE ~ PIA_SP
  )) 

PIA_famdf <- PIA_famdf %>%
  select(-RETIRED_SP) %>%
  left_join(PIA_famdf %>%
              select(ID_SP = ID,
                     YEAR,
                     RETIRED_SP = RETIRED),
            by = c("ID_SP", "YEAR")) %>%
  mutate(RETIRED_SP = case_when(
    # RETIRED_SP == 2: No Spouse
    is.na(ID_SP) == TRUE ~ 2,
    is.na(RETIRED_SP) == FALSE ~ RETIRED_SP,
    is.na(RETIRED_SP) == TRUE ~ 0
  ))

# ==== BENEFIT CALCULATION ======

PIA_auxsum_df1 <- PIA_famdf %>%
  # 1. Start with PIA_nospouse
  # PIA_nospouse: benefit for worker with no spouse and no eligible children
  filter(is.na(ID_SP) == TRUE | death_yr_sp < 2007) %>%
  select(ID, ID_SP, YEAR,
         PIA_COLA) %>%
  mutate(PIA_AUX_SUM = 0) %>%
  # 2. Add PIA_spouse
  # PIA_spouse: PIA for living spouse
  bind_rows(PIA_famdf %>%
              filter(is.na(ID_SP) == FALSE) %>%
              filter(YEAR < death_yr & YEAR < death_yr_sp) %>%
              group_by(ID, ID_SP, YEAR) %>%
              mutate(PIA_SP_L = case_when(
                AGE_SP < 62 ~ 0,
                AGE_SP >= 62 ~ max(0.5*PIA_COLA, PIA_SP_A)
              )) %>%
              mutate(PIA_CHI_L = case_when(
                NCHILD == 0 ~ 0,
                NCHILD > 0 ~ max(0.5*PIA_COLA, 0.5*PIA_SP_A)
              )) %>%
              ungroup() %>%
              mutate(aux_id_sp = if_else(
                PIA_SP_L > PIA_SP_A, 1, 0
              )) %>%
              mutate(aux_id_chi = if_else(
                PIA_COLA > PIA_SP_A & NCHILD > 0, 1, 0
              )) %>%
              mutate(PIA_AUX = case_when(
                aux_id_chi == 0 & aux_id_sp == 0 ~ 0,
                aux_id_sp == 0 & aux_id_chi == 1 ~ PIA_CHI_L*NCHILD,
                aux_id_sp == 1 & aux_id_chi == 0 ~ PIA_SP_L,
                aux_id_sp == 1 & aux_id_chi == 1 ~ (PIA_CHI_L*NCHILD + PIA_SP_L)
              )) %>%
              group_by(ID, ID_SP, YEAR) %>%
              mutate(PIA_AUX = min(PIA_AUX, (MAXFAM_PIA_COLA - PIA_COLA))) %>%
              ungroup() %>%
              mutate(PIA_AUXADD = case_when(
                aux_id_chi == 1 & aux_id_sp == 0 ~ PIA_AUX,
                TRUE ~ PIA_AUX - PIA_SP_A
              )) %>%
              mutate(PIA_AUXADD = if_else(
                PIA_AUXADD < 0,0, PIA_AUXADD
              ))%>%
              select(ID, ID_SP, YEAR,
                     PIA_COLA, 
                     PIA_AUX_SUM = PIA_AUXADD)) %>%
  # 3. Add PIA_sur
  # PIA_sur: the survivor's benefit for spouse & children by year
  bind_rows(PIA_famdf %>%
              filter(is.na(ID_SP) == FALSE) %>%
              filter(death_yr != 2100) %>%
              filter(death_yr < death_yr_sp) %>%
              filter(YEAR >= death_yr) %>%
              group_by(ID, ID_SP) %>%
              mutate(PIA_S = max(PIA_COLA)) %>%
              ungroup() %>%
              # PIA_SP_S is the PIA the spouse would have after the ID-person died
              group_by(ID, ID_SP, YEAR) %>%
              mutate(PIA_SP_S = max(PIA_SP_A, PIA_S)) %>%
              mutate(aux_sp = if_else(
                PIA_SP_S > PIA_SP_A, 1, 0
              )) %>%
              mutate(PIA_CHI = max(0.75*PIA_S, 0.5*PIA_SP_A)) %>%
              mutate(aux_chi = if_else(
                0.75*PIA_S > 0.5*PIA_SP_A & NCHILD > 0, 1, 0
              )) %>%
              mutate(PIA_CHI_S = if_else(
                aux_chi == 1, PIA_CHI*NCHILD, 0
              )) %>%
              ungroup() %>%
              select(ID, ID_SP, YEAR, death_yr, death_yr_sp, NCHILD, 
                     PIA_SP_A, PIA_COLA, aux_sp,PIA_CHI_S,
                     PIA_SP_S, MAXFAM_PIA_COLA) %>%
              mutate(YEAR = map2(death_yr, death_yr_sp, seq)) %>%
              unnest(YEAR) %>%
              inner_join(cola) %>%
              mutate(
                # Set the first year of retirement as the base/reference
                COLA_ADJ = 
                  ifelse(death_yr == YEAR, 1, COLA_ADJ)
              ) %>%
              group_by(ID) %>%
              # Calculate the cumulative COLA with the base year
              mutate(GROWTH_FACTOR = cumprod(COLA_ADJ)) %>%
              # Calculated the adjusted benefit using PIA and GROWTH_FACTOR
              mutate(PIA_SP_S = PIA_SP_S * GROWTH_FACTOR,
                     #         PIA_CHI_S = PIA_CHI_S * GROWTH_FACTOR,
                     MAXFAM_PIA_COLA = MAXFAM_PIA_COLA * GROWTH_FACTOR) %>%
              ungroup() %>%
              filter(NCHILD == 0) %>%
              # Now the PIA_SP_S is the survivor's PIA for spouse with COLA adjusted
              select(ID, ID_SP, YEAR, death_yr, death_yr_sp, NCHILD,PIA_SP_A,
                     PIA_SP_S, aux_sp, MAXFAM_PIA_COLA) %>%
              # Only consider the family with no children here
              group_by(ID, YEAR) %>%
              mutate(PIA_FAMSUM = case_when(
                aux_sp == 1 ~ min(MAXFAM_PIA_COLA, PIA_SP_S),
                aux_sp == 0 ~ PIA_SP_A)) %>%
              ungroup() %>%
              mutate(PIA_AUX_SUM = case_when(
                aux_sp == 0 ~ 0,
                aux_sp == 1 ~ (PIA_FAMSUM - PIA_SP_A)
              )) %>%
              mutate(PIA_COLA = 0) %>%
              select(ID, ID_SP, YEAR,
                     PIA_COLA, PIA_AUX_SUM)) %>%
  # 4. Add PIA_sur_chi
  # PIA_sur_chi: the survivor's benefit for children of workers WITHOUT a spouse 
  bind_rows(PIA_sur_chi <- PIA_famdf %>%
              filter(is.na(ID_SP) == TRUE & NCHILD > 0) %>%
              filter(YEAR == death_yr) %>%
              mutate(PIA_CHI_S = PIA_COLA) %>%
              select(ID, ID_SP, YEAR, PIA_COLA,
                     PIA_AUX_SUM = PIA_CHI_S) ) %>%
  # 5. Add PIA_children
  # PIA_children: benefit when worker is alive, children only (no spouse)
  bind_rows(PIA_famdf %>%
              filter(is.na(ID_SP) == TRUE & NCHILD > 0) %>%
              filter(YEAR < death_yr) %>%
              mutate(PIA_CHI_L = 0.5*PIA_COLA*NCHILD) %>%
              group_by(ID, YEAR) %>% 
              mutate(PIA_AUXADD = min(PIA_CHI_L, (MAXFAM_PIA_COLA - PIA_COLA))) %>%
              ungroup()%>%
              select(ID, ID_SP, YEAR,
                     PIA_COLA,
                     PIA_AUX_SUM = PIA_AUXADD)) %>%
  # 6. Add PIA_sur_spchi
  # PIA_sur_spchi: the survivor benefit for spouse+children (if worker died first)
  bind_rows(PIA_sur_spchi <- PIA_famdf %>%
              filter(NCHILD > 0) %>%
              filter(is.na(ID_SP) == FALSE) %>%
              filter(death_yr != 2100) %>%
              filter(death_yr < death_yr_sp) %>%
              filter(YEAR >= death_yr) %>%
              group_by(ID, ID_SP) %>%
              mutate(PIA_S = max(PIA_COLA)) %>%
              ungroup() %>%
              # PIA_SP_S is the PIA the spouse would have after the ID-person died
              group_by(ID, ID_SP, YEAR) %>%
              mutate(PIA_SP_S = max(PIA_SP_A, PIA_S)) %>%
              mutate(PIA_CHI = max(0.75*PIA_S, 0.5*PIA_SP_A)) %>%
              mutate(aux_chi = if_else(
                (0.75*PIA_S > 0.5*PIA_SP_A) & NCHILD > 0, 1, 0
              )) %>%
              mutate(PIA_CHI_S = if_else(
                aux_chi == 1, PIA_CHI*NCHILD, 0
              )) %>%
              ungroup() %>%
              select(ID, ID_SP, YEAR, death_yr, death_yr_sp, 
                     NCHILD, PIA_SP_A, 
                     PIA_SP_S, PIA_CHI_S, MAXFAM_PIA_COLA, aux_chi) %>%
              mutate(YEAR = map2(death_yr, death_yr_sp, seq)) %>%
              unnest(YEAR) %>%
              inner_join(cola) %>%
              mutate(
                # Set the first year of retirement as the base/reference
                COLA_ADJ = 
                  ifelse(death_yr == YEAR, 1, COLA_ADJ)
              ) %>%
              group_by(ID) %>%
              # Calculate the cumulative COLA with the base year
              mutate(GROWTH_FACTOR = cumprod(COLA_ADJ)) %>%
              # Calculated the adjusted benefit using PIA and GROWTH_FACTOR
              mutate(PIA_SP_S = PIA_SP_S * GROWTH_FACTOR,
                     PIA_CHI_S = PIA_CHI_S * GROWTH_FACTOR,
                     PIA_SP_A = PIA_SP_A * GROWTH_FACTOR,
                     MAXFAM_PIA_COLA = MAXFAM_PIA_COLA * GROWTH_FACTOR) %>%
              ungroup() %>%
              select(ID, ID_SP, YEAR, death_yr, death_yr_sp, NCHILD,PIA_SP_A,
                     PIA_SP_S, PIA_CHI_S, MAXFAM_PIA_COLA, aux_chi) %>%
              group_by(ID, YEAR) %>%
              mutate(fmax1 = min(PIA_SP_S + PIA_CHI_S*NCHILD, MAXFAM_PIA_COLA),
                     fmax2 = PIA_SP_A + min(PIA_CHI_S*NCHILD, MAXFAM_PIA_COLA)) %>%
              # sur_choice = 1: choose to use the survivor's benefit
              mutate(sur_choice = if_else(
                fmax1 > fmax2, 1, 0
              )) %>%
              mutate(PIA_SUR_AUX = if_else(
                sur_choice == 1, 
                min(PIA_SP_S + PIA_CHI_S*NCHILD, MAXFAM_PIA_COLA),
                min(PIA_CHI_S*NCHILD, MAXFAM_PIA_COLA)
              )) %>%
              ungroup() %>%
              mutate(PIA_AUX_SUM = case_when(
                sur_choice == 1 ~ PIA_SUR_AUX - PIA_SP_A,
                sur_choice == 0 ~ PIA_SUR_AUX
              )) %>%
              mutate(PIA_COLA = 0) %>%
              select(ID, ID_SP, YEAR,
                     PIA_COLA, PIA_AUX_SUM)) %>%
  # 7. Add PIA_waux_2100
  # the people with aux benefit in 2100
  # 7.1 PIA_spouse2100: spouse aux benefit in 2100
  bind_rows(PIA_famdf %>%
              filter(is.na(ID_SP) == FALSE) %>%
              filter(YEAR == 2100) %>%
              filter(death_yr_sp == 2100) %>%
              group_by(ID, ID_SP, YEAR) %>%
              mutate(PIA_SP_L = case_when(
                AGE_SP < 62 ~ 0,
                AGE_SP >= 62 ~ max(0.5*PIA_COLA, PIA_SP_A)
              )) %>%
              mutate(PIA_CHI_L = case_when(
                NCHILD == 0 ~ 0,
                NCHILD > 0 ~ max(0.5*PIA_COLA, 0.5*PIA_SP_A)
              )) %>%
              ungroup() %>%
              mutate(aux_id_sp = if_else(
                PIA_SP_L > PIA_SP_A, 1, 0
              )) %>%
              mutate(aux_id_chi = if_else(
                PIA_COLA > PIA_SP_A & NCHILD > 0, 1, 0
              )) %>%
              mutate(PIA_AUX = case_when(
                aux_id_chi == 0 & aux_id_sp == 0 ~ 0,
                aux_id_sp == 0 & aux_id_chi == 1 ~ PIA_CHI_L*NCHILD,
                aux_id_sp == 1 & aux_id_chi == 0 ~ PIA_SP_L,
                aux_id_sp == 1 & aux_id_chi == 1 ~ (PIA_CHI_L*NCHILD + PIA_SP_L)
              )) %>%
              group_by(ID, ID_SP, YEAR) %>%
              mutate(PIA_AUX = min(PIA_AUX, (MAXFAM_PIA_COLA - PIA_COLA))) %>%
              ungroup() %>%
              mutate(PIA_AUXADD = case_when(
                aux_id_chi == 1 & aux_id_sp == 0 ~ PIA_AUX,
                TRUE ~ PIA_AUX - PIA_SP_A
              )) %>%
              mutate(PIA_AUXADD = if_else(
                PIA_AUXADD < 0,0, PIA_AUXADD )) %>%
              select(ID, ID_SP, YEAR,
                     PIA_COLA, 
                     PIA_AUX_SUM = PIA_AUXADD)) %>%
  # 7.2. PIA_children2100: the children aux benefit in 2100
  bind_rows(PIA_famdf %>%
              filter(is.na(ID_SP) == TRUE & NCHILD > 0) %>%
              filter(YEAR == 2100) %>%
              mutate(PIA_CHI_L = 0.5*PIA_COLA*NCHILD) %>%
              group_by(ID, YEAR) %>% 
              mutate(PIA_AUXADD = min(PIA_CHI_L, (MAXFAM_PIA_COLA - PIA_COLA))) %>%
              ungroup() %>%
              select(ID, ID_SP, YEAR,
                     PIA_COLA,
                     PIA_AUX_SUM = PIA_AUXADD))

# 8. Generate & Add PIA_exclude
# PIA_exclude: all the obs that DO NOT NEED EXTRA AUX CALCULATION (!= no aux benefit !!!).
PIA_exclude <- PIA_famdf %>%
  anti_join(PIA_auxsum_df1,
            by = c("ID", "YEAR"))%>%
  select(ID, ID_SP, YEAR,
         PIA_COLA) %>%
  mutate(PIA_AUX_SUM = 0)

# The Final Dataframe: PIA_auxsum_df
PIA_auxsum_df <- PIA_auxsum_df1 %>%
  bind_rows(PIA_exclude)


# Build baseline scenario 

BASELINE <- PIA_auxsum_df %>%
  left_join(dfSamps %>%
              select(ID, YEAR, WEIGHTS),
            by = c("ID", "YEAR"))

BASELINW_WEIGHTS <- BASELINE %>%
  filter(is.na(WEIGHTS) == FALSE)
BASELINW_NOWEIGHTS <- BASELINE %>%
  filter(is.na(WEIGHTS) == TRUE) %>%
  select(-WEIGHTS) %>%
  left_join(dfSamps %>%
              select(ID_SP = ID, 
                     YEAR, WEIGHTS),
            by = c("ID_SP", "YEAR"))


BASELINE_v1 <- BASELINE

BASELINE_insamps <- dfSamps_joined %>%
  rename(YEAR = YEAR_EARN) %>%
  left_join(BASELINE_v1,
            by = c("ID", "YEAR")) %>%
  select(-WEIGHTS) %>%
  left_join(dfSamps %>%
              select(ID, YEAR, WEIGHTS),
            by = c("ID", "YEAR")) %>%
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
      ungroup() %>%
      
      # Scale to full population 
      inner_join(
        df_pop_ssa %>%
          rename(YEAR = year) %>%
          group_by(YEAR) %>%
          summarise(TOTAL = sum(total)) %>%
          ungroup()
      ) %>%
      
      # Compute total taxable payroll
      mutate(TAXABLE_PAYROLL = (1/1e9) * INCWAGE * (TOTAL/N)) %>%
      select(
        YEAR, 
        TAXABLE_PAYROLL
      )
  )

BASELINE_outsamps <- BASELINE_v1 %>%
  anti_join(dfSamps_joined %>%
              rename(YEAR = YEAR_EARN),
            by = c("ID", "YEAR")) %>%
  mutate(ANNUAL_EARNINGS = 0, 
         TAXABLE_PAYROLL = 0)

BASELINE <- BASELINE_insamps %>%
  bind_rows(BASELINE_outsamps)

saveRDS(BASELINE, file = "BASELINE_v1.rds")

# == BASELINE Dataframe ====

BASELINE_SUM <- BASELINE %>%
  mutate(PIA_COLA = replace_na(PIA_COLA, 0),
         PIA_AUX_SUM = replace_na(PIA_AUX_SUM, 0)) %>%
  group_by(YEAR) %>%
  summarise(AUX_BENEFITS = sum(PIA_AUX_SUM)*(1/1e9),
            TOTAL_PIA_COLA = sum(PIA_COLA)*(1/1e9)) %>%
  ungroup() %>%
  inner_join(BASELINE %>%
               distinct(YEAR, TAXABLE_PAYROLL) %>%
               group_by(YEAR) %>%
               summarize(TAXABLE_PAYROLL = sum(TAXABLE_PAYROLL)) %>%
               ungroup(),
             by = "YEAR") %>%
  inner_join(
    dfSamps %>%
      group_by(YEAR) %>%
      summarise(N = n()) %>%
      ungroup() %>%
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
  mutate(
    AUX_BENEFITS = 
      ifelse(is.na(AUX_BENEFITS), 0, AUX_BENEFITS)
  ) %>%
  mutate(
    TOTAL_OUTLAYS = 
      (TOTAL_PIA_COLA + AUX_BENEFITS)* FACTOR
  ) %>%
  
  # Joins in nominal GDP
  inner_join(
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(YEAR = REFYEAR, NGDP) %>%
      na.omit(),
    by = 'YEAR'
  ) %>%
  
  # Social Security metrics
  mutate(
    PERC_TAXABLE = (TOTAL_OUTLAYS + AUX_BENEFITS) / TAXABLE_PAYROLL,
    PERC_GDP = (TOTAL_OUTLAYS + AUX_BENEFITS) / NGDP
  ) 

BASELINE_SUM |>
  write.csv(file="BASELINE_SUM.csv", row.names = FALSE)

dfECON <- 
  read.csv("OASI_DI_PROJECTIONS.csv")

# Test model against the SSA baseline
BASELINE_SUM_comp <- BASELINE_SUM %>%
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


