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
set.seed(2025)
## LOAD DATA --------------------
load("data_prep/init_sim_data.RData")
potential_ids <- sprintf("%08d", sample(1000000:9999999, 5000000))  # 10,000 candidates

## MORTALITY AND NEW HOUSEHOLDS -------------------- 

# Simulates one year of demographic changes for a sample population -
# Given current year (YEAR_NOW) and an initial sample dataframe (dfInitSamps)
# Applies mortality rates, removes deceased, updates ages/marital status
# (including widows), removes deceased, assigns new household IDS to
# newly independent 18-year-olds; returns updated population

makeDemographic_Project <- function(YEAR_NOW, dfInitSamps) {

# Life table data for the current year
  life_table_now <- cohort_life_tables %>%
    filter(YEAR_LIVING == YEAR_NOW) %>%
    select(
      AGE = age,
      PR_DEATH = m_q_x
      ) %>%
    mutate(SEX = 1) %>%
    add_row(
      cohort_life_tables %>%
        filter(YEAR_LIVING == YEAR_NOW) %>%
        select(
          AGE = age,
          PR_DEATH = f_q_x
          ) %>%
        mutate(SEX = 2)
    )
  
# Randomly assign deaths to people of the same sex and cohort
  dfDeaths <- dfInitSamps %>% 
    group_by(AGE, SEX) %>%
    summarise(COUNT = n()) %>%
    inner_join(life_table_now) %>%
    mutate(DEATHS = round(COUNT * PR_DEATH)) %>%
    ungroup() %>%
    filter(DEATHS > 0) %>%
    select(
      'AGE', 
      'SEX', 
      'DEATHS'
      )
  
# Detect dead IDs
  id_dead <- c()
  
  for(i in 1:nrow(dfDeaths)){
    id_dead <- c(id_dead,
                 dfInitSamps[which(dfInitSamps$AGE == dfDeaths$AGE[i] & 
                                     dfInitSamps$SEX == dfDeaths$SEX[i]),] %>% 
                   select(ID) %>%
                   slice_sample(n = dfDeaths$DEATHS[i]) %>% 
                   pull(ID)
    )
  }
  
  dfInitSamps_deaths <- dfInitSamps %>%
    mutate(DEAD = ifelse(ID %in% id_dead, 1, 0)) %>%
    mutate(
      AGE = AGE + 1,
      YEAR = YEAR_NOW
      )
  
# Detect widowed individuals
  widows <- dfInitSamps_deaths %>%
    ungroup() %>%
    
    # DEAD is household-level and >= 1 means someone in the household has died
    filter(MARST == 1 & DEAD >= 1) %>%
    arrange(SERIAL) %>%
    mutate(
      
      # PERNUM_SP is the spouse's person number (may be missing)
      PERNUM_SP = ifelse(is.na(PERNUM_SP), PERNUM, PERNUM_SP)
      ) %>%
    select(
      ID, 
      PERNUM_SP = PERNUM
      ) %>%
    
    # Returns data frame with ID, PERNUM_SP (spouse number), and WIDOWED = 1
    mutate(WIDOWED = 1)
  
# Update marital status to reflect widowhood
  dfInitSamp_deaths_w_widow <- dfInitSamps_deaths %>%
  
    # Remove old versions of widow rows
    filter(!ID %in% widows$ID) %>%
  
    # Add updated widow records
    add_row(widows) %>%
  
    # Keep only alive individuals
    filter(DEAD == 0) %>%
  
    # Update marital status if widowed 
    mutate(
      WIDOWED = ifelse(is.na(WIDOWED),  0L, WIDOWED),
      MARST = ifelse(WIDOWED == 1, 0, MARST)
      )
  
# New households  
  # Selects only unique SERIAL values from household IDS
  ids_existing <- dfInitSamp_deaths_w_widow %>% 
    distinct(SERIAL) %>% 
    pull()
  
  # Find the potential IDS that do not already exist
  new_ids <- setdiff(potential_ids, ids_existing)
  
  new18yo <- dfInitSamp_deaths_w_widow %>%
    ungroup() %>%
    filter(AGE >= 18) %>%
    filter(RELATE == 301)
  
  # Prepare new independent household entries for 18 year olds
  new_unit_ids <- sample(new_ids, new18yo %>% nrow())
  
  dfInitSamp_new_units <- dfInitSamp_deaths_w_widow %>%
    filter(AGE < 18 | RELATE != 301) %>%
    add_row(
      new18yo %>% 
        mutate(SERIAL = new_unit_ids) %>%
        mutate(SAMPLE_NO = 1) %>%
        mutate(RELATE = 101) %>%
        mutate(PERNUM = 1)
    )
  dfInitSamp_new_units
}

## SIMULATE BIRTHS BASED ON FERTILITY RATES --------------------

# Simulates births for the current year based on age-specific fertility rates.
# Given the current year (YEAR_NOW) and an updated population dataframe (dfInitSamp_new_units)
# Identifies fertile women, assigns births by probability using fertility data
# Generates baby records with demographic attributes/unique IDs, returns updated pop.

makeBabies <- function(YEAR_NOW, dfInitSamp_new_units) {
  
  # Identify women; fertility data for a given year
  females <- dfInitSamp_new_units %>% 
    filter(SEX == 2)
  
  fert_data <- dfFert06_08 %>%
    filter(YEAR == YEAR_NOW) %>%
    select(-YEAR)
  
  # Join with fertility rates to find probability of having a baby
  females_fertile <- females %>%
    left_join(fert_data, 
              by = 'AGE') %>%
    mutate(
      FERT_PER_CAPITA = 
        ifelse(is.na(FERT_PER_CAPITA), 0, FERT_PER_CAPITA),
      PR_BABY = runif(n()),
      BABY = 
        ifelse(PR_BABY < FERT_PER_CAPITA, 1, 0)
    )
  
  # Filter for those with babies and assign attributes
  babies <- females_fertile %>%
    filter(BABY == 1)
  
  babies_augmented <- babies %>%
    mutate(
      ID = NA,
      YEAR = YEAR,
      SERIAL = SERIAL,
      SAMPLE_NO = SAMPLE_NO,
      AGE = 0,
      SEX = sample(1:2, babies %>%
                     filter(BABY == 1) %>%
                     nrow(), 
                   prob = c(0.48, 0.52),
                   replace = TRUE),
      INCWAGE = 0,
      RELATE = 301,
      MARST = 0,
      LABFORCE = 0,
      DISABWRK = 0,
      RETIRED = 0,
      SSA_ID = 00
    )
  
  # Determine new PERNUM for babies
  pernum_babies <- babies %>%
    group_by(SERIAL) %>%
    summarise(PERNUM = max(PERNUM)) %>%
    mutate(PERNUM_ACTUAL = PERNUM + 1) %>%
    ungroup() %>%
    select(
      SERIAL, 
      PERNUM_ACTUAL
      )
  
  # Finalize baby records
  babies_final <- babies_augmented %>%
    inner_join(pernum_babies, by = "SERIAL") %>%
    mutate(
      PERNUM = PERNUM_ACTUAL
    ) %>%
    select(
      -PERNUM_ACTUAL, 
      -FERT_PER_CAPITA
      )
  
  # Assign unique IDs to new babies
  existing_ids <- dfInitSamp_new_units$ID
  new_ids <- setdiff(potential_ids, existing_ids)
  BABY_ID <- sample(new_ids, nrow(babies_final))
  
  # Add babies to initial dataset, remove dead
  dfInitSamp_babies <- dfInitSamp_new_units %>%
    select(-contains("DEATH"), -DEAD) %>%
    add_row(
      babies_final %>%
        select(
          -contains("DEATH"), 
          -DEAD, 
          -contains("BABY")
          ) %>%
        mutate(ID = BABY_ID)
    )
  
  dfInitSamp_babies
}
  
## MARRIAGE AND DIVORCE --------------------   

# Simulates marriage and divorce for the current year 
# Given current year (YEAR_NOW) and population dataframe after births (dfInitSamps_babies)
# Matches eligible men and women into marriages based on age and income similarity
# Updates marital and household statuses, applies age-based divorce rates
# Returns updated population with new marital statuses

makeMarriages_Divorced <- function(YEAR_NOW, dfInitSamp_babies) {
  
# Estimate the men who will be married in year t+1 
  # Identify eligible single men (age 18-70, not widowed)
  singleMen <- dfInitSamp_babies %>%
    filter(
      SEX == 1, 
      MARST == 0, 
      WIDOWED == 0, 
      AGE %in% c(18:70)
      ) %>%
    arrange(AGE)
  
  # Estimate % of unmarried men by age, compare with SSA data
  marriage_gap <- dfInitSamp_babies %>%
    group_by(AGE) %>%
    summarise(PERC_MARRIED = mean(MARST == 1)) %>%
    inner_join(
      df_pop_ssa %>%
        filter(year == YEAR_NOW) %>%
        mutate(marry_perc = m_mar / m_tot) %>%
        select(
          AGE = age, 
          MARRY_PERC_ACTUAL = marry_perc
          ),
      by = 'AGE'
      ) %>%
    # Find difference between in-sample marriage percentage and SSA projected
    mutate(MARRY_PERC = MARRY_PERC_ACTUAL - PERC_MARRIED) %>%
    mutate(MARRY_PERC = 
             ifelse(MARRY_PERC < 0, 0, MARRY_PERC)
           ) %>%
    select(AGE, MARRY_PERC)
  
  # Join gap data to men and assign marriage decision
  men_with_marry_prob <- singleMen %>% 
    inner_join(marriage_gap, by = "AGE") %>%
    mutate( 
      PR_NOT_MARRY = runif(n()),
      MARRY = ifelse(PR_NOT_MARRY <= MARRY_PERC, 1, 0)
      )
    
  # Find the likelihood of a single woman to be married
  single_women <- dfInitSamp_babies %>%
    filter(
      SEX == 2,
      MARST == 0,
      WIDOWED == 0, 
      AGE %in% 18:70
      ) %>%
    inner_join(
      df_pop_ssa %>% 
        filter(year == YEAR_NOW) %>%
        mutate(MARRY_PERC_FEMALE = f_mar / f_tot) %>%
        select(
          AGE = age, 
          MARRY_PERC_FEMALE
          ),
      by = "AGE"
    )
  
  # Attempt to match men who will marry with women using Euclidean distance 
  matches <- men_with_marry_prob %>%
    filter(MARRY == 1) %>%
    select(
      ID, 
      AGE, 
      INCWAGE
      ) %>%
    cross_join(
      single_women %>%
        select(
          ID_SP = ID, 
          AGE_SP = AGE, 
          INCWAGE_SP = INCWAGE,
          PERC_MARRY = MARRY_PERC_FEMALE
          )
      ) %>%
    # Find women with lowest weighted Euc. distance based on inc and age
    mutate(
      DISTANCE = (1/PERC_MARRY) * sqrt(((INCWAGE - INCWAGE_SP)^2 / 1000)
                                       + (AGE - AGE_SP)^2)) %>%
        filter(DISTANCE < 999999999) %>%
        arrange(
          ID, 
          DISTANCE
          ) %>%
        group_by(ID) %>%
        slice_min(DISTANCE, n = 5) %>%
        ungroup()
  
  # Select a unique spouse for each man
  serialID_married <- c()
  
  for (id in matches %>% distinct(ID) %>% pull()) {
    id_sp <- matches %>%
      filter(
        ID == id, 
        !ID_SP %in% serialID_married
        ) %>%
      head(1)
    
    if (nrow(id_sp) == 0) {
      serialID_married <- c(serialID_married, NA)
    } else {
      serialID_married <- c(serialID_married, 
                            id_sp %>% pull(ID_SP))
    }
  }
  matched_ids <- matches %>% distinct(ID) %>% pull()
    
  # Create married men entries
  married_men <- men_with_marry_prob %>%
    filter(MARRY == 1) %>%
    arrange(ID) %>%
    select(-contains('SP')) %>%
    mutate(ID_SP = serialID_married) %>%
    left_join(
      single_women %>%
        select(-contains('SP')) %>%
        select(
          ID_SP = ID, 
          PERNUM_SP = PERNUM, 
          SSA_ID_SP = SSA_ID, 
          INCWAGE_SP = INCWAGE,
          LABFORCE_SP = LABFORCE, 
          DISABWRK_SP = DISABWRK,
          RETIRED_SP = RETIRED
          ),
      by = 'ID_SP'
    ) %>%
    mutate(
      MARST = 1,
      SAMPLE_NO = SAMPLE_NO + 1
      )
  
  # Create married women entries
  married_women <- single_women %>%
    filter(ID %in% serialID_married) %>%
    select(-contains('SP')) %>%
    inner_join(
      married_men %>%
        select(
          ID = ID_SP,
          ID_SP = ID,
          SERIAL_SP = SERIAL,
          SAMPLE_NO_SP = SAMPLE_NO, 
          PERNUM_SP = PERNUM,
          SSA_ID_SP = SSA_ID,
          INCWAGE_SP = INCWAGE,
          LABFORCE_SP = LABFORCE,
          DISABWRK_SP = DISABWRK,
          RETIRED_SP = RETIRED
          ),
      by = 'ID'
    ) %>%
    mutate(
      MARST = 1,
      RELATE = 201, 
      PERNUM = 2,
      SERIAL = SERIAL_SP,
      SAMPLE_NO = SAMPLE_NO_SP
      ) %>%
    select(
      -SERIAL_SP, 
      -SAMPLE_NO_SP
      )
  
  # Singles and married 
  singles_and_married <- single_women %>%
    filter(!ID %in% married_women$ID) %>%
    mutate(MARST = 0) %>%
    add_row(
      married_men %>%
        filter(!is.na(ID_SP)) %>%
        select(
          -MARRY_PERC, 
          -MARRY, 
          -PR_NOT_MARRY
          ) %>%
        mutate(MARST = 1)) %>%
    add_row(
      married_women %>%
        select(-MARRY_PERC_FEMALE) %>%
        mutate(MARST = 1)) %>%
    select(-MARRY_PERC_FEMALE)
  
  dfInitSamp_married <- dfInitSamp_babies %>%
    filter(!ID %in% singles_and_married$ID) %>%
    add_row(
      singles_and_married
    )
  
  # Divorce rate by age
  div_rate_by_age <- df_pop_ssa %>%
    filter(year == YEAR_NOW) %>%
    group_by(age) %>%
    summarise(DIV_RATE = (m_div + f_div)/total) %>%
    rename(AGE = age)
  
  # Assign uniform max divorce rate
  max_div_rate <- max(div_rate_by_age$DIV_RATE, na.rm = TRUE)
  
  # Join with married individuals
  divorcees <- dfInitSamp_married %>% 
    filter(MARST == 1) %>%
    inner_join(
      div_rate_by_age,
      by = c('AGE')
      ) %>%
    mutate(DIV_RATE = max_div_rate)
  
  # ** Divorcees - PUT IN MORE DETAIL
  divorced_spouse <- divorcees %>%
    mutate(DIVORCE_PROB = runif(divorcees %>%
                                  nrow())) %>%
    filter(PERNUM == 2) %>%
    mutate(DIVORCED = ifelse(DIV_RATE <= DIVORCE_PROB, 0, 1)) %>%
    filter(DIVORCED == 1) %>%
    mutate(SAMPLE_NO = SAMPLE_NO + 1)

  divorced_head <- divorcees %>%
    filter(
      PERNUM == 1,
      ID %in% divorced_spouse$ID_SP
      ) %>%
    select(-DIV_RATE) %>%
    mutate(DIVORCED = 1)
  
  divorced_couples <- divorced_head %>%
    add_row(divorced_spouse %>%
              select(-DIVORCE_PROB, -DIV_RATE))
  
  dfInitSamp_divorced <- dfInitSamp_married %>%  
    filter(!ID %in% divorced_couples$ID) %>%
    mutate(DIVORCED = 0) %>%
    add_row(divorced_couples)
  
  dfInitSamp_divorced
}

## INCOME GROWTH AND LABOR FORCE --------------------

# Simulates labor force participation, disability, and income dynamics for current year
# Given the year (YEAR_NOW) and post-divorce population (dfInitSamp_divorced)
# Updates individual labor force participation status based on transition probability
# Aligns LF participation rates w/ SSA targets, assigns disability randomly
# Imputes wages for new or re-employed workers, gives updated pop. dataset

makeIncome_and_LF_and_Disability <- function(YEAR_NOW, dfInitSamp_divorced) {
  
  dfInitSamp_LAB_FORCE_transition <- dfInitSamp_divorced %>%
  # Join with probabilities of leaving workforce in current year
    left_join(transition %>%
                filter(YEAR == YEAR_NOW) %>%
                select(-YEAR), 
              by = c('AGE', 'SEX', 'LABFORCE')) %>%
    # Fill missing probabilities with 1 (exits LF)  
    mutate(PROB_EXIT_LF = ifelse(
      is.na(PROB_EXIT_LF), 1, PROB_EXIT_LF),
      
    # Assign a random probability for each individual
    PROB_EXIT_LF_ID = runif(dfInitSamp_divorced %>% nrow()),
    
    # If retired, exit probability > 1
    PROB_EXIT_LF = ifelse(RETIRED == 1, 2, PROB_EXIT_LF),
    
    # Determine LF status 
    LABFORCE = ifelse(PROB_EXIT_LF_ID < PROB_EXIT_LF, 0, 1),
    
    # Update retirement status if eligible
    RETIRED = ifelse(RETIRED == 0 & 
                       LABFORCE == 0 &
                       AGE >= 63, 1, RETIRED)
  )
  
  # Calculate actual LFPR in simulated data
  actual_lfpr <- dfInitSamp_LAB_FORCE_transition %>%
    group_by(
      AGE, 
      SEX
      ) %>%
    summarise(
      LFPR = sum(LABFORCE == 1)/n(),
      LFPR_VOL = sum(LABFORCE == 1),
      COUNT = n()
      ) %>%
    filter(AGE >= 18)
  
  # Join with SSA LFPR targets and compute differences 
  targets_lfpr <- LFPR %>%
    filter(Year == YEAR_NOW) %>%
    mutate(Value = Value/100) %>%
    rename(
      YEAR = Year,
      SEX = Sex, 
      AGE = AgeRange, 
      LFPR_SSA = Value
      )
  
  # Determine if LFPR needs to be changed from SSA data
  adjust_lfpr <- actual_lfpr %>%
    left_join(
      targets_lfpr, 
      by = c("AGE", "SEX")
      ) %>%
    mutate(
      DIFF = LFPR_SSA - LFPR,
      NEED_CHANGE = round(COUNT * DIFF)
    )
  
  # Join NEED_CHANGE values to main data
  dfInitSamp_LAB_FORCE_transition <- dfInitSamp_LAB_FORCE_transition %>%
    left_join(adjust_lfpr %>% 
                select(
                  AGE, 
                  SEX, 
                  NEED_CHANGE
                  ), 
              by = c("AGE", "SEX")) %>%
    group_by(
      AGE, 
      SEX
      ) %>%
    group_modify(~ {
      df <- .x
      need_change <- unique(df$NEED_CHANGE)
      
      # Skip adjustment if need_change is null
      if (is.na(need_change) || need_change == 0) {
        return(df)
      }
      
      # If need_change is greater than 0 (LFPR_SSA > LFPR) - 
        # Add those currently out of LF into LF
      if (need_change > 0) {
        eligible <- which(df$LABFORCE == 0)
        
        # Randomly select those to flip to 1
        if (length(eligible) >= need_change) {
          flip_ids <- sample(eligible, need_change)
          df$LABFORCE[flip_ids] <- 1
        }
      } 
      
      # If need_change is less than 0 (LFPR_SSA < LFPR) -
        # Remove those currently in LF out of LF
      else if (need_change < 0) {
        eligible <- which(df$LABFORCE == 1)
        
        # Randomly select those to flip to 0
        if (length(eligible) >= abs(need_change)) {
          flip_ids <- sample(eligible, abs(need_change))
          df$LABFORCE[flip_ids] <- 0
        }
      }
      return(df)
    }) %>%
    ungroup() %>%
    select(-NEED_CHANGE)
  
  # Assign disability status
  dfInitSamp_LAB_FORCE_transition <- dfInitSamp_LAB_FORCE_transition %>%
    select(
      -PROB_EXIT_LF_ID, 
      -PROB_EXIT_LF,
      -END
      ) %>%
    
    # Disabled if randomly generated probability is < 0.001
    mutate(
      DISABWRK_PERC = runif(dfInitSamp_LAB_FORCE_transition %>% nrow()),
      DISABWRK = ifelse(0.001 > DISABWRK_PERC | DISABWRK == 1, 1, 0)
      )
  
  # Income growth 
  dfInitSamp_inc_growth <- dfInitSamp_LAB_FORCE_transition %>%
    mutate(INCWAGE = INCWAGE)
  
  # For people employed #
  counts <- dfInitSamp_inc_growth %>%
    filter(LABFORCE == 1 & INCWAGE == 0) %>%
    group_by(AGE) %>%
    summarise(COUNT = n())

  wages <- c()
  wages_df <- samps %>%
    filter(
      YEAR == 2007,
      INCWAGE > 0
      ) %>%
    distinct(
      AGE, 
      INCWAGE
      ) %>%
    arrange(AGE)
  wages_df <- wages_df %>%
    add_row(
      wages_df %>%
        filter(AGE == 70) %>%
        mutate(AGE = list(71:100)) %>%
        unnest(AGE)
    )
  
  # Loop over each age group, pulls income 
  wages <- c()
  
  for(i in 1:nrow(counts)) {
    sampled_wages <- wages_df %>% 
      filter(AGE == counts$AGE[i]) %>%
      mutate(INCWAGE = as.integer(INCWAGE)) %>%
      
      # Randomly sample n wages equal to the number of people with zero income
      # Append the sampled values
      # For people who are newly employed, assign random income from 2007 initial income based on age
      slice_sample(n = counts$COUNT[i], replace = TRUE) %>%
      pull(INCWAGE)
    
    wages <- c(wages, sampled_wages)
  }
  
  # Replace zero-income rows with wages
  dfInitSamp_inc_growth <- dfInitSamp_inc_growth %>%
    filter(LABFORCE != 1 | INCWAGE != 0) %>%
    
    # Replace INCWAGE of those with zero income with sampled wages
    add_row(
      dfInitSamp_inc_growth %>%
        filter(LABFORCE == 1 & INCWAGE == 0) %>%
        arrange(AGE) %>%
        mutate(INCWAGE = wages)
    )
  
  # Those not in LF have zero income
  dfInitSamp_inc_growth <- dfInitSamp_inc_growth %>%
    mutate(INCWAGE = ifelse(
      LABFORCE == 0, 0, INCWAGE)
    )
}

## ECONOMIC & POPULATION CHANGES BY YEAR --------------------

Begin <- Sys.time()
samps <- dfInitSamps

# Loop through each year 2008-2100 and apply functions
for(i in 2008:2100) {
  print(i)
  maxYear <- samps %>% 
    # Filters for previous year's data
    filter(YEAR == i - 1)
  
  # Mortality and new households for current year
  dfInitSamp_new_units <- makeDemographic_Project(i, maxYear)
  
  # Fertility for current year
  dfInitSamp_babies <- makeBabies(i, dfInitSamp_new_units)
  
  # Marriages and divorces for current year
  dfInitSamp_divorced <- makeMarriages_Divorced(i, dfInitSamp_babies)
  
  # Income, LF, Disability changes for current year
  dfInitSamp_econ_growth <- makeIncome_and_LF_and_Disability(i, dfInitSamp_divorced)
  
  samps <- samps %>% add_row(dfInitSamp_econ_growth)
}

# Income distribution - with brackets and percent distributions
DISTRIBUTION <- income_dist %>%
  pivot_longer(
    # Keep Year and Total unchanged
    cols = -c(Year, Total),
    
    # New column for previous column names
    names_to = "Income_Bracket",
    
    # New column for values
    values_to = "Count"
  ) %>%
  na.omit() %>%
  
  # Compute percentage of share of total in each income bracket for the year
  mutate(PERC = Count/Total) %>%
  mutate(
    # Get min and max bounds from bracket names
    MIN_TO = as.numeric(str_extract(Income_Bracket, "(?<=x)\\d+(?=_)")),
    MAX_TO = as.numeric(str_extract(Income_Bracket, "(?<=_)\\d+")),
    
    # Handle "Max" separately - for rows where the bracket is "Max"
      # Set MIN_TO as highest previous MAX_TO value
      # Set MAX_TO as Inf (no upper limit)
    MIN_TO = ifelse(Income_Bracket == "Max", max(MAX_TO, na.rm = TRUE), MIN_TO),
    MAX_TO = ifelse(Income_Bracket == "Max", Inf, MAX_TO)
  ) %>%
  
  # MIN_TO shifts down, makes each MIN_TO equal to prev. row's MAX_TO
  mutate(
    MIN_TO = lag(MAX_TO),
    MIN_TO = ifelse(is.na(MIN_TO) | is.infinite(MIN_TO), 0, MIN_TO), 
    MAX_TO = ifelse(is.na(MAX_TO) | is.infinite(MAX_TO), 0, MAX_TO)
    ) %>%
  
  # Join with max income
  inner_join(MAX_INCOME %>% rename(Year = REFYEAR)) %>%
  mutate(
    MAX_TO = ifelse(MAX_TO > MAX_INCOME, MAX_INCOME, MAX_TO),
    MIN_TO = ifelse(MIN_TO > MAX_INCOME, MAX_INCOME, MIN_TO)
    ) %>%
  
  mutate(
    EXPECTED = (MIN_TO + MAX_TO)/2,
    EXPECTED = ifelse(Income_Bracket == 'Max', 2.5 * MAX_INCOME, EXPECTED),
    PERC_MAX = ifelse(Income_Bracket == 'Max', MAX_INCOME, EXPECTED),
    MAX_TO = ifelse(Income_Bracket == 'Max', EXPECTED, MAX_TO)
    )

df_econ_assumptions %>%
  filter(ALTERNATIVE %in% c(0,2)) %>%
  select(REFYEAR, AWI) %>%
  na.omit() %>%
  print(n = 300)

## PROJECTED DISTRIBUTIONS --------------------

# Create projected distribution for years 2023-2100
distribution_maxinc <- DISTRIBUTION %>%
  filter(Year == 2022) %>%
  # Turning income brackets to relative proportions
  mutate(
    MIN_TO = MIN_TO / 147000,       # Figure out what 147000 is
    MAX_TO = MAX_TO / 147000
    ) %>%
  select(-MAX_INCOME) %>%
  mutate(Year = list(2023:2100)) %>%
  unnest(Year) %>%
  arrange(Year) %>%
  inner_join(
    MAX_INCOME %>% 
      rename(Year = REFYEAR)) %>%
  # Scaling brackets back up
  mutate(
    MIN_TO = MIN_TO * MAX_INCOME,
    MAX_TO = MAX_TO * MAX_INCOME
    )

# Adding projected rows back into distribution
DISTRIBUTION <- DISTRIBUTION %>%
  add_row(distribution_maxinc)

# Simulating distribution of incomes for a given year
Make_Year_Distribution <- function(YEAR) {
  
  # Determine how many income brackets exist for given year
  N <- DISTRIBUTION %>%
    group_by(Year) %>%
    summarise(Count = n()) %>%
    filter(Year == YEAR) %>%
    pull(Count)
  
  DISTRIBUTION <- DISTRIBUTION %>% 
    filter(Year == YEAR)
  
  INCOMES <- c()
  
  # Simulate income values
    # For each income bracket, generate number of RVs proportional to that bracket's share of pop
  for(i in 1:N) {
    if(i < N) {
      MIN <- DISTRIBUTION$MIN_TO[i]
      MAX <- DISTRIBUTION$MAX_TO[i]
      INCOMES_DRAW <- round(runif(round(DISTRIBUTION$PERC[i] * 25000), MIN, MAX))
      INCOMES <- c(INCOMES, INCOMES_DRAW)
    }
    
    else {
      MIN <- DISTRIBUTION$MIN_TO[i]
      MAX <- DISTRIBUTION$MAX_INCOME[i] * 2.5
      INCOMES_DRAW <- round(runif(round(DISTRIBUTION$PERC[i] * 25000), MIN, MAX))
      INCOMES <- c(INCOMES, INCOMES_DRAW)
    }
  }
  INCOMES
}

# Aligns income distribution in a given year with a target distribution
# For specific year, constructs capped income distribution (based on MAX_INCOME)
# Matches individuals' income percentiles to target
# Returns updated dataset where incomes are adjusted to reflect target distribution

matchDist <- function(year) {
  
  # Make target distribution
  INC <- Make_Year_Distribution(year)
  MAX <- MAX_INCOME %>% 
    filter(REFYEAR == year) %>%
    pull(MAX_INCOME)
  
  # Assign ID and sort by income
  target_dist <- data.frame(ID = 1:length(INC),
                            INCWAGE = INC) %>%
    arrange(INCWAGE) %>%
    
    # Cap incomes above MAX
    mutate(
      INCWAGE_NEW = ifelse(INCWAGE > MAX, MAX, INCWAGE),
    
    # Total income retained after capping
      PERC_TOTAL = sum(INCWAGE_NEW) / sum(INCWAGE), 
    
    # Cumulative income share
      PERC = cumsum(INCWAGE) / sum(INCWAGE), 
    
    # Rank-based percentile position
      percentile = (row_number() - 0.5) / n
    )
  
  # Filter for target year, compute percentile for everyone w non-zero income
  future_dist <- samps %>%
    filter(
      YEAR == year, 
      INCWAGE != 0
      ) %>%
    arrange(INCWAGE) %>%
    mutate(percentile = (row_number() - 0.5) / n())
  
  
  # Match percentiles between distributions 
  suppressWarnings(future_dist <- future_dist %>%
                     mutate(
                       matched_PERC = approx(
                         x = target_dist$percentile,
                         y = target_dist$PERC,
                         xout = percentile,
                         rule = 2
                         )$y
                       )
                   )
  
  suppressWarnings(perc_to_income <- approxfun(
    x = target_dist$PERC,
    y = target_dist$INCWAGE,
    rule = 2
  ))
  
  future_dist_adjusted <- future_dist %>%
    mutate(INCWAGE = perc_to_income(matched_PERC)) %>%
    select(
      -percentile, 
      -matched_PERC
      )
  
  samps %>%
    filter(
      YEAR == year,
      INCWAGE == 0
      ) %>%
    add_row(future_dist_adjusted)
}

target <- matchDist(2007)

for(i in 2008:2100) {
  print(i)
  target <- target %>%
    add_row(matchDist(i))
}

filtered_econ_assumptions <- df_econ_assumptions %>%
  filter(ALTERNATIVE %in% c(0,2)) %>%
  select(
    YEAR = REFYEAR, 
    AWI
    ) %>%
  na.omit()

# Produces table of YEAR and AWI_ADJUST
target_adjusted <- target %>%
  group_by(YEAR) %>%
  summarise(MEAN = mean(INCWAGE)) %>%
  inner_join(filtered_econ_assumptions) %>%
  
  # Adjusting AWI - calculating percent difference from base year (2007)
  mutate(AWI_ADJUST = 1 + ((AWI - 40405.48) / 40405.48)) %>%
  select(
    YEAR, 
    AWI_ADJUST
    )

# Target data limited to years that exist in target_adjusted
samps <- target %>%
  inner_join(
    target_adjusted, 
    by = 'YEAR'
    ) %>%
  mutate(INCWAGE = INCWAGE) %>%
  select(-AWI_ADJUST)

# Calculate weights of each sample pop in the real population
weights <- df_pop_ssa %>%
  mutate(m_not_married = m_tot - m_mar ) %>%
  select(
    year, 
    age, 
    m_not_married
    ) %>%
  rename(
    YEAR = year, 
    AGE = age, 
    TOTAL_SSA = m_not_married
    ) %>%
  mutate(
    MARST = 0, 
    SEX = 1
    ) %>%
  
  # Appends rows for married men
  add_row(
    df_pop_ssa %>%
      select(
        year, 
        age, 
        m_mar
        ) %>%
      rename(
        YEAR = year, 
        AGE = age, 
        TOTAL_SSA = m_mar
        ) %>%
      mutate(
        MARST = 1, 
        SEX = 1
        )
  ) %>%
  
  # Appends rows for married women
  add_row(
    df_pop_ssa %>%
      select(
        year, 
        age, 
        f_mar
        ) %>%
      rename(
        YEAR = year, 
        AGE = age, 
        TOTAL_SSA = f_mar
        ) %>%
      mutate(
        MARST = 1, 
        SEX = 2
        )
  ) %>%
  
  # Appends rows for unmarried women
  add_row(
    df_pop_ssa %>%
      mutate(f_not_married = f_tot - f_mar) %>%
      select(
        year, 
        age, 
        f_not_married
        ) %>%
      rename(
        YEAR = year, 
        AGE = age, 
        TOTAL_SSA = f_not_married
        ) %>%
      mutate(
        MARST = 0, 
        SEX = 2)
    ) %>%
  
  # Join total population by year
  inner_join(
    df_pop_ssa %>%
      group_by(year) %>%
      summarise(TOTAL_POP = sum(total)) %>%
      rename(YEAR = year)
  ) %>%
  
  # Compute share of total population
  mutate(PERC_SSA = TOTAL_SSA/TOTAL_POP) %>%
  inner_join(
    samps %>%
      group_by(YEAR) %>%
      summarise(PEOPLE = n())
  ) %>%
  
  # Compute how many sample people each cell should represent
  mutate(SAMPS_POP = PERC_SSA * PEOPLE) %>%
  inner_join(
    samps %>%
      filter(AGE < 100) %>%
      group_by(
        YEAR, 
        AGE, 
        MARST, 
        SEX
        ) %>%
      summarise(COUNT = n())
  ) %>%
  
  # Computing weight factor based on SAMPS_POP size
  mutate(WEIGHTS = SAMPS_POP/COUNT)

samps_w_weights <- samps %>%
  filter(AGE < 100) %>%
  inner_join(weights, by = c('YEAR', 'AGE', 'SEX', 'MARST')) %>%
  group_by(YEAR) %>%
  mutate(MEAN_INCOME = mean(INCWAGE)) %>%
  inner_join(
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(
        YEAR = REFYEAR, 
        AWI
        ) %>%
      na.omit()
  ) %>%
  select(
    -TOTAL_SSA, 
    -TOTAL_POP, 
    -DISABWRK_PERC, 
    -PERC_SSA, 
    -PEOPLE, 
    -SAMPS_POP, 
    -COUNT,
    -MEAN_INCOME, 
    -AWI
    )

samps_w_weights %>%
  mutate(INCWAGE = ifelse(LABFORCE == 0, 0, INCWAGE)) %>%
  mutate(INCWAGE = INCWAGE) %>%
  inner_join(
    MAX_INCOME %>% 
      rename(YEAR = REFYEAR)
    ) %>%
  
  # Caps incomes at MAX_INCOME (yearly maximum taxable)
  mutate(INCWAGE_NEW = ifelse(INCWAGE > MAX_INCOME, MAX_INCOME, INCWAGE)) %>%
  group_by(YEAR) %>%
  
  # Compute summary statistics 
  summarise(
    TOTAL_PAYROLL = sum(INCWAGE_NEW * WEIGHTS),
    MEAN = mean(INCWAGE[INCWAGE > 0] * WEIGHTS[INCWAGE > 0] ),
    LFPR = sum(WEIGHTS[LABFORCE == 1])/sum(WEIGHTS),
    N = n()
    ) %>%
  inner_join(
    df_pop_ssa %>%
      group_by(year) %>%
      summarise(TOTAL = sum(total)) %>%
      rename(YEAR = year)
  ) %>%
  mutate(TOTAL = (1/1e9) * TOTAL_PAYROLL * (TOTAL/N)) %>%
  inner_join(
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(
        YEAR = REFYEAR, 
        AWI, 
        TAXABLE_PAYROLL
        ) %>%
      na.omit()
  ) %>%
  
  # Calculate percent difference between model's total taxable payroll and SSA's
  mutate(PERC_DIFF = (TAXABLE_PAYROLL-TOTAL) / TOTAL) %>%
  print(n = 100)

write_rds(samps_w_weights, "initial_simulation.RDS")
