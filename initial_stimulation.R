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
library(arrow)

checkpoint_data <- "checkpoint_data/"

options(scipen = 999)

set.seed(2025)
load("init_sim_data.RData")

# potential_ids: potential id (8-digit)
# potential_ids would be used both for person and household
potential_ids <- sprintf("%08d", sample(10000000:99999999, 5000000))  # 10,000 candidates

# For the simplicity of testing:
YEAR_NOW <- 2008

makeDemographic_Project <- function(YEAR_NOW, dfInitSamps) {
  
  # Life table data for the current year
  life_table_now <- cohort_life_tables %>%
    filter(YEAR_LIVING == YEAR_NOW) %>%
    select(AGE = age,
           PR_DEATH = m_q_x) %>%
    mutate(SEX = 1) %>%
    add_row(
      cohort_life_tables %>%
        filter(YEAR_LIVING == YEAR_NOW) %>%
        select(AGE = age,
               PR_DEATH = f_q_x) %>%
        mutate(SEX = 2)
    )
  
  # Assign deaths to people of the same sex and cohort
  dfDeaths <- dfInitSamps %>% 
    group_by(AGE, SEX) %>%
    summarise(COUNT = n()) %>%
    # inner_join(life_table_now) %>%
    left_join(life_table_now) |>
    mutate(DEATHS = round(COUNT * PR_DEATH)) %>%
    ungroup() %>%
    filter(DEATHS > 0) %>%
    select('AGE', 'SEX', 'DEATHS')
  
  # Detect dead IDs
  id_dead <- c()
  
  id_dead <- c()
  for(i in 1:nrow(dfDeaths)){
    id_dead <- c(id_dead,
                 dfInitSamps[which(dfInitSamps$AGE == dfDeaths$AGE[i] 
                                   & dfInitSamps$SEX == dfDeaths$SEX[i]),] %>% 
                   select(ID) %>%
                   slice_sample(n = dfDeaths$DEATHS[i]) %>% 
                   pull(ID)
    )
  }
  
  dfInitSamps_deaths <- dfInitSamps %>%
    mutate(DEAD = ifelse(ID %in%
                           id_dead, 1, 0)) %>%
    mutate(AGE = AGE + 1,
           YEAR = YEAR_NOW)
  
  widows2_df1 <- dfInitSamps_deaths %>%
    ungroup() |>
    # DEAD is household-level and >= 1 means someone in the household has died
    # MARST: Married Status, MARST == 1 -> married, spouse present
    filter(MARST == 1 & DEAD >= 1) %>%
    arrange(SERIAL) %>%
    # select the dead people's ID, household number, death stauts and personal number
    # the dead person's personal number would be the spouse-pernum for their spouse
    select(ID, DEAD, SERIAL,
           PERNUM_SP = PERNUM)
  widows2 <- dfInitSamps_deaths |>
    # only keep the widowed people who:
    # (1) are in the same household as the dead
    # (2) their spouse-pm is the same as the pernum of the dead
    semi_join(widows2_df1,
              by = c("SERIAL", "PERNUM_SP")) |> 
    # mutate new "widowed" info
    mutate(WIDOWED = 1)
  
  dfInitSamp_deaths_w_widow <- dfInitSamps_deaths |>
    # delete original record of the new widowed people
    anti_join(widows2,
              by = "ID") |>
    # add the edited info for the new widowed
    bind_rows(widows2) |>
    # Keep only alive individuals
    filter(DEAD == 0) |>
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
  
  # Find the potential household IDS that do not already exist
  new_ids <- setdiff(potential_ids, ids_existing)
  
  # Count the number of (bio) children older than 18 and still lives with their families
  new18yo <- dfInitSamp_deaths_w_widow %>%
    ungroup() %>%
    filter(AGE >= 18) %>%
    # RELATE == 301: Biological Children
    # Question: why do not include adopted children (although it might not make a huge difference)
    filter(RELATE == 301)
  
  # Prepare new independent household entries for 18 year olds
  # randomly select n new potential household ids for children who are older than 18 years old
  new_unit_ids <- sample(new_ids, new18yo %>% nrow())
  dfInitSamp_new_units <- dfInitSamp_deaths_w_widow %>%
    #    filter(AGE < 18 | RELATE != 301) %>%
    anti_join(new18yo,
              by = "ID") |>
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

# ** DESCRIBE FUNCTION
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
      FERT_PER_CAPITA = ifelse(is.na(FERT_PER_CAPITA), 0, 
                               FERT_PER_CAPITA),
      PR_BABY = runif(n()),
      BABY = ifelse(PR_BABY < FERT_PER_CAPITA, 1, 0)
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
    select(SERIAL, PERNUM_ACTUAL)
  
  # Finalize baby records
  babies_final <- babies_augmented %>%
    #    inner_join(pernum_babies, by = "SERIAL") %>%
    left_join(pernum_babies, by = "SERIAL") |>
    mutate(
      PERNUM = PERNUM_ACTUAL
    ) %>%
    select(-PERNUM_ACTUAL, -FERT_PER_CAPITA)
  
  # Assign unique IDs to new babies
  existing_ids <- dfInitSamp_new_units$ID
  new_ids <- setdiff(potential_ids, existing_ids)
  BABY_ID <- sample(new_ids, nrow(babies_final))
  
  # Add babies to initial dataset, remove dead
  dfInitSamp_babies <- dfInitSamp_new_units %>%
    select(-contains("DEATH"), -DEAD) %>%
    add_row(
      babies_final %>%
        select(-contains("DEATH"), -DEAD, -contains("BABY")) %>%
        mutate(ID = BABY_ID)
    )
  
  dfInitSamp_babies
}

## MARRIAGE AND DIVORCE --------------------   

# ** DESCRIBE FUNCTION
makeMarriages_Divorced <- function(YEAR_NOW, dfInitSamp_babies) {
  
  # Estimate the men who will be married in year t+1 
  # Identify eligible single men (age 18-70, not widowed)
  singleMen <- dfInitSamp_babies %>%
    filter(SEX == 1, 
           MARST == 0, 
           WIDOWED == 0, 
           AGE %in% c(18:70)) %>%
    arrange(AGE)
  
  # Estimate % of unmarried men by age, compare with SSA data
  # Changes made: 
  # 1. limit the gender when calculating the marriage gap
  # 2. do the same marriage gap for women
  marriage_gap <- dfInitSamp_babies |>
    filter(SEX == 1) |>
    group_by(AGE) |>
    summarise(PERC_MARRIED = mean(MARST == 1)) |>
    ungroup() |>
    #    inner_join(
    left_join(
      df_pop_ssa |>
        filter(year == YEAR_NOW) |>
        mutate(marry_perc = m_mar/m_tot) |>
        select(AGE = age, MARRY_PERC_ACTUAL = marry_perc),
      by = "AGE"
    ) |>
    # Find difference between in-sample marriage percentage and SSA projected
    mutate(MARRY_PERC = MARRY_PERC_ACTUAL - PERC_MARRIED) |>
    mutate(MARRY_PERC = ifelse(MARRY_PERC < 0, 0, MARRY_PERC)) |>
    select(AGE, MARRY_PERC) 
  
  # Join gap data to men and assign marriage decision
  # The number of men getting married this year is the difference between the actual married rate
  # and the ssa predicted value
  men_with_marry_prob <- singleMen %>% 
    #    inner_join(marriage_gap, by = "AGE") %>%
    left_join(marriage_gap, by = "AGE") |>
    mutate( 
      PR_NOT_MARRY = runif(n()),
      MARRY = ifelse(PR_NOT_MARRY <= MARRY_PERC, 1, 0)
    )
  
  # Find the likelihood of a single woman to be married
  single_women <- dfInitSamp_babies %>%
    filter(SEX == 2,
           MARST == 0, 
           WIDOWED == 0, 
           AGE %in% 18:70) %>%
    inner_join(
      df_pop_ssa %>% 
        filter(year == YEAR_NOW) %>%
        mutate(MARRY_PERC_FEMALE = f_mar / f_tot) %>%
        select(AGE = age, MARRY_PERC_FEMALE),
      by = "AGE"
    )
  
  # Attempt to match men who will marry with women using Euclidean distance 
  matches <- men_with_marry_prob %>%
    filter(MARRY == 1) %>%
    select(ID, AGE, INCWAGE) %>%
    cross_join(
      single_women %>%
        select(ID_SP = ID, 
               AGE_SP = AGE, 
               INCWAGE_SP = INCWAGE,
               PERC_MARRY = MARRY_PERC_FEMALE)
    ) %>%
    # Find women with lowest weighted Euc. distance based on inc and age
    mutate(lgwage = log1p(INCWAGE),
           lgwage_sp = log1p(INCWAGE_SP)) |>
    mutate(lgwage_sc = scale(lgwage),
           lgwage_sp_sc = scale(lgwage_sp),
           age_sc = scale(AGE),
           age_sp_sc = scale(AGE_SP)) |>
    mutate(
      DISTANCE = (1/PERC_MARRY) * sqrt(((lgwage - lgwage_sp)^2)+ (age_sc - age_sp_sc)^2)
      #                                        + lamda*((INCWAGE == 0) & (INCWAGE_SP == 0))
      #                                       + lamda2*((INCWAGE*INCWAGE_SP == 0))
    ) |>
    filter(DISTANCE < 999999999) %>%
    #    slice_min(DISTANCE, n = 100)
    arrange(ID, DISTANCE) %>%
    group_by(ID) %>%
    slice_min(DISTANCE, n = 5) %>%
    ungroup()
  
  # Select a unique spouse for each man
  serialID_married <- c()
  
  matched_ids <- matches %>% distinct(ID) %>% pull()
  
  for (id in matched_ids) {
    id_sp <- matches %>%
      filter(ID == id, 
             !ID_SP %in% serialID_married) %>%
      head(1)
    
    if (nrow(id_sp) == 0) {
      serialID_married <- c(serialID_married, NA)
    } else {
      serialID_married <- c(serialID_married, 
                            id_sp %>% pull(ID_SP))
    }
  }
  
  
  # Create married men entries
  married_men <- men_with_marry_prob %>%
    filter(MARRY == 1) %>%
    arrange(ID) %>%
    select(-contains('SP')) %>%
    mutate(ID_SP = serialID_married) %>%
    left_join(
      single_women %>%
        select(-contains('SP')) %>%
        select(ID_SP = ID, 
               PERNUM_SP = PERNUM,
               SSA_ID_SP = SSA_ID, 
               INCWAGE_SP = INCWAGE, 
               LABFORCE_SP = LABFORCE, 
               DISABWRK_SP = DISABWRK,
               RETIRED_SP = RETIRED),
      by = 'ID_SP'
    ) %>%
    mutate(
      MARST = 1,
      SAMPLE_NO = SAMPLE_NO + 1)
  
  # Create married women entries
  married_women <- single_women %>%
    filter(ID %in% serialID_married) %>%
    select(-contains('SP')) %>%
    inner_join(
      married_men %>%
        select(ID = ID_SP,
               ID_SP = ID,
               SERIAL_SP = SERIAL,
               SAMPLE_NO_SP = SAMPLE_NO,
               PERNUM_SP = PERNUM,
               SSA_ID_SP = SSA_ID,
               INCWAGE_SP = INCWAGE,
               LABFORCE_SP = LABFORCE,
               DISABWRK_SP = DISABWRK,
               RETIRED_SP = RETIRED),
      by = 'ID'
    ) %>%
    mutate(MARST = 1,
           RELATE = 201, 
           PERNUM = 2,
           SERIAL = SERIAL_SP,
           SAMPLE_NO = SAMPLE_NO_SP) %>%
    select(-SERIAL_SP, -SAMPLE_NO_SP)
  
  # Singles and married 
  singles_and_married <- single_women %>%
    filter(!ID %in% married_women$ID) %>%
    mutate(MARST = 0) %>%
    add_row(
      married_men %>%
        filter(!is.na(ID_SP)) %>%
        select(-MARRY_PERC, -MARRY, -PR_NOT_MARRY) %>%
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
  # Question: why? 
  max_div_rate <- max(div_rate_by_age$DIV_RATE, na.rm = TRUE)
  
  # Join with married individuals
  divorcees <- dfInitSamp_married %>% 
    filter(MARST == 1) %>%
    inner_join(
      div_rate_by_age,
      by = "AGE") 
  #  %>% mutate(DIV_RATE = max_div_rate)
  
  # ** Divorcees - PUT IN MORE DETAIL
  #  divorced_spouse <- divorcees %>%
  #    mutate(DIVORCE_PROB = runif(divorcees %>%
  #                                  nrow())) %>%
  #    filter(PERNUM == 2) %>%
  #    mutate(DIVORCED = ifelse(DIV_RATE <= DIVORCE_PROB, 0, 1)) %>%
  #    filter(DIVORCED == 1) %>%
  #    mutate(SAMPLE_NO = SAMPLE_NO + 1)
  
  divorced_spouse <- divorcees %>%
    mutate(DIVORCE_PROB = runif(divorcees %>%
                                  nrow())) %>%
    filter(PERNUM == 2) %>%
    mutate(DIVORCED = ifelse(DIV_RATE <= (DIVORCE_PROB/2), 0, 1)) %>%
    filter(DIVORCED == 1) %>%
    mutate(SAMPLE_NO = SAMPLE_NO + 1)
  
  
  divorced_head <- divorcees %>%
    filter(PERNUM == 1,
           ID %in% divorced_spouse$ID_SP) %>%
    select(-DIV_RATE) %>%
    mutate(DIVORCED = 1)
  
  divorced_couples <- divorced_head %>%
    bind_rows(divorced_spouse %>%
                select(-DIVORCE_PROB, -DIV_RATE))
  
  dfInitSamp_divorced <- dfInitSamp_married %>%  
    #    filter(!ID %in% divorced_couples$ID) %>%
    anti_join(divorced_couples,
              by = "ID") |>
    mutate(DIVORCED = 0) %>%
    bind_rows(divorced_couples)
  
  dfInitSamp_divorced
}

## INCOME GROWTH AND LABOR FORCE --------------------

# ** DESCRIBE FUNCTION
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
    group_by(AGE, SEX) |>
    summarise(LFPR = sum(LABFORCE == 1)/n(),
              LFPR_VOL = sum(LABFORCE == 1),
              COUNT = n()) |>
    ungroup() |>
    filter(AGE >= 18)
  
  # Join with SSA LFPR targets and compute differences 
  targets_lfpr <- LFPR %>%
    filter(Year == YEAR_NOW) %>%
    mutate(Value = Value/100) %>%
    rename(YEAR = Year,
           SEX = Sex, 
           AGE = AgeRange, 
           LFPR_SSA = Value
    )
  
  # Determine if LFPR needs to be changed from SSA data
  adjust_lfpr <- actual_lfpr %>%
    left_join(targets_lfpr, by = c("AGE", "SEX")) %>%
    mutate(
      DIFF = LFPR_SSA - LFPR,
      NEED_CHANGE = round(COUNT * DIFF)
    )
  
  # Join NEED_CHANGE values to main data
  dfInitSamp_LAB_FORCE_transition <- dfInitSamp_LAB_FORCE_transition %>%
    left_join(adjust_lfpr %>% 
                select(AGE, SEX, NEED_CHANGE), 
              by = c("AGE", "SEX")) %>%
    group_by(AGE, SEX) %>%
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
    select(-PROB_EXIT_LF_ID, -PROB_EXIT_LF, -END) %>%
    
    # Disabled if randomly generated probability is < 0.001
    mutate(DISABWRK_PERC = runif(dfInitSamp_LAB_FORCE_transition %>%
                                   nrow()),
           DISABWRK = case_when(DISABWRK < 0.001 ~ 1,
                                DISABWRK == 1 ~ 1,
                                DISABWRK != 1 ~ 0))
  #           DISABWRK = ifelse(0.001 > DISABWRK_PERC | DISABWRK == 1, 1, 0))
  
  # Income growth 
  dfInitSamp_inc_growth <- dfInitSamp_LAB_FORCE_transition %>%
    mutate(INCWAGE = INCWAGE)
  
  # For people employed #
  counts <- dfInitSamp_inc_growth %>%
    # INCWAGE == 0: would be used in steps afterwards
    filter(LABFORCE == 1 & INCWAGE == 0) %>%
    group_by(AGE) %>%
    summarise(COUNT = n())
  
  wages <- c()
  # samps is defined later as dfInitSamp
  # Here for the simplicity of testing, I would just apply dfInitSamp to samps
  
  wages_df <- samps %>%
    # Look at the wage distribution for people in the sample dataset
    filter(YEAR == 2007,
           INCWAGE > 0) %>%
    distinct(AGE, INCWAGE) %>%
    arrange(AGE)
  wages_df <- wages_df %>%
    bind_rows(
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
      
      # ** ask about this bit
      # Randomly sample n wages equal to the number of people with zero income
      # Append the sampled values
      # ? Could not slice here, because COUNT in count is larger than n of wage values in wage_df
      slice_sample(n = counts$COUNT[i], replace = TRUE) %>%
      pull(INCWAGE)
    
    wages <- c(wages, sampled_wages)
  }
  
  # Replace zero-income rows with wages
  dfInitSamp_inc_growth <- dfInitSamp_inc_growth %>%
    filter(LABFORCE != 1 | INCWAGE != 0) %>%
    
    # Replace INCWAGE of those with zero income with sampled wages
    bind_rows(
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

t1 <- Sys.time()
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
t2 <- Sys.time()
t2 - t1
# Time difference of 5.256769 mins


write_dataset(samps, path = "checkpoint_data/samps_v1")
samps_arrow <- open_dataset("checkpoint_data/samps_v1")

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
#    MIN_TO = as.numeric(str_extract(Income_Bracket, "(?<=x)\\d+(?=_)")),
    MIN_TO = as.numeric(str_extract(Income_Bracket, "(?<=X)\\d+(?=_)")),
    MAX_TO = as.numeric(str_extract(Income_Bracket, "(?<=_)\\d+"))) |>
    # Handle "Max" separately - for rows where the bracket is "Max"
    # Set MIN_TO as highest previous MAX_TO value
    # Set MAX_TO as Inf (no upper limit)
    # Chaneg: MAX_TO should be within each year
  group_by(Year) |>
  mutate(MIN_TO = ifelse(Income_Bracket == "Max", max(MAX_TO, na.rm = TRUE), MIN_TO),
         MAX_TO = ifelse(Income_Bracket == "Max", Inf, MAX_TO)) |>
  ungroup() |>
  # Change: Delete All the Following part: 
  # MIN_TO shifts down, makes each MIN_TO equal to prev. row's MAX_TO
#  mutate(MIN_TO = lag(MAX_TO),
#         MIN_TO = ifelse(is.na(MIN_TO) | is.infinite(MIN_TO), 0, MIN_TO),
#         MAX_TO = ifelse(is.na(MAX_TO) | is.infinite(MAX_TO), 0, MAX_TO)) %>%
  
  # Join with max income
  left_join(MAX_INCOME %>% rename(Year = REFYEAR),
            by = "Year") |>
  na.omit() |>
  mutate(MAX_TO = ifelse(MAX_TO > MAX_INCOME, MAX_INCOME, MAX_TO),
         MIN_TO = ifelse(MIN_TO > MAX_INCOME, MAX_INCOME, MIN_TO)) %>%
  
  # ** Figure out what this part is doing??
  mutate(EXPECTED = (MIN_TO + MAX_TO)/2,
         EXPECTED = ifelse(Income_Bracket == 'Max', 2.5 * MAX_INCOME, EXPECTED),
         MAX_TO = ifelse(Income_Bracket == 'Max', EXPECTED, MAX_TO)) |>
  mutate(PERC_MAX = ifelse(Income_Bracket == 'Max', MAX_INCOME, EXPECTED))

df_econ_assumptions %>%
  filter(ALTERNATIVE %in% c(0,2)) %>%
  select(REFYEAR, AWI) %>%
  na.omit() %>%
  print(n = 300)



## PROJECTED DISTRIBUTIONS --------------------

# Note: MAX_INCOME in the original dataset is the maximum taxable income eligible for social security tax

# Create projected distribution for years 2023-2100
distribution_maxinc <- DISTRIBUTION %>%
  filter(Year == 2022) %>%
  # Turning income brackets to relative proportions
  mutate(MIN_TO = MIN_TO / 147000,       # Figure out what 147000 is
         MAX_TO = MAX_TO / 147000) %>%   # 147000 is the maximum taxable income for 2022 (ss tax)
  select(-MAX_INCOME) %>%
  mutate(Year = list(2023:2100)) %>%
  unnest(Year) %>%
  arrange(Year) %>%
  # Changes: change inner_join into left_join
  left_join(MAX_INCOME %>% 
               rename(Year = REFYEAR)) %>%
  # Scaling brackets back up
  mutate(MIN_TO = MIN_TO * MAX_INCOME,
         MAX_TO = MAX_TO * MAX_INCOME)

# Adding projected rows back into distribution
DISTRIBUTION <- DISTRIBUTION %>%
#  add_row(distribution_maxinc)
  bind_rows(distribution_maxinc)

# Simulating distribution of incomes for a given year
YEAR <- 2026

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
  # RV: Random Variable
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

