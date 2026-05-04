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
set.seed(2025)
dfFert06_08

normalize_tfr <- function(fert_data, tfr_targets, proj_start) {
  # tfr_targets: data frame with columns YEAR and TARGET_TFR
  
  fert_data %>%
    group_by(YEAR) %>%
    mutate(CURRENT_TFR = sum(FERT_PER_CAPITA)) %>%
    ungroup() %>%
    left_join(tfr_targets, by = "YEAR") %>%
    mutate(
      SCALE = case_when(
        YEAR < proj_start  ~ 1,
        is.na(TARGET_TFR)  ~ 1,
        TRUE               ~ TARGET_TFR / CURRENT_TFR
      ),
      FERT_PER_CAPITA = FERT_PER_CAPITA * SCALE
    ) %>%
    select(-CURRENT_TFR, -TARGET_TFR, -SCALE)
}


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
  id_dead <- dfInitSamps %>%
    inner_join(dfDeaths, by = c("AGE", "SEX")) %>%
    group_by(AGE, SEX) %>%
    group_modify(~ slice_sample(.x, n = .x$DEATHS[1])) %>%
    pull(ID)
  
  dfInitSamps_deaths <- dfInitSamps %>%
    mutate(DEAD = ifelse(ID %in% id_dead, 1, 0)) %>%
    mutate(
      AGE = AGE + 1,
      YEAR = YEAR_NOW
    )
  
  widows2_df1 <- dfInitSamps_deaths %>%
    ungroup() %>%
    # DEAD is household-level and >= 1 means someone in the household has died
    # MARST: Married Status, MARST == 1 -> married, spouse present
    filter(MARST == 1 & DEAD >= 1) %>%
    arrange(SERIAL) %>%
    # select the dead people's ID, household number, death stauts and personal number
    # the dead person's personal number would be the spouse-pernum for their spouse
    select(ID, DEAD, SERIAL,
           PERNUM_SP = PERNUM)
  widows2 <- dfInitSamps_deaths %>%
    # only keep the widowed people who:
    # (1) are in the same household as the dead
    # (2) their spouse-pm is the same as the pernum of the dead
    semi_join(widows2_df1,
              by = c("SERIAL", "PERNUM_SP")) %>%
    # mutate new "widowed" info
    mutate(WIDOWED = 1)
  
  dfInitSamp_deaths_w_widow <- dfInitSamps_deaths %>%
    # delete original record of the new widowed people
    anti_join(widows2,
              by = "ID") %>%
    # add the edited info for the new widowed
    bind_rows(widows2) %>%
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
    # RELATE == 301: Biological Children
    filter(RELATE == 301)
  
  # Prepare new independent household entries for 18 year olds
  new_unit_ids <- sample(new_ids, new18yo %>% nrow())
  
  dfInitSamp_new_units <- dfInitSamp_deaths_w_widow %>%
    #    filter(AGE < 18 | RELATE != 301) %>%
    anti_join(new18yo,
              by = "ID") %>%
    #    add_row(
    bind_rows(
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
    #    inner_join(pernum_babies, by = "SERIAL") %>%
    left_join(pernum_babies, by = "SERIAL") %>%
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

  # Cache SSA year data once (was filtered 3 separate times)
  ssa_year <- df_pop_ssa %>% filter(year == YEAR_NOW)

  singleMen <- dfInitSamp_babies %>%
    filter(SEX == 1, MARST == 0, WIDOWED == 0, between(AGE, 18, 70))

  marriage_gap <- dfInitSamp_babies %>%
    group_by(AGE) %>%
    summarise(PERC_MARRIED = mean(MARST == 1), .groups = "drop") %>%
    left_join(
      ssa_year %>% transmute(AGE = age, MARRY_PERC_ACTUAL = m_mar / m_tot),
      by = "AGE"
    ) %>%
    transmute(AGE, MARRY_PERC = pmax(0, MARRY_PERC_ACTUAL - PERC_MARRIED))

  men_with_marry_prob <- singleMen %>%
    inner_join(marriage_gap, by = "AGE") %>%
    mutate(MARRY = as.integer(runif(n()) <= MARRY_PERC))

  single_women <- dfInitSamp_babies %>%
    filter(SEX == 2, MARST == 0, WIDOWED == 0, between(AGE, 18, 70)) %>%
    left_join(
      ssa_year %>% transmute(AGE = age, MARRY_PERC_FEMALE = f_mar / f_tot),
      by = "AGE"
    )

  men_to_match <- men_with_marry_prob %>%
    filter(MARRY == 1) %>%
    select(ID, AGE, INCWAGE)

  wom_pool <- single_women %>%
    select(ID_SP = ID, AGE_SP = AGE, INCWAGE_SP = INCWAGE, PERC_MARRY = MARRY_PERC_FEMALE)

  # Pre-compute global scaling parameters once (avoids scale() matrix inside mutate)
  all_lgw <- log1p(c(men_to_match$INCWAGE, wom_pool$INCWAGE_SP))
  all_age <- c(men_to_match$AGE, wom_pool$AGE_SP)
  lw_mean <- mean(all_lgw); lw_sd <- sd(all_lgw)
  ag_mean <- mean(all_age); ag_sd <- sd(all_age)

  men_scaled <- men_to_match %>%
    mutate(lgw_sc  = (log1p(INCWAGE) - lw_mean) / lw_sd,
           age_sc  = (AGE - ag_mean) / ag_sd)

  wom_scaled <- wom_pool %>%
    mutate(lgw_sp_sc = (log1p(INCWAGE_SP) - lw_mean) / lw_sd,
           age_sp_sc = (AGE_SP - ag_mean) / ag_sd)

  matches <- men_scaled %>%
    mutate(AGE_WIN = map(AGE, ~(.x - 10L):(.x + 10L))) %>%
    unnest(AGE_WIN) %>%
    inner_join(wom_scaled %>% rename(AGE_WIN = AGE_SP), by = "AGE_WIN") %>%
    rename(AGE_SP = AGE_WIN) %>%
    mutate(DISTANCE = (1 / PERC_MARRY) * sqrt((lgw_sc - lgw_sp_sc)^2 + (age_sc - age_sp_sc)^2)) %>%
    arrange(ID, DISTANCE) %>%
    group_by(ID) %>%
    slice_min(DISTANCE, n = 5, with_ties = FALSE) %>%
    ungroup()

  # Greedy unique matching with O(1) hash-set lookups — replaces O(n²) for loop
  men_seen   <- new.env(hash = TRUE, parent = emptyenv())
  women_seen <- new.env(hash = TRUE, parent = emptyenv())
  n_pairs    <- nrow(matches)
  man_out    <- integer(n_pairs)
  woman_out  <- integer(n_pairs)
  k          <- 0L

  for (i in seq_len(n_pairs)) {
    mk <- as.character(matches$ID[i])
    wk <- as.character(matches$ID_SP[i])
    if (!exists(mk, envir = men_seen,   inherits = FALSE) &&
        !exists(wk, envir = women_seen, inherits = FALSE)) {
      assign(mk, TRUE, envir = men_seen)
      assign(wk, TRUE, envir = women_seen)
      k <- k + 1L
      man_out[k]   <- matches$ID[i]
      woman_out[k] <- matches$ID_SP[i]
    }
  }

  # Build serialID_married aligned to arranged marrying men
  men_marrying     <- men_with_marry_prob %>% filter(MARRY == 1) %>% arrange(ID) %>% pull(ID)
  assignment_map   <- setNames(woman_out[seq_len(k)], man_out[seq_len(k)])
  serialID_married <- as.integer(assignment_map[as.character(men_marrying)])

  married_men <- men_with_marry_prob %>%
    filter(MARRY == 1) %>%
    arrange(ID) %>%
    select(-contains('SP')) %>%
    mutate(ID_SP = serialID_married) %>%
    left_join(
      single_women %>%
        select(-contains('SP')) %>%
        select(ID_SP = ID, PERNUM_SP = PERNUM, SSA_ID_SP = SSA_ID, INCWAGE_SP = INCWAGE,
               LABFORCE_SP = LABFORCE, DISABWRK_SP = DISABWRK, RETIRED_SP = RETIRED),
      by = "ID_SP"
    ) %>%
    mutate(MARST = 1, SAMPLE_NO = SAMPLE_NO + 1)

  married_women <- single_women %>%
    filter(ID %in% serialID_married) %>%
    select(-contains('SP')) %>%
    inner_join(
      married_men %>%
        select(ID = ID_SP, ID_SP = ID, SERIAL_SP = SERIAL, SAMPLE_NO_SP = SAMPLE_NO,
               PERNUM_SP = PERNUM, SSA_ID_SP = SSA_ID, INCWAGE_SP = INCWAGE,
               LABFORCE_SP = LABFORCE, DISABWRK_SP = DISABWRK, RETIRED_SP = RETIRED),
      by = "ID"
    ) %>%
    mutate(MARST = 1, RELATE = 201, PERNUM = 2, SERIAL = SERIAL_SP, SAMPLE_NO = SAMPLE_NO_SP) %>%
    select(-SERIAL_SP, -SAMPLE_NO_SP)

  singles_and_married <- single_women %>%
    filter(!ID %in% married_women$ID) %>%
    mutate(MARST = 0) %>%
    bind_rows(
      married_men %>%
        filter(!is.na(ID_SP)) %>%
        select(-MARRY_PERC, -MARRY, -PR_NOT_MARRY) %>%
        mutate(MARST = 1)
    ) %>%
    bind_rows(married_women %>% select(-MARRY_PERC_FEMALE) %>% mutate(MARST = 1)) %>%
    select(-MARRY_PERC_FEMALE)

  dfInitSamp_married <- dfInitSamp_babies %>%
    filter(!ID %in% singles_and_married$ID) %>%
    bind_rows(singles_and_married)

  # Divorce
  div_rate_by_age <- ssa_year %>%
    group_by(age) %>%
    summarise(DIV_RATE = (m_div + f_div) / total, .groups = "drop") %>%
    rename(AGE = age)

  divorcees <- dfInitSamp_married %>%
    filter(MARST == 1) %>%
    inner_join(div_rate_by_age, by = "AGE")

  divorced_spouse <- divorcees %>%
    mutate(DIVORCE_PROB = runif(n()),
           DIVORCED     = as.integer(DIV_RATE > DIVORCE_PROB)) %>%
    filter(PERNUM == 2, DIVORCED == 1) %>%
    mutate(SAMPLE_NO = SAMPLE_NO + 1)

  divorced_head <- divorcees %>%
    filter(PERNUM == 1, ID %in% divorced_spouse$ID_SP) %>%
    select(-DIV_RATE) %>%
    mutate(DIVORCED = 1)

  divorced_couples <- bind_rows(
    divorced_head,
    divorced_spouse %>% select(-DIVORCE_PROB, -DIV_RATE)
  )

  dfInitSamp_married %>%
    filter(!ID %in% divorced_couples$ID) %>%
    mutate(DIVORCED = 0) %>%
    bind_rows(divorced_couples)
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
      DISABWRK = case_when(DISABWRK < 0.001 ~ 1,
                           DISABWRK == 1 ~ 1,
                           DISABWRK != 1 ~ 0))
  #      DISABWRK = ifelse(0.001 > DISABWRK_PERC | DISABWRK == 1, 1, 0))
  
  
  # Income growth 
  dfInitSamp_inc_growth <- dfInitSamp_LAB_FORCE_transition %>%
    mutate(INCWAGE = INCWAGE)
  
  # For people employed #
  counts <- dfInitSamp_inc_growth %>%
    filter(LABFORCE == 1 & INCWAGE == 0) %>%
    group_by(AGE) %>%
    summarise(COUNT = n())
  
  wages_df <- samps %>%
    # Look at the wage distribution for people in the sample dataset
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
  
  # Sample wages for each age group vectorized
  wages <- counts %>%
    left_join(wages_df %>% mutate(INCWAGE = as.integer(INCWAGE)), by = "AGE") %>%
    group_by(AGE) %>%
    group_modify(~ slice_sample(.x, n = .x$COUNT[1], replace = TRUE)) %>%
    arrange(AGE) %>%
    pull(INCWAGE)
  
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
    # Change: R is sensitive to upper and lower case, and the income bracket start with X but not x
    # MIN_TO: extract the floor value within each income bracket
    MIN_TO = as.numeric(str_extract(Income_Bracket, "(?<=X)\\d+(?=_)")),
    # MAX_TO: extract the cell value within each income bracket
    MAX_TO = as.numeric(str_extract(Income_Bracket, "(?<=_)\\d+"))) %>%
  
  # Handle "Max" separately - for rows where the bracket is "Max"
  # Set MIN_TO as highest previous MAX_TO value
  # Set MAX_TO as Inf (no upper limit)
  # Change: MAX_TO should be within each year
  group_by(Year) %>%
  mutate(MIN_TO = ifelse(Income_Bracket == "Max", max(MAX_TO, na.rm = TRUE), MIN_TO),
         MAX_TO = ifelse(Income_Bracket == "Max", Inf, MAX_TO)) %>%
  ungroup() %>%
  # Change: Delete some lines of codes: 
  # The following parts are not required if the floor value within each income bracket has been extracted
  # with the correct regular expression originally (in 763)
  
  # Join with max income
  left_join(MAX_INCOME %>% rename(Year = REFYEAR),
            by = "Year") %>%
  na.omit() %>%
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

## PROJECTED DISTRIBUTIONS --------------------

# Create projected distribution for years 2023-2100
distribution_maxinc <- DISTRIBUTION %>%
  filter(Year == 2022) %>%
  # Turning income brackets to relative proportions
  mutate(MIN_TO = MIN_TO / 147000,       # Figure out what 147000 is
         MAX_TO = MAX_TO / 147000) %>%   # 147000 is the maximum taxable income for 2022 (FICA)
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
  # RV: Random Variable
  for(i in 1:N) {
    if(i < N) {
      MIN <- DISTRIBUTION$MIN_TO[i]
      MAX <- DISTRIBUTION$MAX_TO[i]
      # PERC: the percentage of people in one income bracket (from df income_dist)
      # Generation 25000*(the pct share falling in this income bracket) RV between MIN and MAX
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

# Load the samps now
# samps_arrow <- open_dataset("checkpoint_data/samps_v1")
# samps <- samps_arrow |>
#   collect()

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
  # Transform the INC(income distribution) into a dataframe
  target_dist <- data.frame(ID = 1:length(INC),
                            INCWAGE = INC) %>%
    # Make sure the incwage is in asending order (required for cumulative income share)
    arrange(INCWAGE) %>%
    
    # Cap incomes above MAX
    mutate(
      INCWAGE_NEW = ifelse(INCWAGE > MAX, MAX, INCWAGE),
      
      # Total income retained after capping
      PERC_TOTAL = sum(INCWAGE_NEW) / sum(INCWAGE), 
      
      # Cumulative income share
      PERC = cumsum(INCWAGE) / sum(INCWAGE), 
      
      # Rank-based percentile position
      # The percentile here is determined by your ranking postion among all the people
      # The percentile is determined by the NUMBER POSITION you ranked (E.g., 1st, 10th, 51st),
      # instead of your absolute wage income (incwage) value
      percentile = (row_number() - 0.5) / n()
    )
  
  # Filter for target year, compute percentile for everyone w non-zero income
  future_dist <- samps %>%
    filter(
      YEAR == year, 
      INCWAGE != 0
    ) %>%
    # set the income wage in asending order
    arrange(INCWAGE) %>%
    # Generate the percentile for ranking (number position, instead of income value)
    mutate(percentile = (row_number() - 0.5) / n())
  
  
  # Match percentiles between distributions 
  suppressWarnings(future_dist <- future_dist %>%
                     mutate(
                       # percentile: rank number position percentile
                       # reference input
                       matched_PERC = approx(
                         x = target_dist$percentile,
                         # PERC: Cumulative income share percentage
                         # reference output (what p maps to)
                         y = target_dist$PERC,
                         # Use the percentile for rank position in future_dist for projection
                         # percentile: the input we want to have in this position
                         # always input -> output: input is percentile(rank), and output is PERC(CDF)
                         xout = percentile,
                         # If a percentile falls outside the range, use the nearest boundary value (extrapolation)
                         rule = 2
                         # Extracts just the interpolated y-values from the result
                       )$y
                     )
  )
  # perc_to_income: use the relationship of PERC - incwage in target_dist, to generate the incwage
  # based on given PERC (CDF of incwage) 
  # projection of target_dist onto other dataframe, in a way of function
  
  suppressWarnings(perc_to_income <- approxfun(
    # x: input var/independent var --- PERC
    x = target_dist$PERC,
    y = target_dist$INCWAGE,
    rule = 2
  ))
  
  # Notes: Interpretation on approx
  #  approx(
  # x = [independent/input variable from reference],
  # y = [dependent/output variable from reference],
  # xout = [new input values you want to map]
  # )$y
  
  # approxfun(): create a function for approx() that can be repeated
  # --- create a project relation using given input (x) and given output(y)
  # A function of new input: approxfun(x), treat x as input and therefore generate the output
  
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
    # add_row(future_dist_adjusted)
    bind_rows(future_dist_adjusted)
}

## MASTER SIMULATION FUNCTION --------------------

# Takes the initial RDS population dataframe and runs the full pipeline:
# demographic changes, births, marriages/divorces, income/LF, income distribution
# matching, and weighting. Returns samps_w_weights.
#
# Requires init_sim_data.RData to be loaded (provides cohort_life_tables,
# df_pop_ssa, transition, LFPR, df_econ_assumptions, income_dist, MAX_INCOME).
#
# Side effects: overwrites globals samps, DISTRIBUTION, and optionally dfFert06_08.

run_simulation <- function(
    init_data,
    tfr_targets  = NULL,
    proj_start   = 2010,
    sim_years    = 2008:2100
) {

  # Optionally normalize fertility for projection years
  if (!is.null(tfr_targets)) {
    dfFert06_08 <<- normalize_tfr(dfFert06_08, tfr_targets, proj_start)
  }

  # samps needed inside makeIncome_and_LF_and_Disability for 2007 wage reference
  samps <<- init_data

  # Simulation loop
  results <- vector("list", length(sim_years))
  current_year_data <- init_data

  for (i in seq_along(sim_years)) {
    year <- sim_years[i]
    print(year)
    dfInitSamp_new_units  <- makeDemographic_Project(year, current_year_data)
    dfInitSamp_babies     <- makeBabies(year, dfInitSamp_new_units)
    dfInitSamp_divorced   <- makeMarriages_Divorced(year, dfInitSamp_babies)
    current_year_data     <- makeIncome_and_LF_and_Disability(year, dfInitSamp_divorced)
    results[[i]]          <- current_year_data
  }

  samps <<- bind_rows(init_data, results)

  # Build income distribution (historic + projected)
  DISTRIBUTION <<- income_dist %>%
    pivot_longer(cols = -c(Year, Total), names_to = "Income_Bracket", values_to = "Count") %>%
    na.omit() %>%
    mutate(
      PERC   = Count / Total,
      MIN_TO = as.numeric(str_extract(Income_Bracket, "(?<=X)\\d+(?=_)")),
      MAX_TO = as.numeric(str_extract(Income_Bracket, "(?<=_)\\d+"))
    ) %>%
    group_by(Year) %>%
    mutate(
      MIN_TO = ifelse(Income_Bracket == "Max", max(MAX_TO, na.rm = TRUE), MIN_TO),
      MAX_TO = ifelse(Income_Bracket == "Max", Inf, MAX_TO)
    ) %>%
    ungroup() %>%
    left_join(MAX_INCOME %>% rename(Year = REFYEAR), by = "Year") %>%
    na.omit() %>%
    mutate(
      MAX_TO   = ifelse(MAX_TO > MAX_INCOME, MAX_INCOME, MAX_TO),
      MIN_TO   = ifelse(MIN_TO > MAX_INCOME, MAX_INCOME, MIN_TO),
      EXPECTED = (MIN_TO + MAX_TO) / 2,
      EXPECTED = ifelse(Income_Bracket == "Max", 2.5 * MAX_INCOME, EXPECTED),
      PERC_MAX = ifelse(Income_Bracket == "Max", MAX_INCOME, EXPECTED),
      MAX_TO   = ifelse(Income_Bracket == "Max", EXPECTED, MAX_TO)
    ) %>%
    bind_rows(
      (.) %>%
        filter(Year == 2022) %>%
        mutate(MIN_TO = MIN_TO / 147000, MAX_TO = MAX_TO / 147000) %>%
        select(-MAX_INCOME) %>%
        mutate(Year = list(2023:max(sim_years))) %>%
        unnest(Year) %>%
        arrange(Year) %>%
        left_join(MAX_INCOME %>% rename(Year = REFYEAR)) %>%
        mutate(MIN_TO = MIN_TO * MAX_INCOME, MAX_TO = MAX_TO * MAX_INCOME)
    )

  # Match income distributions year by year
  target <- matchDist(min(sim_years) - 1)  # base year (2007)
  for (yr in sim_years) {
    target <- bind_rows(target, matchDist(yr))
  }

  # Scale to AWI
  filtered_econ <- df_econ_assumptions %>%
    filter(ALTERNATIVE %in% c(0, 2)) %>%
    select(YEAR = REFYEAR, AWI) %>%
    na.omit()

  target_adjusted <- target %>%
    group_by(YEAR) %>%
    summarise(MEAN = mean(INCWAGE)) %>%
    inner_join(filtered_econ, by = "YEAR") %>%
    mutate(AWI_ADJUST = AWI / 40405.48) %>%
    select(YEAR, AWI_ADJUST)

  samps <<- target %>%
    inner_join(target_adjusted, by = "YEAR") %>%
    select(-AWI_ADJUST)

  # Build weights
  pop_ssa_adjusted_1 <- df_pop_ssa %>%
    select(year, age, m_mar) %>%
    rename(YEAR = year, AGE = age, TOTAL_SSA = m_mar) %>%
    mutate(MARST = 1, SEX = 1)

  pop_ssa_adjusted_2 <- df_pop_ssa %>%
    select(year, age, f_mar) %>%
    rename(YEAR = year, AGE = age, TOTAL_SSA = f_mar) %>%
    mutate(MARST = 1, SEX = 2)

  weights <- df_pop_ssa %>%
    mutate(m_not_married = m_tot - m_mar, f_not_married = f_tot - f_mar) %>%
    select(year, age, m_not_married, f_not_married) %>%
    pivot_longer(cols = c("m_not_married", "f_not_married"), names_to = "marst_sex", values_to = "not_married") %>%
    mutate(SEX = case_when(marst_sex == "m_not_married" ~ 1, marst_sex == "f_not_married" ~ 2)) %>%
    rename(YEAR = year, AGE = age, TOTAL_SSA = not_married) %>%
    mutate(MARST = 0) %>%
    select(-marst_sex) %>%
    bind_rows(pop_ssa_adjusted_1, pop_ssa_adjusted_2) %>%
    left_join(
      df_pop_ssa %>% group_by(year) %>% summarise(TOTAL_POP = sum(total)) %>% rename(YEAR = year),
      by = "YEAR"
    ) %>%
    mutate(PERC_SSA = TOTAL_SSA / TOTAL_POP) %>%
    inner_join(samps %>% group_by(YEAR) %>% summarise(PEOPLE = n()) %>% ungroup(), by = "YEAR") %>%
    mutate(SAMPS_POP = PERC_SSA * PEOPLE) %>%
    inner_join(
      samps %>% filter(AGE < 100) %>% group_by(YEAR, AGE, MARST, SEX) %>% summarise(COUNT = n()) %>% ungroup(),
      by = c("YEAR", "AGE", "MARST", "SEX")
    ) %>%
    mutate(WEIGHTS = SAMPS_POP / COUNT)

  samps %>%
    filter(AGE < 100) %>%
    left_join(weights, by = c("YEAR", "AGE", "SEX", "MARST")) %>%
    left_join(
      df_econ_assumptions %>%
        filter(ALTERNATIVE %in% c(0, 2)) %>%
        select(YEAR = REFYEAR, AWI) %>%
        na.omit(),
      by = "YEAR"
    ) %>%
    select(-TOTAL_SSA, -TOTAL_POP, -DISABWRK_PERC, -PERC_SSA, -PEOPLE, -SAMPS_POP, -COUNT, -AWI)
}
