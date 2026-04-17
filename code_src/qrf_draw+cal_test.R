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
library(grf)

set.seed(2025)
## LOAD DATA --------------------
load("init_sim_data.RData")
potential_ids <- sprintf("%08d", sample(1000000:9999999, 5000000))  # 10,000 candidates

awi_table <- df_econ_assumptions |>
  select(YEAR = REFYEAR,
         ALTERNATIVE, AWI, CPI_ADJ) |>
  distinct() |>
  arrange(YEAR) |>
  filter(ALTERNATIVE %in% c(0,2)) |>
  filter(is.na(AWI) == FALSE)

get_awi <- function(year, awi_table) {
  awi_val <- awi_table %>%
    filter(YEAR == year) %>%
    pull(AWI)
  
  if (length(awi_val) == 0) {
    stop(paste("Missing AWI for year:", year))
  }
  
  awi_val[1]
}

dfInitSamps <- dfInitSamps |>
  mutate(
    birth_year = YEAR - AGE,
    cohort_5yr = (floor(birth_year / 5) * 5),
    U_DRAW = runif(n(), min = 0.001, max = 0.999)
  )

qrf_model <- readRDS(file = "C:/Users/YundiHou/OneDrive - Cato Institute/Desktop/ss_is/revision/initial/J360066/qrf_model3.rds")
colnames(qrf_model$X.orig)
# [1] "age"           "age_sq"        "sex1"          "income_real_a"

predict_real_income_qrf <- function(newdata, qrf_model, mode = "draw", u_vec = NULL) {
  # mode: "mean", "median", "draw"
  
  if (mode == "median") {
    pred <- predict(qrf_model, newdata = newdata, quantiles = 0.5)$predictions
    return(as.numeric(pred))
  }
  
  if (mode == "draw") {
    if (is.null(u_vec)) {
      stop("u_vec must be provided when mode = 'draw'")
    }
    if (length(u_vec) != nrow(newdata)) {
      stop("length(u_vec) must equal nrow(newdata)")
    }
    
    u_vec <- pmin(pmax(u_vec, 0.001), 0.999)
    pred <- predict(qrf_model, newdata = newdata, quantiles = u_vec)$predictions
    return(as.numeric(pred))
  }
  
  if (mode == "mean") {
    pred <- predict(qrf_model, newdata = newdata)$predictions
    return(as.numeric(pred))
  }
  
  stop("mode must be one of: 'mean', 'median', 'draw'")
}

update_u_draw <- function(u_prev, rho = 0.9) {
  shock <- runif(length(u_prev))
  u_new <- rho * u_prev + (1 - rho) * shock
  u_new <- pmin(pmax(u_new, 0.001), 0.999)
  u_new
}

initialize_u_draw <- function(n_new) {
  runif(n_new, min = 0.001, max = 0.999)
}

calibrate_income_to_awi <- function(df, awi_target) {
  worker_idx <- which(df$LABFORCE == 1 & df$INCWAGE > 0 & is.finite(df$INCWAGE))
  
  if (length(worker_idx) == 0) {
    warning(paste("No positive workers to calibrate in year", unique(df$YEAR)))
    df$AWI_SCALE_FACTOR <- 1
    return(df)
  }
  
  mean_now <- mean(df$INCWAGE[worker_idx], na.rm = TRUE)
  
  if (!is.finite(mean_now) || mean_now <= 0) {
    warning(paste("Invalid mean income before calibration in year", unique(df$YEAR)))
    df$AWI_SCALE_FACTOR <- 1
    return(df)
  }
  
  scale_factor <- awi_target / mean_now
  
  df$INCWAGE[worker_idx] <- df$INCWAGE[worker_idx] * scale_factor
  df$INCWAGE <- round(df$INCWAGE, 2)
  df$AWI_SCALE_FACTOR <- scale_factor
  
  df
}

build_qrf_features <- function(df, qrf_model) {
  df <- df %>%
    mutate(
      sex = factor(sex, levels = c(0, 1))
    )
  
  x_formula <- ~ age + age_sq + sex + income_real_a
  
  mm <- model.matrix(x_formula, data = df)[, -1, drop = FALSE]
  
  trained_colnames <- colnames(qrf_model$X.orig)
  
  if (!all(trained_colnames %in% colnames(mm))) {
    missing_cols <- setdiff(trained_colnames, colnames(mm))
    stop(
      paste(
        "Missing columns in prediction matrix:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  mm <- mm[, trained_colnames, drop = FALSE]
  
  if (!identical(colnames(mm), trained_colnames)) {
    stop("Prediction matrix columns do not exactly match training matrix columns.")
  }
  
  mm
}


draw_entry_real_income <- function(entry_df, donor_df, awi_prev) {
  if (nrow(entry_df) == 0) {
    return(numeric(0))
  }
  
  if (!is.numeric(awi_prev) || length(awi_prev) != 1 || !is.finite(awi_prev) || awi_prev <= 0) {
    stop("awi_prev must be a single positive finite number.")
  }
  
  donor_df_clean <- donor_df %>%
    filter(
      LABFORCE == 1,
      INCWAGE > 0,
      is.finite(INCWAGE)
    ) %>%
    mutate(
      donor_real_income = INCWAGE / awi_prev
    ) %>%
    filter(
      is.finite(donor_real_income),
      donor_real_income > 0
    )
  
  if (nrow(donor_df_clean) == 0) {
    stop("No valid donors available for entrant initialization.")
  }
  
  draws <- numeric(nrow(entry_df))
  
  for (i in seq_len(nrow(entry_df))) {
    age_i <- entry_df$age[i]
    sex_i <- entry_df$sex[i]
    
    # 1. age + sex
    pool <- donor_df_clean %>%
      filter(AGE == age_i, SEX == sex_i)
    
    # 2. age
    if (nrow(pool) == 0) {
      pool <- donor_df_clean %>%
        filter(AGE == age_i)
    }
    
    # 3. sex
    if (nrow(pool) == 0) {
      pool <- donor_df_clean %>%
        filter(SEX == sex_i)
    }
    
    if (nrow(pool) == 0) {
      stop(
        paste0(
          "No donor pool found for entrant row ", i,
          " (age = ", age_i, ", sex = ", sex_i, ")."
        )
      )
    }
    
    draws[i] <- sample(pool$donor_real_income, size = 1, replace = TRUE)
  }
  
  draws
}

makeIncome_LF_Disability_QRF <- function(YEAR_NOW,
                                         dfInitSamp_divorced,
                                         transition,
                                         LFPR,
                                         awi_table,
                                         qrf_model,
                                         qrf_mode = "draw",
                                         rho_u = 0.9) {
  
  # 1. Labor force transition
  df_now <- dfInitSamp_divorced %>%
    left_join(
      transition %>%
        filter(YEAR == YEAR_NOW) %>%
        select(-YEAR),
      by = c("AGE", "SEX", "LABFORCE")
    ) %>%
    mutate(
      PROB_EXIT_LF = ifelse(is.na(PROB_EXIT_LF), 1, PROB_EXIT_LF),
      PROB_EXIT_LF_ID = runif(n()),
      PROB_EXIT_LF = ifelse(RETIRED == 1, 2, PROB_EXIT_LF),
      LABFORCE = ifelse(PROB_EXIT_LF_ID < PROB_EXIT_LF, 0, 1),
      RETIRED = ifelse(RETIRED == 0 & LABFORCE == 0 & AGE >= 63, 1, RETIRED)
    )
  

  # 2. Align LFPR to SSA target
  actual_lfpr <- df_now %>%
    group_by(AGE, SEX) %>%
    summarise(
      LFPR = mean(LABFORCE == 1),
      LFPR_VOL = sum(LABFORCE == 1),
      COUNT = n(),
      .groups = "drop"
    ) %>%
    filter(AGE >= 18)
  
  targets_lfpr <- LFPR %>%
    filter(Year == YEAR_NOW) %>%
    mutate(Value = Value / 100) %>%
    rename(
      YEAR = Year,
      SEX = Sex,
      AGE = AgeRange,
      LFPR_SSA = Value
    )
  
  adjust_lfpr <- actual_lfpr %>%
    left_join(targets_lfpr, by = c("AGE", "SEX")) %>%
    mutate(
      DIFF = LFPR_SSA - LFPR,
      NEED_CHANGE = round(COUNT * DIFF)
    )
  
  df_now <- df_now %>%
    left_join(
      adjust_lfpr %>% select(AGE, SEX, NEED_CHANGE),
      by = c("AGE", "SEX")
    ) %>%
    group_by(AGE, SEX) %>%
    group_modify(~{
      df <- .x
      need_change <- unique(df$NEED_CHANGE)
      
      if (length(need_change) == 0 || is.na(need_change) || need_change == 0) {
        return(df)
      }
      
      if (need_change > 0) {
        eligible <- which(df$LABFORCE == 0)
        if (length(eligible) > 0) {
          flip_n <- min(length(eligible), need_change)
          flip_ids <- sample(eligible, flip_n)
          df$LABFORCE[flip_ids] <- 1
        }
      }
      
      if (need_change < 0) {
        eligible <- which(df$LABFORCE == 1)
        if (length(eligible) > 0) {
          flip_n <- min(length(eligible), abs(need_change))
          flip_ids <- sample(eligible, flip_n)
          df$LABFORCE[flip_ids] <- 0
        }
      }
      
      df
    }) %>%
    ungroup() %>%
    select(-NEED_CHANGE)
  

  # 3. Disability 
  df_now <- df_now %>%
    select(-any_of(c("PROB_EXIT_LF_ID", "PROB_EXIT_LF", "END"))) %>%
    mutate(
      DISABWRK_PERC = runif(n()),
      DISABWRK = case_when(
        DISABWRK == 1 ~ 1,
        DISABWRK_PERC < 0.001 ~ 1,
        TRUE ~ 0
      )
    )
  

  # 4. Prepare AWI and lag income
  awi_prev <- awi_table %>%
    filter(YEAR == YEAR_NOW - 1) %>%
    pull(AWI)
  
  awi_now <- awi_table %>%
    filter(YEAR == YEAR_NOW) %>%
    pull(AWI)
  
  if (length(awi_prev) == 0) stop(paste("Missing AWI for year", YEAR_NOW - 1))
  if (length(awi_now) == 0) stop(paste("Missing AWI for year", YEAR_NOW))
  
  awi_prev <- awi_prev[1]
  awi_now  <- awi_now[1]
  
  if (!"U_DRAW" %in% names(df_now)) {
    df_now$U_DRAW <- NA_real_
  }
  
  df_qrf <- df_now %>%
    mutate(
      INCWAGE_LAG = INCWAGE,
      income_real_a = ifelse(awi_prev > 0, INCWAGE_LAG / awi_prev, 0),
      income_real_a = ifelse(is.na(income_real_a), 0, income_real_a),
      income_real_a = pmax(income_real_a, 0),
      age = AGE,
      age_sq = AGE^2
    )
  
  df_qrf <- df_qrf %>%
    mutate(
      ENTRY = ifelse(LABFORCE == 1 & INCWAGE_LAG == 0, 1, 0),
      income_real_pred = 0
    )
  
  # 5. Continuing workers -> QRF with persistent U_DRAW
  continuing_idx <- which(df_qrf$LABFORCE == 1 & df_qrf$INCWAGE_LAG > 0)
  
  if (length(continuing_idx) > 0) {
    u_prev <- df_qrf$U_DRAW[continuing_idx]
    
    missing_u <- is.na(u_prev)
    if (any(missing_u)) {
      u_prev[missing_u] <- initialize_u_draw(sum(missing_u))
    }
    
    u_now <- update_u_draw(u_prev, rho = rho_u)
    df_qrf$U_DRAW[continuing_idx] <- u_now
    
    apply_df_workers <- df_qrf %>%
      slice(continuing_idx) %>%
      mutate(
        sex = if_else(SEX == 1, 0, 1),
        sex = factor(sex, levels = c(0, 1))
      ) %>%
      transmute(
        age = age,
        age_sq = age_sq,
        sex = sex,
        income_real_a = income_real_a
      )
    
    newdata_workers <- build_qrf_features(
      df = apply_df_workers,
      qrf_model = qrf_model
    )
    
    pred_real <- predict_real_income_qrf(
      newdata = newdata_workers,
      qrf_model = qrf_model,
      mode = qrf_mode,
      u_vec = if (qrf_mode == "draw") df_qrf$U_DRAW[continuing_idx] else NULL
    )
    
    pred_real <- pmax(pred_real, 0)
    df_qrf$income_real_pred[continuing_idx] <- pred_real
  }
  
  
  # 6. Entrants -> donor draw + initialize U_DRAW
  entrant_idx <- which(df_qrf$LABFORCE == 1 & df_qrf$INCWAGE_LAG == 0)
  
  if (length(entrant_idx) > 0) {
    entry_df <- df_qrf %>%
      slice(entrant_idx) %>%
      transmute(
        age = age,
        sex = SEX
      )
    
    donor_real_draws <- draw_entry_real_income(
      entry_df = entry_df,
      donor_df = df_qrf,
      awi_prev = awi_prev
    )
    
    df_qrf$income_real_pred[entrant_idx] <- donor_real_draws
    
    df_qrf$U_DRAW[entrant_idx] <- initialize_u_draw(length(entrant_idx))
  }
  

  # 7. Convert real income into nominal income
  df_qrf <- df_qrf %>%
    mutate(
      INCWAGE = case_when(
        LABFORCE == 0 ~ 0,
        LABFORCE == 1 ~ income_real_pred * awi_now,
        TRUE ~ 0
      ),
      INCWAGE = ifelse(is.na(INCWAGE), 0, INCWAGE)
    )
  
  # 8. AWI calibration
  df_qrf <- calibrate_income_to_awi(df_qrf, awi_target = awi_now)
  
  df_qrf <- df_qrf %>%
    mutate(
      INCWAGE = round(INCWAGE, 2)
    )
  
  # 9. Return
  df_qrf %>%
    select(
      -INCWAGE_LAG,
      -income_real_a,
      -income_real_pred,
      -age_sq,
      -ENTRY
    )
}

# === Revise on Marriage ====
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
    left_join(
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
    left_join(
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
    # Changes: 
    # (1) Take log of Income (Income tends to have a long-tail distribution)
    # (2) Standardize/Normalize both the age and the log income
    # Reasons for the change:
    # 1. To avoid to many "0 income" to "0 income" pairs;
    # 2. To avoid one certain variable, age or income, affects the match too much,
    # so that the other variable does not matter much
    mutate(lgwage = log1p(INCWAGE),
           lgwage_sp = log1p(INCWAGE_SP)) %>%
    mutate(lgwage_sc = scale(lgwage),
           lgwage_sp_sc = scale(lgwage_sp),
           age_sc = scale(AGE),
           age_sp_sc = scale(AGE_SP)) %>%
    mutate(
      DISTANCE = (1/PERC_MARRY) * sqrt(((lgwage - lgwage_sp)^2)+ (age_sc - age_sp_sc)^2)
    ) %>%
    filter(DISTANCE < 999999999) %>%
    #    slice_min(DISTANCE, n = 100)
    arrange(ID, DISTANCE) %>%
    group_by(ID) %>%
    slice_min(DISTANCE, n = 5) %>%
    ungroup()
  
  # Select a unique spouse for each man
  marrying_men <- men_with_marry_prob %>%
    filter(MARRY == 1) %>%
    arrange(ID)
  
  used_wives <- character(0)
  match_rows <- vector("list", nrow(marrying_men))
  
  for (i in seq_len(nrow(marrying_men))) {
    man_id <- marrying_men$ID[i]
    
    candidate <- matches %>%
      filter(
        ID == man_id,
        !ID_SP %in% used_wives
      ) %>%
      slice(1)
    
    if (nrow(candidate) == 0) {
      match_rows[[i]] <- tibble(
        ID = man_id,
        ID_SP = NA_character_
      )
    } else {
      wife_id <- candidate$ID_SP[1]
      used_wives <- c(used_wives, wife_id)
      
      match_rows[[i]] <- tibble(
        ID = man_id,
        ID_SP = wife_id
      )
    }
  }
  
  match_table <- bind_rows(match_rows)
  
  stopifnot(nrow(match_table) == nrow(marrying_men))
  stopifnot(identical(match_table$ID, marrying_men$ID))
  
  serialID_married <- match_table$ID_SP
  
  # Create married men entries
  married_men <- marrying_men %>%
    select(-contains("SP")) %>%
    left_join(match_table, by = "ID") %>%
    left_join(
      single_women %>%
        select(-contains("SP")) %>%
        select(
          ID_SP = ID,
          PERNUM_SP = PERNUM,
          SSA_ID_SP = SSA_ID,
          INCWAGE_SP = INCWAGE,
          LABFORCE_SP = LABFORCE,
          DISABWRK_SP = DISABWRK,
          RETIRED_SP = RETIRED
        ),
      by = "ID_SP"
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
    #    add_row(
    bind_rows(
      married_men %>%
        filter(!is.na(ID_SP)) %>%
        select(
          -MARRY_PERC, 
          -MARRY, 
          -PR_NOT_MARRY
        ) %>%
        mutate(MARST = 1)) %>%
    bind_rows(
      married_women %>%
        select(-MARRY_PERC_FEMALE) %>%
        mutate(MARST = 1)) %>%
    select(-MARRY_PERC_FEMALE)
  
  dfInitSamp_married <- dfInitSamp_babies %>%
    filter(!ID %in% singles_and_married$ID) %>%
    bind_rows(
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
    ) 
  # Divorce rate should be unique to each age
  # Therefore, there is no need to assign the latest divorce rate among all ages to all married population
  # mutate(DIV_RATE = max_div_rate)
  
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

## Demographics & New-born Babies -------

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


## ECONOMIC & POPULATION CHANGES BY YEAR --------------------

samps <- dfInitSamps
results_list <- list()

# Loop through each year 2008-2100 and apply functions
prev_year <- dfInitSamps

# ===== Application =====
t1 <- Sys.time()

results_list <- vector("list", length = 2022 - 2008 + 1)
names(results_list) <- as.character(2008:2022)

for (i in 2008:2022) {
  print(i)
  
  # Mortality and new households for current year
  dfInitSamp_new_units <- makeDemographic_Project(i, prev_year)
  
  # Fertility for current year
  dfInitSamp_babies <- makeBabies(i, dfInitSamp_new_units)
  
  # Marriages and divorces for current year
  dfInitSamp_divorced <- makeMarriages_Divorced(i, dfInitSamp_babies)
  
  # Income, LF, Disability changes for current year
  dfInitSamp_econ_growth <- makeIncome_LF_Disability_QRF(
    YEAR_NOW = i,
    dfInitSamp_divorced = dfInitSamp_divorced,
    transition = transition,
    LFPR = LFPR,
    awi_table = awi_table,
    qrf_model = qrf_model,
    qrf_mode = "draw"
  )
  
  # Save current simulated year
  results_list[[as.character(i)]] <- dfInitSamp_econ_growth
  
  # Next loop only needs this year's output
  prev_year <- dfInitSamp_econ_growth
  
  # Clean up large temporary objects
  rm(dfInitSamp_new_units, dfInitSamp_babies, dfInitSamp_divorced, dfInitSamp_econ_growth)
  gc()
}

# Combine at the end only once
samps_simulated <- bind_rows(results_list)

# If you want full data including base/history
samps_all <- bind_rows(dfInitSamps, samps_simulated)

t2 <- Sys.time()
t2 - t1

# === Testing for Performance ====
# Use the year_q to see the distribution of incomes within each year
# If year_q looks weird, then the income_LFP_disability function will need revision

year_q <- list()
for (i in seq(2008, 2022, 1)) {
  df3 <- samps_all |>
    filter(YEAR == i)|>
    filter(INCWAGE > 0)
  quantilev <- quantile(df3$INCWAGE, probs = c(0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9, 0.95))
  
  year_q[[as.character(i)]] <- quantilev
}

# === Weight --------

samps_all <- open_dataset("test1_checkpoint_data/samps_all_qrf3_v5_draw") |>
  collect()

pop_ssa_adjusted_1 <- df_pop_ssa %>%
  select(year, age, m_mar) %>%
  # TOTAL_SSA: the total population within this category 
  rename(YEAR = year, AGE = age, TOTAL_SSA = m_mar) %>%
  # MARST == 1: married; SEX == 1: men
  mutate(MARST = 1, SEX = 1)

# pop_ssa_adjusted_2: married women
pop_ssa_adjusted_2 <- df_pop_ssa %>%
  select(year, age, f_mar) %>%
  rename(YEAR = year, AGE = age, TOTAL_SSA = f_mar) %>%
  # MARST == 1: married; SEX == 2: women
  mutate(MARST = 1, SEX = 2)

pop_ssa_grouped <- df_pop_ssa %>%
  group_by(year) %>%
  # TOTAL_POP is the total number of population in that year
  summarise(TOTAL_POP = sum(total)) %>%
  rename(YEAR = year) %>%
  ungroup()

weights <- df_pop_ssa %>%
  mutate(m_not_married = m_tot - m_mar,
         f_not_married = f_tot - f_mar) %>%
  select(year, age, m_not_married, f_not_married) %>%
  # Transform into long datatset with "Married Status" & "Gender" as 2 seperate variable
  pivot_longer(
    cols = c("m_not_married", "f_not_married"),
    names_to = "marst_sex",
    values_to = "not_married"
  ) %>%
  mutate(SEX = case_when(
    marst_sex == "m_not_married" ~ 1,
    marst_sex == "f_not_married" ~ 2
  )) %>%
  # Data for the single/not married men and women
  rename(YEAR = year, 
         AGE = age, 
         # TOTAL_SSA = m_not_married) %>%
         TOTAL_SSA = not_married) %>%
  mutate(MARST = 0,
         # SEX = 1
  ) %>%
  select(-marst_sex) %>%
  # Append the data for married men & women
  bind_rows(pop_ssa_adjusted_1,
            pop_ssa_adjusted_2) %>%
  left_join(df_pop_ssa %>%
              group_by(year) %>%
              # TOTAL_POP is the total number of population in that year
              summarise(TOTAL_POP = sum(total)) %>%
              rename(YEAR = year) %>%
              ungroup(),
            by = "YEAR") %>%
  # PERC_SSA: the percentage of pop in each (marriage, gender, age) group, from SSA data
  mutate(PERC_SSA = TOTAL_SSA / TOTAL_POP) %>%
  # samps_grouped: year and number of people (no age filter) in each year in the sample
  inner_join(samps_grouped <- samps_all %>%
               group_by(YEAR) %>%
               # The number of people in each year
               summarise(PEOPLE = n()) %>%
               ungroup()) %>%
  # SAMPS_POP: the people of each (marriage, gender, age) group that should have been in the samps
  # SAMPS_POP is the projected population based on PERC_SSA and the total number of people in the samps
  mutate(SAMPS_POP = PERC_SSA * PEOPLE) %>%
  # samps_filter: the actual count of # of people in each (marriage, gender, age) group in the samps
  # Notes: samps_filtered has FILTERD OUT AGE >= 100, so the total population for each year here
  # WOULD NOT equals the TOTAL_POP
  inner_join(samps_all %>%
               filter(AGE < 100) %>%
               group_by(YEAR, AGE, MARST, SEX) %>%
               summarise(COUNT = n()) %>%
               ungroup()) %>%
  # weights: projected value / actual value
  mutate(WEIGHTS = SAMPS_POP/COUNT)

samps_w_weights <- samps_all %>%
  filter(AGE < 100) %>%
  # inner_join(weights, 
  #            by = c('YEAR', 'AGE', 'SEX', 'MARST')) %>%
  left_join(weights, 
            by = c('YEAR', 'AGE', 'SEX', 'MARST')) %>%
  group_by(YEAR) %>%
  mutate(MEAN_INCOME = mean(INCWAGE)) %>%
  ungroup() %>%
  left_join(
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(YEAR = REFYEAR,
             AWI) %>%
      na.omit(),
    by = "YEAR"
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

# === Test --------
# AWI data is in the df_econ_assumption

sampletest_df1 <- samps_w_weights %>%
  select(ID, YEAR, SERIAL, INCWAGE, WEIGHTS)

awi_df <- df_econ_assumptions %>%
  filter(ALTERNATIVE %in% c(0,2)) %>%
  select(REFYEAR, AWI) %>%
  na.omit() %>%
  rename(YEAR = REFYEAR)

awi_sample <- sampletest_df1 %>%
  select(ID,YEAR, INCWAGE, WEIGHTS) %>%
  distinct() %>%
  filter(INCWAGE > 0) %>% 
  group_by(YEAR) %>%
  summarise(avgwage = weighted.mean(INCWAGE, WEIGHTS)) %>%
  ungroup() 

awi_test_df1 <- awi_df %>%
  inner_join(awi_sample,
             by = "YEAR") %>%
  mutate(awi_diff = (avgwage - AWI)/AWI) %>%
  print(n = 100)

