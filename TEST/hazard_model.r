### Packages ### 
library(dplyr)
library(RMariaDB)
library(fredr)
library(DBI)
library(readxl)
library(ipumsr)
### Set Up API Keys and Database Authentication ###

###
read_vi_g6 <- function(path) {
 
  raw <- read_excel(path, sheet = "VI.G6", col_names = FALSE)
 
  # Column names are in row 10 (0-indexed: row 9 in R's 1-indexed read)
  col_names <- c("year", "cpi_w", "avg_wage_index", "taxable_payroll_b",
                 "gdp_b", "eff_interest_factor")
 
  # Helper: extract a block of numeric rows between two row indices
  extract_block <- function(df, start_row, end_row, scenario) {
    df[start_row:end_row, 1:6] |>
      setNames(col_names) |>
      mutate(
        across(everything(), as.numeric),
        scenario = scenario
      ) |>
      filter(!is.na(year))
  }
 
  # Row positions (1-indexed, matching what read_excel returns):
  #   Historical  : rows 12–65   (label in row 11)
  #   Intermediate: rows 67–143  (label in row 66)
  #   Low-cost    : rows 145–221 (label in row 144)
  #   High-cost   : rows 223–299 (label in row 222)
 
  historical    <- extract_block(raw,  12,  65, "historical")
  intermediate  <- extract_block(raw,  67, 143, "intermediate")
  low_cost      <- extract_block(raw, 145, 221, "low_cost")
  high_cost     <- extract_block(raw, 223, 299, "high_cost")
 
  bind_rows(historical, intermediate, low_cost, high_cost) |>
    mutate(year = as.integer(year)) |>
    select(scenario, year, cpi_w, avg_wage_index,
           taxable_payroll_b, gdp_b, eff_interest_factor)
}
 
# ── Usage ─────────────────────────────────────────────────────────────────────
path <- "C:/Users/kchanwong/Downloads/SingleYearTRTables_TR2025 (3).xlsx"   # adjust path as needed
vi_g6 <- read_vi_g6(path)
economic_variables <- vi_g6 %>% 
    select(scenario, year, avg_wage_index) %>%
    filter(scenario %in% c('historical', 'intermediate'))

#### Goal: Build Hazard Model ####
#### based on expected Social Security Wealth ####
## Step One: Build Social Security Wealth Variable ### 
INFLATION <- fredr(series_id = 'CWSR0000SA0') %>%
    mutate(YEAR = format(date, "%Y"),
           MONTH = format(date, "%m")) %>% 
           select(YEAR, MONTH, CPI_W = value)  %>%
           filter(MONTH == 12)   

colnames(SAMPLE_PERSON) <- c('ID', 'AGE', 'YEAR', 'EARNINGS')
SAMPLE_PERSON <- SAMPLE_PERSON %>% mutate(EARNINGS = as.integer(EARNINGS))
project_earnings_mincer <- function(past_data,    # data.frame: AGE, YEAR, EARNINGS
                                    cpi_data,     # data.frame: YEAR, CPI_W
                                    beta1,
                                    beta2,
                                    end_age = 65) {
  past_data$YEAR <- as.integer(past_data$YEAR)
  cpi_data$YEAR  <- as.integer(cpi_data$YEAR)
  past_data      <- past_data[order(past_data$YEAR), ]

  last_row       <- tail(past_data, 1)
  start_age      <- as.integer(last_row$AGE)
  start_year     <- as.integer(last_row$YEAR)
  start_earnings <- last_row$EARNINGS

  base_cpi <- cpi_data$CPI_W[cpi_data$YEAR == start_year]
  if (length(base_cpi) == 0) stop(paste("No CPI_W data found for year", start_year))

  future_ages  <- seq(start_age + 1L, as.integer(end_age))
  future_years <- start_year + seq_along(future_ages)
  t            <- seq_along(future_ages)

  future_cpi       <- cpi_data$CPI_W[match(future_years, cpi_data$YEAR)]
  nominal_earnings <- start_earnings * exp(beta1 * t + beta2 * t^2) * (future_cpi / base_cpi)

  projected <- data.frame(
    AGE      = future_ages,
    YEAR     = future_years,
    EARNINGS = nominal_earnings,
    TYPE     = "projected"
  )

  past_out      <- past_data[, c("AGE", "YEAR", "EARNINGS")]
  past_out$TYPE <- "observed"

  rbind(past_out, projected)
}
lifetime_earnings <- function(past_data, cpi_data, beta1, beta2, end_age = 65) {

  past_data$YEAR <- as.integer(past_data$YEAR)
  cpi_data$YEAR  <- as.integer(cpi_data$YEAR)
  past_data      <- past_data[order(past_data$YEAR), ]

  past_data$LIFETIME_EARNINGS <- sapply(seq_len(nrow(past_data)), function(i) {
    future_profile <- project_earnings_mincer(
      past_data = past_data[1:i, ],
      cpi_data  = cpi_data,
      beta1     = beta1,
      beta2     = beta2,
      end_age   = end_age
    )
    sum(past_data$EARNINGS[1:i], na.rm = TRUE) +
      sum(future_profile$EARNINGS[future_profile$TYPE == "projected"], na.rm = TRUE)
  })

  past_data
}

# Unified AWI series: historical takes priority, intermediate fills future years
awi_series <- bind_rows(
  economic_variables %>% filter(scenario == "historical"),
  economic_variables %>% filter(scenario == "intermediate") %>%
    anti_join(economic_variables %>% filter(scenario == "historical"), by = "year")
) %>% select(year, avg_wage_index)

get_fra <- function(birth_year) {
  case_when(
    birth_year <= 1954 ~ 66,
    birth_year == 1955 ~ 66 + 2/12,
    birth_year == 1956 ~ 66 + 4/12,
    birth_year == 1957 ~ 66 + 6/12,
    birth_year == 1958 ~ 66 + 8/12,
    birth_year == 1959 ~ 66 + 10/12,
    birth_year >= 1960 ~ 67
  )
}
calculate_ss_wealth <- function(past_data, awi_data, cpi_data, birth_year,
                                beta1, beta2, end_age = 65,
                                discount_rate = 0.02, cola = 0.023, max_age = 100) {

  past_data$YEAR <- as.integer(past_data$YEAR)
  cpi_data$YEAR  <- as.integer(cpi_data$YEAR)
  awi_data$year  <- as.integer(awi_data$year)
  past_data      <- past_data[order(past_data$YEAR), ]

  fra           <- get_fra(birth_year)
  indexing_year <- as.integer(birth_year + 60)
  awi_1977      <- 9779.44
  awi_index     <- awi_data$avg_wage_index[awi_data$year == indexing_year]
  bp1           <- floor(180  * awi_index / awi_1977)
  bp2           <- floor(1085 * awi_index / awi_1977)

  # Pre-compute named lookup vectors (avoid repeated df joins inside loop)
  cpi_vec <- setNames(cpi_data$CPI_W, cpi_data$YEAR)
  awi_vec <- setNames(awi_data$avg_wage_index, awi_data$year)

  # Pre-compute indexed observed earnings once (fixed across all iterations)
  obs_awi     <- awi_vec[as.character(past_data$YEAR)]
  obs_indexed <- ifelse(
    past_data$YEAR < indexing_year,
    past_data$EARNINGS * (awi_index / obs_awi),
    past_data$EARNINGS
  )

  # Pre-compute per-age discount sums (only depends on age, not earnings)
  monthly_r    <- (1 + discount_rate)^(1/12) - 1
  months_seq   <- seq(fra * 12, max_age * 12 - 1)
  m_since_fra  <- months_seq - fra * 12
  cola_factors <- (1 + cola)^floor(m_since_fra / 12)
  disc_sums    <- setNames(
    sapply(past_data$AGE, function(age) {
      sum(cola_factors / (1 + monthly_r)^(months_seq - age * 12))
    }),
    past_data$AGE
  )

  n       <- nrow(past_data)
  pia_vec <- numeric(n)
  ss_vec  <- numeric(n)

  for (i in seq_len(n)) {

    start_age      <- as.integer(past_data$AGE[i])
    start_year     <- as.integer(past_data$YEAR[i])
    start_earnings <- past_data$EARNINGS[i]
    base_cpi       <- cpi_vec[as.character(start_year)]

    future_ages <- seq(start_age + 1L, as.integer(end_age))
    if (length(future_ages) > 0) {
      future_years  <- start_year + seq_along(future_ages)
      t             <- seq_along(future_ages)
      future_cpi    <- cpi_vec[as.character(future_years)]
      proj_earnings <- start_earnings * exp(beta1 * t + beta2 * t^2) * (future_cpi / base_cpi)
      proj_awi      <- awi_vec[as.character(future_years)]
      proj_indexed  <- ifelse(
        future_years < indexing_year,
        proj_earnings * (awi_index / proj_awi),
        proj_earnings
      )
    } else {
      proj_indexed <- numeric(0)
    }

    all_indexed <- c(obs_indexed[1:i], proj_indexed)
    aime        <- sum(sort(all_indexed, decreasing = TRUE)[1:min(35, length(all_indexed))], na.rm = TRUE) / 420
    pia         <- 0.90 * min(aime, bp1) +
                   0.32 * max(0, min(aime, bp2) - bp1) +
                   0.15 * max(0, aime - bp2)
    pia_vec[i]  <- floor(pia * 10) / 10
    ss_vec[i]   <- pia_vec[i] * disc_sums[i]
  }

  past_data$PIA       <- pia_vec
  past_data$SS_WEALTH <- ss_vec
  past_data
}







db <- dbConnect(
    RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    password = Sys.getenv("DB_PASSWORD"),
    user = Sys.getenv('DB_USER'),
    host = Sys.getenv('DB_HOST'),
    port = Sys.getenv('DB_PORT')
)
PERSON_INDEX <- lapply(seq(1920, 1980, by = 10), function(decade) {
  dbGetQuery(
    sprintf(
      "SELECT * FROM social_security_research.puf_demo_2020 WHERE `BY` >= %d AND `BY` <= %d",
      decade, decade + 9
    ),
    conn = db
  ) %>% as_tibble()
}) %>% setNames(paste0("decade_", seq(1920, 1980, by = 10)))

library(parallel)


awi_historical_early <- tibble(
  scenario       = "historical",
  year           = 1951:1970,
  avg_wage_index = c(
    2799.16, 2973.32, 3139.44, 3155.64, 3301.44,
    3532.36, 3641.72, 3673.80, 3855.80, 4007.12,
    4086.76, 4291.40, 4396.64, 4576.32, 4658.72,
    4938.36, 5213.44, 5571.76, 5893.76, 6186.24
  )
)

economic_variables <- bind_rows(awi_historical_early, economic_variables)


parallel_ss_wealth <- function(earnings_df, cl,
                               beta1 = 0.05, beta2 = -0.001, end_age = 65,
                               discount_rate = 0.02, cola = 0, max_age = 100) {
  colnames(earnings_df) <- c("ID", "AGE", "YEAR", "EARNINGS")
  earnings_df$EARNINGS  <- as.integer(earnings_df$EARNINGS)

  person_list <- split(earnings_df, earnings_df$ID)

  results <- parLapply(cl, person_list, function(person_data) {
    birth_year <- as.integer(min(person_data$YEAR) - min(person_data$AGE))
    tryCatch(
      calculate_ss_wealth(
        past_data     = person_data,
        awi_data      = awi_series,
        cpi_data      = INFLATION,
        birth_year    = birth_year,
        beta1         = beta1,
        beta2         = beta2,
        end_age       = end_age,
        discount_rate = discount_rate,
        cola          = cola,
        max_age       = max_age
      ),
      error = function(e) NULL
    )
  })

  bind_rows(Filter(Negate(is.null), results))
}
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, c("calculate_ss_wealth", "get_fra", "awi_series", "INFLATION"))
result <- parallel_ss_wealth(earnings, cl)

result %>% filter(SS_WEALTH > 0)

sampled_ids <- sample(PERSON_INDEX$decade_1940$ID, 5000, replace = TRUE)
unique_ids  <- unique(sampled_ids)
earnings <- dbGetQuery(
    sprintf(
      "SELECT * FROM social_security_research.puf_earnings_2020 WHERE ID IN (%s)",
      paste(unique_ids, collapse = ",")
    ),
    conn = db
  )
earnings <- as_tibble(earnings)



# One batched query for all unique IDs
  earnings <- dbGetQuery(
    sprintf(
      "SELECT * FROM social_security_research.puf_earnings_2020 WHERE ID IN (%s)",
      paste(unique_ids, collapse = ",")
    ),
    conn = db
  )
# Create cluster once — reused across all decades
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, c("calculate_ss_wealth", "get_fra", "awi_series", "INFLATION"))

run_decade_bootstrap <- function(decade_df, n_sample = 5000) {

  # Bootstrap sample (with replacement)
  sampled_ids <- sample(decade_df$ID, n_sample, replace = TRUE)
  unique_ids  <- unique(sampled_ids)

  # One batched query for all unique IDs
  earnings <- dbGetQuery(
    sprintf(
      "SELECT * FROM social_security_research.puf_earnings_2020 WHERE ID IN (%s)",
      paste(unique_ids, collapse = ",")
    ),
    conn = db
  )
  colnames(earnings) <- c("ID", "AGE", "YEAR", "EARNINGS")
  earnings$EARNINGS  <- as.integer(earnings$EARNINGS)

  # Process each person in parallel
  person_list <- split(earnings, earnings$ID)

  results <- parLapply(cl, person_list, function(person_data) {
    birth_year <- as.integer(min(person_data$YEAR) - min(person_data$AGE))
    tryCatch(
      calculate_ss_wealth(
        past_data     = person_data,
        awi_data      = awi_series,
        cpi_data      = INFLATION,
        birth_year    = birth_year,
        beta1         = 0.05,
        beta2         = -0.001,
        end_age       = 65,
        discount_rate = 0.02,
        cola          = 0,
        max_age       = 100
      ),
      error = function(e) NULL
    )
  })
run_decade_bootstrap(PERSON_INDEX$decade_1980)
  result_df <- bind_rows(Filter(Negate(is.null), results))
  if (nrow(result_df) == 0) return(result_df)

  # Replicate rows to honour bootstrap duplicates
  id_counts        <- table(sampled_ids)
  counts           <- as.integer(id_counts[as.character(result_df$ID)])
  counts[is.na(counts)] <- 1L
  result_df[rep(seq_len(nrow(result_df)), counts), ]
}

all_results <- lapply(names(PERSON_INDEX), function(decade_name) {
  message("Processing ", decade_name)
  df        <- run_decade_bootstrap(PERSON_INDEX[[decade_name]])
  df$decade <- decade_name
  df
})

stopCluster(cl)

final_results <- bind_rows(all_results)




SAVINGS <- fredr(series_id = 'PSAVERT') %>%
    mutate(YEAR = format(date, '%Y'),
           MONTH = format(date, '%m')) %>%
    filter(MONTH == 12) %>% 
    select(YEAR, MONTH, SAVING_RATE = value) %>%
    mutate(YEAR = as.integer(YEAR))

SAMPLE_PERSON <- dbGetQuery(
    "SELECT * from social_security_research.puf_earnings_2020 WHERE ID = 5880552",
    conn = db
)
colnames(SAMPLE_PERSON) <- c('ID', 'AGE', 'YEAR', 'EARNINGS')
SAMPLE_PERSON <- SAMPLE_PERSON %>% mutate(EARNINGS = as.integer(EARNINGS))
birth_year <- as.integer(min(SAMPLE_PERSON$YEAR) - min(SAMPLE_PERSON$AGE))
start <- proc.time()
result <- calculate_ss_wealth(
  past_data     = SAMPLE_PERSON,
  awi_data      = awi_series,
  cpi_data      = INFLATION,
  birth_year    = birth_year,
  beta1         = 0.05,
  beta2         = -0.001,
  end_age       = 65,
  discount_rate = 0.02,
  max_age       = 100,
  cola = 0
)
result 
end <- proc.time()
end-start

