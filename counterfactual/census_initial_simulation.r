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
## LOAD DATA AND SET UP--------------------
setwd('C:/Users/kchanwong/Documents/GitHub/social_security_cato_model/data_prep/')
load("init_sim_data.RData")
source('C:/Users/kchanwong/Documents/GitHub/social_security_cato_model/code_src/initial_simulation_functions.r')
potential_ids <- sprintf("%08d", sample(1000000:9999999, 5000000))  # 10,000 candidates
## Code Run ###
t1 <- Sys.time()
years <- 2008:2100
results <- vector("list", length(years))
current_year_data <- dfInitSamps
### Using Census Fertility Projections, What Happens? 
TFR_PROJECTIONS <- read.csv("C:/Users/kchanwong/Documents/TFR_PROJ_DIFF_SOURCE.csv")
dfFert06_08 <- normalize_tfr(dfFert06_08, TFR_PROJECTIONS %>% select(YEAR, 
                                TARGET_TFR = CENSUS), proj_start = 2025)
# Loop through each year 2008-2100 and apply functions
for(i in seq_along(years)) {
  year <- years[i]
  print(year)

  # Mortality and new households for current year
  dfInitSamp_new_units <- makeDemographic_Project(year, current_year_data)

  # Fertility for current year
  dfInitSamp_babies <- makeBabies(year, dfInitSamp_new_units)

  # Marriages and divorces for current year
  dfInitSamp_divorced <- makeMarriages_Divorced(year, dfInitSamp_babies)

  # Income, LF, Disability changes for current year
  dfInitSamp_econ_growth <- makeIncome_and_LF_and_Disability(year, dfInitSamp_divorced)

  results[[i]] <- dfInitSamp_econ_growth
  current_year_data <- dfInitSamp_econ_growth
}
t2 <- Sys.time()
t2 - t1
samps <- bind_rows(dfInitSamps, results)
# Income distribution - with brackets and percent distributions
target <- matchDist(2007)
for(i in 2008:2100) {
  print(i)
  target <- target %>%
    # add_row(matchDist(i))
    bind_rows(matchDist(i))
}
target %>%
  write_rds("C:/Users/kchanwong/Documents/GitHub/social_security_cato_model/counterfactual/census.rds")
### 
