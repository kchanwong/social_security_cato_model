library(testthat)
library(tidyverse)

BASELINE_SUM <- read.csv("BASELINE_SUM.csv")

dfECON <- 
  read.csv("OASI_DI_PROJECTIONS.csv")

test_that("Model tracks SSA baseline within 1 percentage point on average", {
  
  BASELINE_SUM_comp <- BASELINE_SUM %>%
    select(YEAR, contains('PERC')) %>%
    mutate(PERC_TAXABLE = 100 * PERC_TAXABLE) %>%
    inner_join(
      dfECON %>%
        filter(ALTERNATIVE %in% c(0, 2)) %>%
        select(YEAR, OASI_COST_RATE)
    ) %>%
    mutate(DIFF_SSA = OASI_COST_RATE - PERC_TAXABLE)
  
  mean_abs_diff <- mean(abs(BASELINE_SUM_comp$DIFF_SSA))
  
  expect_lt(mean_abs_diff, 1)
})


