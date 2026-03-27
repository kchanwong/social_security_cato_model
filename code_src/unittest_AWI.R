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


library(testthat)
library(dplyr)
library(tidyr)

load("init_sim_data.RData")

samps_w_weights <- readRDS("initial_simulation_v1.RDS")

awi_test <- function(sampletest_df, awi_ref) {
  
  awi_sample <- sampletest_df %>%
    distinct(ID, YEAR, INCWAGE, WEIGHTS) %>%
    filter(INCWAGE > 0) %>%
    group_by(YEAR) %>%
    summarise(
      avgwage = weighted.mean(INCWAGE, WEIGHTS),
      .groups = "drop"
    )
  
  awi_ref %>%
    inner_join(awi_sample, by = "YEAR") %>%
    mutate(
      awi_diff = (avgwage - AWI) / AWI
    )
}

test_that("Average wage matches SSA AWI within 5%", {
  
  res <- awi_test(
    samps_w_weights,
    df_econ_assumptions %>%
      filter(ALTERNATIVE %in% c(0,2)) %>%
      select(REFYEAR, AWI) %>%
      na.omit() %>%
      rename(YEAR = REFYEAR)
  )
  
  offenders <- res %>%
    mutate(mean_avgwage = mean(avgwage),
           mean_AWI = mean(AWI)) %>%
    select(mean_avgwage, mean_AWI) %>%
    distinct() %>%
    mutate(mean_awi_diff = (mean_avgwage - mean_AWI)/mean_AWI) %>%
    mutate(
      deviation = abs(mean_awi_diff),
      gap = deviation - 0.05
    ) %>%
    filter(deviation > 0.05)
  
  expect_true(
    nrow(offenders) == 0,
    info = paste0(
      "AWI deviation exceeds 5% by:\n",
      paste(
        capture.output(
          print(
            offenders %>%
              select(mean_avgwage, mean_AWI, mean_awi_diff, gap),
            n = 100)),
        collapse = "\n")))
})
