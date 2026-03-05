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

test_that("Share of earners above maximum taxable income is 6-7%", {
  
  sample_df <- samps_w_weights %>%
    select(ID, YEAR, INCWAGE, WEIGHTS) %>%
    distinct()
  
  max_ref <- MAX_INCOME %>%
    rename(YEAR = REFYEAR)
  
  res <- sample_df %>%
    left_join(max_ref, by = "YEAR") %>%
    filter(INCWAGE > 0) %>%
    group_by(YEAR) %>%
    summarise(
      mt_pct = weighted.mean(INCWAGE > MAX_INCOME, WEIGHTS, na.rm = TRUE),
      .groups = "drop"
    )
  
  offenders <- res %>%
    mutate(avg_mt_pct = mean(mt_pct)) |>
    distinct(avg_mt_pct) |>
    mutate(
      avg_mt_pp   = 100 * avg_mt_pct,
      gap_low_pp  = 100 * pmax(0, 0.06 - avg_mt_pct),  
      gap_high_pp = 100 * pmax(0, avg_mt_pct - 0.07)   
    ) %>%
    filter(avg_mt_pct < 0.06 | avg_mt_pct > 0.07)
  
  expect_true(
    nrow(offenders) == 0,
    info = paste0(
      "mt_pct not in [6%, 7%] (percentage points):\n",
      paste(
        capture.output(
          print(
            offenders %>%
              select(YEAR, mt_pct_pp, gap_low_pp, gap_high_pp),
            n = 200
          )
        ),
        collapse = "\n"
      )
    )
  )
})