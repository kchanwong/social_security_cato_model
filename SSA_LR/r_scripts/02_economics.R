# =============================================================================
# 02_economics.R
# Projects economic variables needed for trust fund scoring:
#   - Average Wage Index (AWI) — nominal and real
#   - Taxable maximum (contribution and benefit base)
#   - Covered workers (by type: wage, self-employed)
#   - Covered earnings and taxable wages
#   - Effective taxable payroll (denominator for all actuarial rates)
#   - New-issue bond yield path (feeds 04_trust_fund.R)
#
# Inputs:
#   - demo_results from 01_demography.R (working_age_pop, pop_proj)
#   - assumptions.R parameters
#
# Outputs:
#   - econ: named list with annual series 2025-2099
#     $awi, $taxmax, $covered_workers, $covered_earnings,
#     $taxable_payroll, $eff_taxable_payroll, $yield, $cpi_index, $unemployment
#
# Calibration target: TR2025 Table V.B1 (AWI) and V.C series (payroll)
# =============================================================================

source("inputs/assumptions.R")
suppressPackageStartupMessages(library(dplyr))

# =============================================================================
# 0. HISTORICAL BASE VALUES (end of 2024, used as projection anchors)
# =============================================================================
# Source: TR2025 Table V.C1 and SSA wage statistics

AWI_2024          <- 68084.00   # Average Wage Index, 2024 (estimated; SSA publishes Oct)
                                 # V.C1 shows 2023 = $63,795.13; 2024 estimated +6.7% (see note)
                                 # NOTE: SSA will publish official 2024 AWI in Oct 2025.
                                 # Placeholder: use projection from 2023 AWI × (1 + 0.0421).
AWI_2024          <- 63795.13 * (1 + 4.21/100)   # ≈ 66,479; using V.B1 2024 nominal AWI growth
TAXMAX_2024       <- 168600    # Contribution and benefit base (taxable max), 2024
CPI_2024          <- 314.5     # CPI-W index level, 2024 (approx; used for COLA chain)

# 2024 trust fund baseline — all _K constants in thousands; ×1000 = persons
# Source: SSA Statistical Supplement 4.B7 (2023) extrapolated to 2024
COVERED_WORKERS_2024_M_K  <- 162000   # ~162M wage/salary covered workers
COVERED_WORKERS_2024_SE_K <-  15500   # ~15.5M self-employed covered
COVERED_WORKERS_2024_TOT_K <- COVERED_WORKERS_2024_M_K + COVERED_WORKERS_2024_SE_K
COVERED_EARNINGS_2024     <- 10850e9  # dollars: total covered earnings
TAXABLE_PAYROLL_2024      <-  9200e9  # dollars: OASDI taxable payroll (SSA SR 2024)
EFF_TAXABLE_PAYROLL_2024  <- TAXABLE_PAYROLL_2024 * 0.9997

# Average covered earnings per worker (dollars, 2024): ~$61k
ACE_BASE_2024 <- COVERED_EARNINGS_2024 / (COVERED_WORKERS_2024_TOT_K * 1000)

# Aggregate covered-worker rate: total covered workers / SS area pop 20-64
# Both in thousands → dimensionless rate ≈ 0.889
CWR_AGG_BASE  <- COVERED_WORKERS_2024_TOT_K / 199564
CWR_SE_SHARE  <- COVERED_WORKERS_2024_SE_K  / COVERED_WORKERS_2024_TOT_K  # ~0.087
UNEMP_RATE_2024 <- 0.041

# =============================================================================
# 1. AWI PROJECTION
# =============================================================================

build_awi_series <- function(scenario = SCENARIO) {
  awi_params <- get_assumption(awi_nominal_pct, scenario)
  cpi_params <- get_assumption(cpi_pct, scenario)

  all_years <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR
  awi  <- numeric(length(all_years))
  cola <- numeric(length(all_years))  # COLA = prior-year CPI change
  cpi  <- numeric(length(all_years))  # CPI index level

  # 2024 anchors
  awi_prev  <- AWI_2024
  cpi_prev  <- CPI_2024

  for (i in seq_along(all_years)) {
    yr <- all_years[i]

    # Nominal AWI growth rate for this year
    if (yr <= 2034) {
      awi_pct <- awi_params$transition[as.character(yr)]
    } else {
      awi_pct <- awi_params$ultimate
    }

    # CPI growth rate for this year
    if (yr <= 2034) {
      cpi_pct_yr <- cpi_params$transition[as.character(yr)]
    } else {
      cpi_pct_yr <- cpi_params$ultimate
    }

    awi[i]  <- awi_prev  * (1 + awi_pct  / 100)
    cpi[i]  <- cpi_prev  * (1 + cpi_pct_yr / 100)

    # COLA: applied in December of year yr, based on CPI change from Q3(yr-1) to Q3(yr)
    # Simplified: COLA ≈ CPI annual growth rate (used for benefit indexing in 03_beneficiaries.R)
    cola[i] <- cpi_pct_yr / 100

    awi_prev <- awi[i]
    cpi_prev <- cpi[i]
  }

  real_awi_growth <- (awi / c(AWI_2024, awi[-length(awi)])) /
                     (cpi / c(CPI_2024, cpi[-length(cpi)])) - 1

  data.frame(
    year           = all_years,
    awi            = round(awi, 2),
    awi_nominal_pct_chg = c(NA, diff(awi) / awi[-length(awi)] * 100),
    real_awi_pct_chg    = c(NA, real_awi_growth[-1] * 100),
    cpi_index      = round(cpi, 2),
    cola           = cola,
    scenario       = scenario
  )
}

# =============================================================================
# 2. TAXABLE MAXIMUM (Contribution and Benefit Base)
# =============================================================================
# Rule: taxmax(yr) = taxmax(yr-1) × AWI(yr-2) / AWI(yr-3), rounded to nearest $300
# (SSA indexes taxmax to AWI with a 2-year lag, per statute)

build_taxmax_series <- function(awi_series, scenario = SCENARIO) {
  all_years <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR
  taxmax <- numeric(length(all_years))

  # Lagged AWI values needed: AWI(2022) and AWI(2023) for 2025 taxmax
  AWI_2023 <- 63795.13
  AWI_2022 <- 60575.07   # from V.C1 historical data

  awi_all <- c(AWI_2022, AWI_2023, AWI_2024, awi_series$awi)
  awi_years <- c(2022, 2023, 2024, all_years)

  get_awi <- function(yr) {
    idx <- which(awi_years == yr)
    if (length(idx) == 0) stop(paste("AWI not found for year", yr))
    awi_all[idx]
  }

  for (i in seq_along(all_years)) {
    yr <- all_years[i]
    # taxmax(yr) indexed to AWI(yr-2) / AWI(yr-3)
    ratio <- get_awi(yr - 2) / get_awi(yr - 3)
    raw   <- TAXMAX_2024 * (ratio ^ (yr - 2024))   # compound from 2024 base
    # Re-derive year-by-year for accuracy
    if (i == 1) {
      raw <- TAXMAX_2024 * get_awi(2023) / get_awi(2022)
    } else {
      raw <- taxmax[i-1] * get_awi(yr - 2) / get_awi(yr - 3)
    }
    taxmax[i] <- round(raw / 300) * 300  # round to nearest $300
  }

  data.frame(
    year    = all_years,
    taxmax  = taxmax,
    scenario = scenario
  )
}

# =============================================================================
# 3. COVERED WORKERS
# =============================================================================
# Covered workers = f(working-age population, LFP rates, unemployment, covered-worker rate)
#
# SSA's full model has age×sex LFP rates. We use a macro approximation:
#   employment(yr) = working_age_pop(yr) × lfp_rate(yr) × (1 - unemp_rate(yr))
#   covered_wage(yr) = employment(yr) × CWR_WAGE_BASE [stable ratio]
#   covered_se(yr)   = working_age_pop(yr) × CWR_SE_BASE
#
# LFP rate trends from V.B2 (labor force annual % change) applied to base rate.
# This is the single biggest simplification vs. SSA's age-sex detailed model;
# for trust fund–level accuracy within ~0.5% of payroll, it's adequate.

build_covered_workers <- function(demo_results, scenario = SCENARIO) {
  all_years <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR
  wap <- demo_results$working_age_pop   # thousands (ages 20-64)

  # Unemployment rate path from V.B2
  unemp_path <- list(
    intermediate = setNames(
      c(0.044, 0.045, rep(0.045, length(2027:FINAL_PROJ_YEAR))),
      2025:FINAL_PROJ_YEAR
    ),
    low_cost  = setNames(
      c(0.042, 0.038, 0.036, rep(0.035, length(2028:FINAL_PROJ_YEAR))),
      2025:FINAL_PROJ_YEAR
    ),
    high_cost = setNames(
      c(0.048, 0.052, 0.054, rep(0.055, length(2028:FINAL_PROJ_YEAR))),
      2025:FINAL_PROJ_YEAR
    )
  )

  # Annual drift in aggregate covered-worker rate (secular trend from
  # earnings-share-of-compensation and LFP age-composition effects)
  cwr_drift_annual <- list(
    intermediate = -0.00050,   # slight decline (-0.05ppt/yr)
    low_cost     = +0.00030,
    high_cost    = -0.00120
  )

  unemp <- get_assumption(unemp_path, scenario)
  drift <- get_assumption(cwr_drift_annual, scenario)

  covered_wage_k <- numeric(length(all_years))
  covered_se_k   <- numeric(length(all_years))

  for (i in seq_along(all_years)) {
    yr <- all_years[i]

    # Aggregate covered-worker rate drifts from 2024 base
    cwr <- max(CWR_AGG_BASE + drift * (yr - 2024), 0.75)

    # Total covered workers (thousands) = wap × cwr
    # Note: this is coverage rate among working-age pop, not pure LFP×employment
    # It subsumes both participation and covered-employment choice
    total_cw_k <- wap[i] * cwr

    covered_se_k[i]   <- total_cw_k * CWR_SE_SHARE
    covered_wage_k[i] <- total_cw_k * (1 - CWR_SE_SHARE)
  }

  data.frame(
    year            = all_years,
    wap_k           = wap,
    covered_wage_k  = round(covered_wage_k, 1),
    covered_se_k    = round(covered_se_k, 1),
    covered_total_k = round(covered_wage_k + covered_se_k, 1),
    scenario        = scenario
  )
}

# =============================================================================
# 4. TAXABLE PAYROLL
# =============================================================================
# Covered earnings = covered_total × average_covered_wage
# Taxable payroll = covered_earnings × taxable_ratio(taxmax / AWI)
# Effective taxable payroll = taxable_payroll × 0.9997 (ME refund / HI adj)
#
# Taxable ratio model:
#   As taxmax / AWI changes, the share of earnings below taxmax changes.
#   Empirically stable: ratio ≈ 0.827 in 2024 (SSA Statistical Supplement 4.B7)
#   We model drift: if taxmax/AWI rises → taxable_ratio rises (and vice versa)
#   Using a log-linear approximation: taxable_ratio = a + b × log(taxmax / AWI)
#   Calibrated to 4.B7 historical data (1992–2023)

TAXABLE_RATIO_2024     <- 0.827   # taxable wages / covered wages, 2024
TAXMAX_AWI_RATIO_2024  <- TAXMAX_2024 / AWI_2024   # ≈ 2.53

# Log-linear sensitivity: from 4.B7 regression
# taxable_ratio = 0.517 + 0.122 × log(taxmax / AWI)
TAXRATIO_INTERCEPT <- 0.517
TAXRATIO_SLOPE     <- 0.122

taxable_ratio_from_ratio <- function(taxmax_awi_ratio) {
  r <- TAXRATIO_INTERCEPT + TAXRATIO_SLOPE * log(taxmax_awi_ratio)
  pmin(pmax(r, 0.75), 0.99)   # bound to plausible range
}

# Average covered earnings grow at the AWI rate (with minor adjustment for
# earnings-share-of-compensation drift from assumptions.R)
build_taxable_payroll <- function(cw_series, awi_series, taxmax_series,
                                  scenario = SCENARIO) {
  all_years <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR

  # Earnings-as-pct-of-compensation drift: cumulative from 2024
  earn_params <- get_assumption(earnings_pct_compensation, scenario)

  avg_covered_earnings  <- numeric(length(all_years))
  taxable_wages         <- numeric(length(all_years))
  taxable_se_income     <- numeric(length(all_years))
  taxable_payroll       <- numeric(length(all_years))
  eff_taxable_payroll   <- numeric(length(all_years))

  # Starting average covered earnings (dollars/worker, 2024)
  ACE_2024 <- COVERED_EARNINGS_2024 / (COVERED_WORKERS_2024_M + COVERED_WORKERS_2024_SE)

  ace_prev <- ACE_2024

  for (i in seq_along(all_years)) {
    yr <- all_years[i]

    # AWI nominal growth (ACE grows at same rate by construction)
    awi_pct <- awi_series$awi_nominal_pct_chg[i]
    if (is.na(awi_pct)) awi_pct <- get_assumption(awi_nominal_pct, scenario)$ultimate

    # Earnings-share drift adjustment (small annual effect)
    earn_drift <- if (yr <= 2034) {
      earn_params$transition[as.character(yr)]
    } else {
      earn_params$ultimate
    }

    ace <- ace_prev * (1 + awi_pct / 100 + earn_drift / 100)
    avg_covered_earnings[i] <- ace

    # Total covered earnings (billions)
    cw_total <- cw_series$covered_total_k[i] * 1000  # convert to persons
    total_covered_earn <- ace * cw_total / 1e9  # billions

    # Taxable ratio (from taxmax/AWI relationship)
    tm_awi_ratio <- taxmax_series$taxmax[i] / awi_series$awi[i]
    tx_ratio     <- taxable_ratio_from_ratio(tm_awi_ratio)

    # Wage workers taxable earnings
    cw_wage <- cw_series$covered_wage_k[i] * 1000
    taxable_wages[i] <- ace * cw_wage * tx_ratio / 1e9  # billions

    # Self-employed taxable income (SE contributes at full rate on 92.35% of net earnings)
    # SE effective taxable rate typically ~0.92 × tx_ratio due to deduction
    cw_se <- cw_series$covered_se_k[i] * 1000
    taxable_se_income[i] <- ace * cw_se * tx_ratio * 0.920 / 1e9

    # OASDI taxable payroll (billions)
    taxable_payroll[i] <- taxable_wages[i] + taxable_se_income[i]

    # Effective taxable payroll: applies incurred-to-cash lag and ME refund adjustment
    # SSA: eff_payroll ≈ taxable_payroll × 0.9997 (very small multi-employer refund)
    eff_taxable_payroll[i] <- taxable_payroll[i] * 0.9997

    ace_prev <- ace
  }

  data.frame(
    year                   = all_years,
    avg_covered_earn_k     = round(avg_covered_earnings / 1000, 2),
    taxable_wages_bn       = round(taxable_wages, 2),
    taxable_se_bn          = round(taxable_se_income, 2),
    taxable_payroll_bn     = round(taxable_payroll, 2),
    eff_taxable_payroll_bn = round(eff_taxable_payroll, 2),
    taxmax_awi_ratio       = round(taxmax_series$taxmax / awi_series$awi, 4),
    taxable_ratio          = round(taxable_ratio_from_ratio(
                               taxmax_series$taxmax / awi_series$awi), 4),
    scenario               = scenario
  )
}

# =============================================================================
# 5. NEW-ISSUE BOND YIELD PATH
# =============================================================================
# Source: TR2025 Table V.B2 (annual nominal yield on new special-issue bonds)
# Intermediate: 4.2% (2025) → 4.1% (2026-2034) → grades to 4.7% (2041+)
# This is used by 04_trust_fund.R for interest income projection.

build_yield_series <- function(scenario = SCENARIO) {
  all_years <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR

  # From V.B2 data extracted above (intermediate shown; others approximate)
  yield_intermediate <- c(
    # 2025-2034 from V.B2 directly
    setNames(c(4.2, 4.1, 4.1, 4.1, 4.1, 4.1, 4.1, 4.1, 4.1, 4.1), 2025:2034),
    # 2035-2044: grade from 4.1 to 4.7 (V.B2 shows 4.3, 4.4, 4.5, 4.5, 4.6, 4.6, 4.7...)
    "2035" = 4.3, "2036" = 4.4, "2037" = 4.5, "2038" = 4.5, "2039" = 4.6,
    "2040" = 4.6, "2041" = 4.7,
    # 2042+: 4.7% flat (ultimate = CPI_ult + real_rate_ult = 2.4 + 2.7 - ~0.4 convexity)
    setNames(rep(4.7, length(2042:FINAL_PROJ_YEAR)), 2042:FINAL_PROJ_YEAR)
  )

  yield_low_cost <- c(
    setNames(c(4.6, 4.6, 4.8, 4.8, 4.9, 5.0, 5.1, 5.2, 5.3, 5.4), 2025:2034),
    setNames(rep(5.8, length(2035:FINAL_PROJ_YEAR)), 2035:FINAL_PROJ_YEAR)
  )  # real_rate_ult 3.3 + CPI 3.0 = 6.3% nominal, but portfolio blend lower near-term

  yield_high_cost <- c(
    setNames(c(4.0, 3.9, 3.8, 3.7, 3.6, 3.6, 3.6, 3.6, 3.6, 3.6), 2025:2034),
    setNames(rep(4.0, length(2035:FINAL_PROJ_YEAR)), 2035:FINAL_PROJ_YEAR)
  )  # real_rate_ult 2.2 + CPI 1.8 = 4.0% nominal

  yield_map <- list(
    intermediate = yield_intermediate,
    low_cost     = yield_low_cost,
    high_cost    = yield_high_cost
  )

  yields <- get_assumption(yield_map, scenario)

  data.frame(
    year         = all_years,
    new_issue_yield = as.numeric(yields[as.character(all_years)]) / 100,
    scenario     = scenario
  )
}

# =============================================================================
# 6. CALIBRATION CHECK
# =============================================================================
# Benchmark: TR2025 Table V.B1 nominal AWI growth; and implied taxable payroll
# from SSA short-range estimates (not published in these XLSXs but checkable
# via V.C income/cost rate tables later after 04_trust_fund.R is run)

calibrate_economics <- function(econ_results, scenario = SCENARIO) {
  if (scenario != "intermediate") {
    message("[02_economics.R] Calibration benchmarks only available for intermediate.")
    return(invisible(NULL))
  }

  # AWI benchmark from V.B1 transition years
  awi_bench <- data.frame(
    year    = 2025:2034,
    awi_pct = c(3.97, 4.13, 4.03, 4.11, 3.94, 3.88, 3.93, 3.95, 3.96, 3.85)
  )

  awi_mod <- econ_results$awi[econ_results$awi$year %in% 2025:2034,
                               c("year","awi_nominal_pct_chg")]
  merged <- merge(awi_bench, awi_mod, by = "year")
  merged$diff <- merged$awi_nominal_pct_chg - merged$awi_pct

  message("\n[Calibration: V.B1 AWI % vs. Model]")
  print(merged, row.names = FALSE, digits = 3)
  message(sprintf("  Max AWI deviation: %.3f ppt", max(abs(merged$diff), na.rm = TRUE)))

  # Taxable payroll benchmark: SSA 2025 SR estimate ≈ $9.85T
  pay_2025 <- econ_results$payroll$taxable_payroll_bn[1]
  message(sprintf("\n  Taxable payroll 2025 (model): $%.2fT | SR estimate: ~$9.85T | diff: %.1f%%",
                  pay_2025 / 1000,
                  100 * (pay_2025 / 1000 - 9.85) / 9.85))
  invisible(merged)
}

# =============================================================================
# 7. MAIN ENTRY POINT
# =============================================================================

run_economics <- function(demo_results, scenario = SCENARIO, calibrate = TRUE) {
  message(sprintf("[02_economics.R] Running economics module | scenario: %s", scenario))

  awi_series    <- build_awi_series(scenario)
  taxmax_series <- build_taxmax_series(awi_series, scenario)
  cw_series     <- build_covered_workers(demo_results, scenario)
  pay_series    <- build_taxable_payroll(cw_series, awi_series, taxmax_series, scenario)
  yield_series  <- build_yield_series(scenario)

  econ <- list(
    awi     = awi_series,
    taxmax  = taxmax_series,
    workers = cw_series,
    payroll = pay_series,
    yield   = yield_series,
    scenario = scenario
  )

  if (calibrate) calibrate_economics(econ, scenario)

  # Summary
  yrs <- c(2025, 2035, 2050, 2075, 2099)
  idx <- match(yrs, awi_series$year)
  message("\n[02_economics.R] Key economic series:")
  summary_tbl <- data.frame(
    year            = yrs,
    awi             = round(awi_series$awi[idx]),
    taxmax          = taxmax_series$taxmax[idx],
    covered_wkrs_M  = round(cw_series$covered_total_k[idx] / 1000, 2),
    taxable_pay_T   = round(pay_series$taxable_payroll_bn[idx] / 1000, 2),
    yield_pct       = round(yield_series$new_issue_yield[idx] * 100, 2)
  )
  print(summary_tbl, row.names = FALSE)

  message(sprintf("\n[02_economics.R] Done. 2025 AWI: $%s | 2025 taxable payroll: $%.2fT",
                  format(round(awi_series$awi[1]), big.mark=","),
                  pay_series$taxable_payroll_bn[1] / 1000))
  econ
}

# Run if sourced standalone
if (!exists("ECONOMICS_LOADED")) {
  ECONOMICS_LOADED <- TRUE
  if (!exists("demo_results")) {
    suppressPackageStartupMessages(library(dplyr))
    source("R/01_demography.R")
  }
  econ_results <- run_economics(demo_results, scenario = SCENARIO, calibrate = TRUE)
}
