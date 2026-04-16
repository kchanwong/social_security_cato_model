# =============================================================================
# assumptions.R
# Single source of truth for all LR model assumptions.
# All parameters that vary across scenarios live here.
# Sources: 2025 Trustees Report Tables V.A1, V.A2, V.B1, V.C1
# =============================================================================

# ---- Projection horizon -----------------------------------------------------
FIRST_PROJ_YEAR <- 2025   # first projection year (short-range starts here)
FINAL_PROJ_YEAR <- 2099   # 75-year horizon from 2025
BASE_POP_YEAR   <- 2024   # year of starting population
VALUATION_YEAR  <- 2025   # January 1 of TR year (ni in SSA notation)

# ---- Scenario selector ------------------------------------------------------
# Options: "intermediate", "low_cost", "high_cost", or "custom"
SCENARIO <- "intermediate"

# =============================================================================
# PROCESS 1: DEMOGRAPHY
# =============================================================================

# ---- 1.1 Fertility (TFR) ----------------------------------------------------
# Source: TR2025 Table V.A1
# SSA uses age-specific fertility rates (ASFRs) that sum to TFR.
# We store the TFR path; ASFRs are derived in 01_demography.R via a fixed
# shape function calibrated to 2024 NCHS data.

tfr_path <- list(
  intermediate = c(
    # year = TFR (from V.A1; 5-yr intervals 2025-2100, linearly interpolated in code)
    "2025" = 1.64, "2030" = 1.72, "2035" = 1.80, "2040" = 1.87,
    "2045" = 1.90, "2050" = 1.90, "2055" = 1.90, "2060" = 1.90,
    "2065" = 1.90, "2070" = 1.90, "2075" = 1.90, "2080" = 1.90,
    "2085" = 1.90, "2090" = 1.90, "2095" = 1.90, "2100" = 1.90
  ),
  low_cost = c(
    "2025" = 1.67, "2030" = 1.84, "2035" = 1.97, "2040" = 2.07,
    "2045" = 2.10, "2050" = 2.10, "2055" = 2.10, "2060" = 2.10,
    "2065" = 2.10, "2070" = 2.10, "2075" = 2.10, "2080" = 2.10,
    "2085" = 2.10, "2090" = 2.10, "2095" = 2.10, "2100" = 2.10
  ),
  high_cost = c(
    "2025" = 1.59, "2030" = 1.54, "2035" = 1.55, "2040" = 1.58,
    "2045" = 1.60, "2050" = 1.60, "2055" = 1.60, "2060" = 1.60,
    "2065" = 1.60, "2070" = 1.60, "2075" = 1.60, "2080" = 1.60,
    "2085" = 1.60, "2090" = 1.60, "2095" = 1.60, "2100" = 1.60
  )
)

# ---- 1.2 Mortality ----------------------------------------------------------
# Source: TR2025 Table V.A1 (age-sex-adjusted death rates), V.A4 (life expectancy)
# SSA's full model uses cause-specific central death rate reduction factors.
# We use a simpler but calibration-verified approach: annual percent reductions
# in q(x) by broad age group, converging to ultimate values.
# Units: percent reduction per year in central death rates m(x)
# Age groups: <1, 1-4, 5-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65-74, 75-84, 85+

# Ultimate annual reduction rates (percent) in m(x) by age group
# Calibrated so that intermediate V.A4 life expectancy path is reproduced
mortality_ult_reduction <- list(
  # Both sexes shown; SSA sets male = female at ultimate
  intermediate = c(
    "lt1"   = 0.90,  # <1
    "1to4"  = 1.00,  # 1-4
    "5to14" = 0.90,  # 5-14
    "15to24"= 0.75,  # 15-24
    "25to34"= 0.75,  # 25-34
    "35to44"= 0.85,  # 35-44
    "45to54"= 1.00,  # 45-54
    "55to64"= 1.05,  # 55-64
    "65to74"= 1.00,  # 65-74
    "75to84"= 0.85,  # 75-84
    "85plus"= 0.60   # 85+
  ),
  low_cost = c(    # slower mortality improvement (higher cost from more survivors)
    "lt1"   = 0.50, "1to4"  = 0.50, "5to14" = 0.50, "15to24"= 0.50,
    "25to34"= 0.50, "35to44"= 0.50, "45to54"= 0.60, "55to64"= 0.65,
    "65to74"= 0.65, "75to84"= 0.55, "85plus"= 0.40
  ),
  high_cost = c(   # faster improvement (more beneficiaries, higher cost)
    "lt1"   = 1.30, "1to4"  = 1.40, "5to14" = 1.25, "15to24"= 1.10,
    "25to34"= 1.10, "35to44"= 1.20, "45to54"= 1.40, "55to64"= 1.50,
    "65to74"= 1.45, "75to84"= 1.25, "85plus"= 0.90
  )
)

# Year at which ultimate mortality reduction rates are achieved
# Intermediate: SSA grades from current (2024) observed rates to ultimate by 2044
mortality_ultimate_year <- list(
  intermediate = 2044,
  low_cost     = 2044,
  high_cost    = 2044
)

# ---- 1.3 Immigration --------------------------------------------------------
# Source: TR2025 Table V.A2
# Two components: LPR net change, Temp/Unlawfully Present (TUP) net change
# Units: thousands

lpr_net <- list(
  intermediate = c(
    "2025" = 910,  "2030" = 788,  "2035" = 788,  "2040" = 788,
    "2045" = 788,  "2050" = 788,  "2055" = 788,  "2060" = 788,
    "2065" = 788,  "2070" = 788,  "2075" = 788,  "2080" = 788,
    "2085" = 788,  "2090" = 788,  "2095" = 788,  "2100" = 788
  ),
  low_cost = c(
    "2025" = 1130, "2030" = 1000, "2035" = 1000, "2040" = 1000,
    "2045" = 1000, "2050" = 1000, "2055" = 1000, "2060" = 1000,
    "2065" = 1000, "2070" = 1000, "2075" = 1000, "2080" = 1000,
    "2085" = 1000, "2090" = 1000, "2095" = 1000, "2100" = 1000
  ),
  high_cost = c(
    "2025" = 709,  "2030" = 595,  "2035" = 595,  "2040" = 595,
    "2045" = 595,  "2050" = 595,  "2055" = 595,  "2060" = 595,
    "2065" = 595,  "2070" = 595,  "2075" = 595,  "2080" = 595,
    "2085" = 595,  "2090" = 595,  "2095" = 595,  "2100" = 595
  )
)

tup_net <- list(
  # Temp/Unlawfully Present net change (thousands)
  intermediate = c(
    "2025" = 1192, "2030" = 536,  "2035" = 520,  "2040" = 502,
    "2045" = 484,  "2050" = 473,  "2055" = 468,  "2060" = 463,
    "2065" = 460,  "2070" = 456,  "2075" = 454,  "2080" = 452,
    "2085" = 451,  "2090" = 449,  "2095" = 449,  "2100" = 448
  ),
  low_cost = c(
    "2025" = 1758, "2030" = 867,  "2035" = 828,  "2040" = 786,
    "2045" = 747,  "2050" = 719,  "2055" = 702,  "2060" = 688,
    "2065" = 678,  "2070" = 671,  "2075" = 665,  "2080" = 661,
    "2085" = 658,  "2090" = 656,  "2095" = 655,  "2100" = 655
  ),
  high_cost = c(
    "2025" = 626,  "2030" = 204,  "2035" = 213,  "2040" = 218,
    "2045" = 221,  "2050" = 227,  "2055" = 234,  "2060" = 239,
    "2065" = 243,  "2070" = 245,  "2075" = 246,  "2080" = 247,
    "2085" = 247,  "2090" = 247,  "2095" = 247,  "2100" = 247
  )
)

# =============================================================================
# PROCESS 2: ECONOMICS
# =============================================================================
# Source: TR2025 Table V.B1

# ---- 2.1 Productivity & Real AWI growth -------------------------------------
# Annual percent change; ultimate values reached by 2034 and held flat through 2099

productivity_pct <- list(
  # Total economy productivity (output per hour)
  intermediate = list(
    transition = c(
      "2025"=1.26,"2026"=1.47,"2027"=1.46,"2028"=1.56,"2029"=1.51,
      "2030"=1.56,"2031"=1.63,"2032"=1.63,"2033"=1.63,"2034"=1.63
    ),
    ultimate = 1.63  # 2034 onward
  ),
  low_cost = list(
    transition = c(
      "2025"=1.28,"2026"=1.51,"2027"=1.55,"2028"=1.71,"2029"=1.91,
      "2030"=1.99,"2031"=1.98,"2032"=1.98,"2033"=1.96,"2034"=1.93
    ),
    ultimate = 1.93
  ),
  high_cost = list(
    transition = c(
      "2025"=0.70,"2026"=1.53,"2027"=1.49,"2028"=1.39,"2029"=1.33,
      "2030"=1.30,"2031"=1.33,"2032"=1.33,"2033"=1.33,"2034"=1.33
    ),
    ultimate = 1.33
  )
)

# Average annual wage in covered employment: nominal % change
# Real AWI = nominal AWI - CPI
awi_nominal_pct <- list(
  intermediate = list(
    transition = c(
      "2025"=3.97,"2026"=4.13,"2027"=4.03,"2028"=4.11,"2029"=3.94,
      "2030"=3.88,"2031"=3.93,"2032"=3.95,"2033"=3.96,"2034"=3.85
    ),
    ultimate = 3.56  # 2034 onward (= 1.13% real + 2.40% CPI)
  ),
  low_cost = list(
    transition = c(
      "2025"=5.10,"2026"=5.50,"2027"=5.28,"2028"=5.31,"2029"=5.38,
      "2030"=5.32,"2031"=5.23,"2032"=5.20,"2033"=5.23,"2034"=5.12
    ),
    ultimate = 4.78  # 1.73% real + 3.00% CPI
  ),
  high_cost = list(
    transition = c(
      "2025"=1.25,"2026"=2.22,"2027"=3.61,"2028"=3.49,"2029"=3.10,
      "2030"=2.81,"2031"=2.70,"2032"=2.72,"2033"=2.71,"2034"=2.59
    ),
    ultimate = 2.34  # 0.53% real + 1.80% CPI
  )
)

# ---- 2.2 Inflation (CPI) ----------------------------------------------------
cpi_pct <- list(
  intermediate = list(
    transition = c(
      "2025"=2.47,"2026"=2.49,"2027"=2.40,"2028"=2.40,"2029"=2.40,
      "2030"=2.40,"2031"=2.40,"2032"=2.40,"2033"=2.40,"2034"=2.40
    ),
    ultimate = 2.40
  ),
  low_cost  = list(transition = setNames(rep(3.00,10), 2025:2034), ultimate = 3.00),
  high_cost = list(transition = c(
    "2025"=2.21,"2026"=1.85,"2027"=1.80,"2028"=1.80,"2029"=1.80,
    "2030"=1.80,"2031"=1.80,"2032"=1.80,"2033"=1.80,"2034"=1.80
  ), ultimate = 1.80)
)

# ---- 2.3 GDP price index growth (used for some deflation) -------------------
gdp_deflator_pct <- list(
  intermediate = list(
    transition = c(
      "2025"=2.17,"2026"=2.07,"2027"=2.05,"2028"=2.05,"2029"=2.05,
      "2030"=2.05,"2031"=2.05,"2032"=2.05,"2033"=2.05,"2034"=2.05
    ),
    ultimate = 2.05
  ),
  low_cost  = list(transition = setNames(rep(2.75,10), 2025:2034), ultimate = 2.75),
  high_cost = list(transition = c(
    "2025"=1.86,"2026"=1.38,"2027"=1.35,"2028"=1.35,"2029"=1.35,
    "2030"=1.35,"2031"=1.35,"2032"=1.35,"2033"=1.35,"2034"=1.35
  ), ultimate = 1.35)
)

# ---- 2.4 LFP / covered worker rates ----------------------------------------
# SSA's full LFP model is sex-age-specific. We store the key macro parameter:
# the ratio of covered earnings to total compensation (affects effective payroll).
# Underlying LFP rates by age/sex come from 01_demography.R (as they feed
# population-weighted covered worker counts).

# Earnings as a percentage of total labor compensation (% point change/yr)
earnings_pct_compensation <- list(
  intermediate = list(
    transition = c("2025"=-0.12,"2026"=-0.08,"2027"=-0.08,"2028"=-0.08,
                   "2029"=-0.08,"2030"=-0.09,"2031"=-0.09,"2032"=-0.09,
                   "2033"=-0.09,"2034"=-0.09),
    ultimate = -0.09
  ),
  low_cost = list(
    transition = setNames(rep(0.00, 10), 2025:2034), ultimate = 0.00
  ),
  high_cost = list(
    transition = c("2025"=-0.12,"2026"=-0.07,"2027"=-0.10,"2028"=-0.12,
                   "2029"=-0.13,"2030"=-0.14,"2031"=-0.15,"2032"=-0.16,
                   "2033"=-0.17,"2034"=-0.18),
    ultimate = -0.17
  )
)

# ---- 2.5 Unemployment rate (ultimate) ---------------------------------------
unemployment_ult <- list(
  intermediate = 4.50,  # percent
  low_cost     = 3.50,
  high_cost    = 5.50
)

# =============================================================================
# PROCESS 4: TRUST FUND / INTEREST RATES
# =============================================================================
# Source: TR2025 Table II.C1 and V.C1

# ---- 4.1 Real interest rate (ultimate) --------------------------------------
# Annual yield on new special-issue bonds = CPI + real_rate
real_interest_rate_ult <- list(
  intermediate = 2.70,   # percent
  low_cost     = 3.30,
  high_cost    = 2.20
)

# Transition path for nominal new-issue yield (from V.C table / SSA bond model)
# Intermediate: new-issue yield grades from ~4.3% (2025) to ultimate ~5.1% (2040+)
# Simplified as: nominal_yield = CPI_ult + real_rate_ult (ultimate pair used after 2039)
# For transition 2025-2039, linear interpolation from short-range yield estimate
new_issue_yield_2025 <- list(
  intermediate = 4.30,   # estimated from SR office passthrough; %
  low_cost     = 4.60,
  high_cost    = 4.00
)

# Year at which new-issue yield reaches ultimate nominal rate
yield_transition_end_year <- list(
  intermediate = 2039,
  low_cost     = 2039,
  high_cost    = 2039
)

# ---- 4.2 Payroll tax rates --------------------------------------------------
# Current law (not varied unless modelling a reform scenario)
oasi_ee_rate <- 0.053   # 5.30% employer + employee each → combined 10.60%
di_ee_rate   <- 0.009   # 0.90% each → combined 1.80%
oasdi_ee_rate <- oasi_ee_rate + di_ee_rate   # 6.20% each

# Self-employed rate = 2x employee rate (but deduction effectively makes it ~EE rate)
oasdi_se_rate <- 0.124  # 12.40%

# ---- 4.3 Taxation-of-benefits (TOB) factors ---------------------------------
# Percent of scheduled benefits collected as income tax (long-run)
# Short-range values come from SR office; these are long-run ultimate factors
tob_factor_oasi <- list(
  intermediate = 0.073,   # ~7.3% of OASI benefits
  low_cost     = 0.068,
  high_cost    = 0.078
)
tob_factor_di <- list(
  intermediate = 0.025,
  low_cost     = 0.023,
  high_cost    = 0.027
)

# ---- 4.4 Administrative expense growth drivers ------------------------------
productivity_admin <- 0.016  # 1.6% annual productivity offset applied to admin costs

# ---- 4.5 Railroad interchange -----------------------------------------------
# Net annual cashflow to OASI / DI (billions, 2025 base; grows with AWI thereafter)
rr_cashflow_oasi_2025 <- -5.2   # negative = net receipt by OASI
rr_cashflow_di_2025   <- -0.8

# =============================================================================
# HELPER: extract assumption for active scenario
# =============================================================================

get_assumption <- function(assumption_list, scenario = SCENARIO) {
  if (!scenario %in% names(assumption_list)) {
    stop(paste("Scenario", scenario, "not found in assumption list"))
  }
  assumption_list[[scenario]]
}

# Convenience wrappers for the most commonly accessed assumptions
get_tfr_path   <- function() get_assumption(tfr_path)
get_lpr_net    <- function() get_assumption(lpr_net)
get_tup_net    <- function() get_assumption(tup_net)
get_real_rate  <- function() get_assumption(real_interest_rate_ult)
get_cpi_path   <- function() get_assumption(cpi_pct)
get_awi_path   <- function() get_assumption(awi_nominal_pct)

# =============================================================================
# CUSTOM SCENARIO OVERRIDES
# Edit this block to run non-standard scenarios.
# Leave as NULL to use the selected SCENARIO above.
# =============================================================================

custom_overrides <- list(
  # Example: hold TFR at 1.62 permanently
  # tfr_path = setNames(rep(1.62, 16),
  #                     c(2025,2030,2035,2040,2045,2050,2055,2060,
  #                       2065,2070,2075,2080,2085,2090,2095,2100)),

  # Example: raise real interest rate to 3.5%
  # real_interest_rate_ult = 3.50,

  # Example: cut LPR immigration to 500k permanently
  # lpr_net = setNames(c(700, rep(500, 15)),
  #                    c(2025,2030,2035,2040,2045,2050,2055,2060,
  #                      2065,2070,2075,2080,2085,2090,2095,2100)),
  NULL
)

# Apply custom overrides if scenario = "custom"
if (SCENARIO == "custom") {
  if (!is.null(custom_overrides$tfr_path))
    tfr_path[["custom"]] <- custom_overrides$tfr_path
  if (!is.null(custom_overrides$real_interest_rate_ult))
    real_interest_rate_ult[["custom"]] <- custom_overrides$real_interest_rate_ult
  if (!is.null(custom_overrides$lpr_net))
    lpr_net[["custom"]] <- custom_overrides$lpr_net
}

message(sprintf("[assumptions.R] Loaded. Scenario: %s | TFR ult: %.2f | Real rate: %.2f%% | LPR ult: %dk",
                SCENARIO,
                tail(get_tfr_path(), 1),
                get_real_rate(),
                tail(get_lpr_net(), 1)))
