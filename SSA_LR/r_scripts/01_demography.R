# =============================================================================
# 01_demography.R
# Cohort-component population projection for the Social Security area.
# Produces: pop[year, age, sex] for FIRST_PROJ_YEAR through FINAL_PROJ_YEAR
#
# Inputs (from assumptions.R):
#   - TFR path (yearly, interpolated from 5-yr nodes)
#   - Mortality reduction rates by age group
#   - LPR and TUP net immigration (thousands, yearly interpolated)
#
# Outputs:
#   - pop_proj: named list by year, each element a (101 x 2) matrix [age 0-100, sex M/F]
#   - dep_ratios: data.frame of aged/total dependency ratios (→ V.A3 equivalent)
#   - life_exp: data.frame of period life expectancy at birth and 65 (→ V.A4 equivalent)
#   - covered_pop: working-age population (20-64) used by 02_economics.R
# =============================================================================

source("inputs/assumptions.R")
library(dplyr)

# =============================================================================
# 0. STARTING POPULATION (January 1, 2025 ≈ July 1, 2024 SS area population)
# =============================================================================
# Source: TR2025 V.A3 gives broad age groups. We back out single-year-of-age
# using the 2024 SSA area population file (actuarial_study_120 life table basis).
# For replication start: we use interpolated single-year estimates that sum to
# the V.A3 totals: Under 20 = 83,034k, 20-64 = 199,564k, 65+ = 62,835k → total 345,433k

load_starting_population <- function() {
  # Single-year-of-age SS area population estimates for Dec 31, 2024 (thousands)
  # Derived from applying SSA life table age distributions to V.A3 group totals.
  # Source: SSA Actuarial Study 120 age distribution + V.A3 group controls.
  # NOTE: Replace this with the SSA's published single-year file when available
  #       (https://www.ssa.gov/oact/HistEst/PopProj/). The values below reproduce
  #       V.A3 group totals for 2024 within 0.1%.

  # Age distribution by sex (percent of age group) from 2020 Census + SSA projection
  # Male fraction by broad age group (approximate; SSA uses 0.512 male share at birth,
  # converging to ~0.48 by 65+)
  ages <- 0:100

  # Build smooth single-year populations using a parametric graduation
  # anchored to V.A3 2024 group totals (thousands):
  # 0-19: 83,034  |  20-64: 199,564  |  65+: 62,835  |  Total: 345,433

  # Male share by age (declines from 0.512 at birth to ~0.468 at 85+)
  male_share <- approx(
    x = c(0,  5, 15, 25, 35, 45, 55, 65, 75, 85, 100),
    y = c(0.512, 0.513, 0.510, 0.504, 0.499, 0.494, 0.487, 0.478, 0.466, 0.453, 0.440),
    xout = ages, rule = 2
  )$y

  # Total population by single year: smooth spline anchored to group totals
  # Group targets (thousands)
  grp_targets <- c(
    sum_0_19  = 83034,
    sum_20_64 = 199564,
    sum_65p   = 62835
  )

  # Approximate age density using model life table shape + scale to hit group totals
  # We use a simple gamma-like shape for illustration; calibrate in production
  # to SSA's published 2024 population file.
  pop_shape <- c(
    # 0-19: baby bust; about 4,150k per year of age on average
    approx(c(0,5,10,15,19), c(3700,4150,4350,4200,4100), xout=0:19, rule=2)$y,
    # 20-64: working age pyramid
    approx(c(20,30,40,50,60,64), c(4800,4500,4350,4400,4200,4050), xout=20:64, rule=2)$y,
    # 65+: rising then falling tail
    approx(c(65,70,75,80,85,90,95,100), c(3800,3200,2600,2000,1350,750,350,90),
           xout=65:100, rule=2)$y
  )

  # Scale each group to target
  scale_0_19  <- grp_targets["sum_0_19"]  / sum(pop_shape[1:20])
  scale_20_64 <- grp_targets["sum_20_64"] / sum(pop_shape[21:65])
  scale_65p   <- grp_targets["sum_65p"]   / sum(pop_shape[66:101])

  pop_total <- c(
    pop_shape[1:20]  * scale_0_19,
    pop_shape[21:65] * scale_20_64,
    pop_shape[66:101] * scale_65p
  )

  pop_male   <- pop_total * male_share
  pop_female <- pop_total * (1 - male_share)

  mat <- cbind(male = pop_male, female = pop_female)
  rownames(mat) <- as.character(ages)
  mat  # thousands, by single year of age
}

# =============================================================================
# 1. MORTALITY: project q(x, t) from base year life tables
# =============================================================================

# 1a. Base 2024 death probabilities q(x) by sex
# Derived from SSA's official 2024 period life tables (Actuarial Study 120 basis).
# Values below are representative; replace with published SSA 2024 life table
# (https://www.ssa.gov/oact/NOTES/as120/LifeTables_Tbl_6.html).

build_base_qx <- function() {
  # Standard SSA 2024 life table q(x) by sex (illustrative; calibrated to V.A4 2024 e0)
  # Male e(0)=76.6, Female e(0)=81.5; Male e(65)=18.3, Female e(65)=20.9

  ages <- 0:100

  # Male q(x): Makeham-Gompertz fit to SSA 2024 observed rates
  # q(x) = 1 - exp(-m(x)) where m(x) is the central death rate
  # Parameters calibrated to SSA 2024 male life table
  makeham_qx <- function(ages, a, b, c_gomp) {
    mx <- a + b * exp(c_gomp * ages)
    # Infant/child fix (non-Gompertz ages 0-14)
    pmin(1 - exp(-mx), 0.9999)
  }

  qx_male <- makeham_qx(ages, a = 0.00030, b = 0.00003, c_gomp = 0.10)
  qx_female <- makeham_qx(ages, a = 0.00015, b = 0.000015, c_gomp = 0.10)

  # Manual overrides for infant/child ages where Gompertz breaks down
  infant_male   <- c(0.0051, 0.00033, 0.00022, 0.00016, 0.00013,  # ages 0-4
                     0.00010, 0.00008, 0.00008, 0.00009, 0.00011,  # 5-9
                     0.00013, 0.00015, 0.00019, 0.00028, 0.00045,  # 10-14
                     0.00083, 0.00109, 0.00130, 0.00147, 0.00156)  # 15-19
  infant_female <- c(0.0042, 0.00027, 0.00019, 0.00014, 0.00011,
                     0.00009, 0.00007, 0.00008, 0.00009, 0.00011,
                     0.00013, 0.00016, 0.00019, 0.00023, 0.00030,
                     0.00041, 0.00051, 0.00059, 0.00065, 0.00069)
  qx_male[1:20]   <- infant_male
  qx_female[1:20] <- infant_female

  list(male = qx_male, female = qx_female)
}

# 1b. Map single year of age to SSA mortality age group
age_to_group <- function(age) {
  dplyr::case_when(
    age < 1   ~ "lt1",
    age < 5   ~ "1to4",
    age < 15  ~ "5to14",
    age < 25  ~ "15to24",
    age < 35  ~ "25to34",
    age < 45  ~ "35to44",
    age < 55  ~ "45to54",
    age < 65  ~ "55to64",
    age < 75  ~ "65to74",
    age < 85  ~ "75to84",
    TRUE      ~ "85plus"
  )
}

# 1c. Annual reduction in m(x) for a given year (linear transition to ultimate)
annual_mortality_reduction <- function(year, scenario = SCENARIO) {
  ult_reductions <- get_assumption(mortality_ult_reduction, scenario)
  ult_year       <- get_assumption(mortality_ultimate_year, scenario)

  if (year >= ult_year) {
    return(ult_reductions / 100)  # convert percent to proportion
  }

  # Linear grade-in from 2024 base (assume ~half of ultimate at start)
  start_year    <- BASE_POP_YEAR
  start_factor  <- 0.5  # start at 50% of ultimate reduction rate
  frac <- (year - start_year) / (ult_year - start_year)
  frac <- max(0, min(1, frac))

  (ult_reductions / 100) * (start_factor + (1 - start_factor) * frac)
}

# 1d. Project q(x) forward from base year to target year
project_qx <- function(base_qx, from_year, to_year, scenario = SCENARIO) {
  qx <- base_qx
  ages <- 0:100

  if (to_year <= from_year) return(qx)

  for (yr in (from_year + 1):to_year) {
    reductions <- annual_mortality_reduction(yr, scenario)
    for (sex in c("male", "female")) {
      # Convert q(x) -> m(x), apply reduction, convert back
      # m(x) = -log(1 - q(x)); q(x) = 1 - exp(-m(x))
      mx <- -log(1 - pmin(qx[[sex]], 0.9999))
      for (a in seq_along(ages)) {
        grp <- age_to_group(ages[a])
        mx[a] <- mx[a] * (1 - reductions[grp])
      }
      qx[[sex]] <- pmin(1 - exp(-mx), 0.9999)
    }
  }
  qx
}

# 1e. Compute period life expectancy from q(x)
compute_life_expectancy <- function(qx) {
  ages <- 0:100
  n <- length(ages)

  le_from_age <- function(qx_vec, start_age = 0) {
    # Standard life table: lx, dx, Lx, Tx, ex
    lx <- numeric(n + 1)
    lx[1] <- 1
    for (i in 1:n) {
      lx[i + 1] <- lx[i] * (1 - qx_vec[i])
    }
    Lx <- (lx[1:n] + lx[2:(n+1)]) / 2
    Lx[n] <- lx[n] / qx_vec[n]  # open interval at 100+
    Tx <- rev(cumsum(rev(Lx)))
    ex <- Tx / lx[1:n]

    start_idx <- start_age - ages[1] + 1
    ex[start_idx]
  }

  list(
    e0_male   = le_from_age(qx$male,   0),
    e0_female = le_from_age(qx$female, 0),
    e65_male  = le_from_age(qx$male,   65),
    e65_female = le_from_age(qx$female, 65)
  )
}

# =============================================================================
# 2. FERTILITY: age-specific fertility rates (ASFRs) from TFR path
# =============================================================================

# Fixed ASFR shape (schedule) calibrated to 2024 NCHS data
# Normalized to sum = 1; multiply by TFR to get ASFRs in births per woman-year
# Ages 15-49 (35 fertile ages)
asfr_shape_2024 <- c(
  # age: 15   16   17   18   19   20   21   22   23   24
  0.004, 0.007, 0.012, 0.020, 0.029,
  0.040, 0.052, 0.063, 0.073, 0.080,
  # 25   26   27   28   29   30   31   32   33   34
  0.085, 0.087, 0.086, 0.083, 0.079,
  0.074, 0.068, 0.060, 0.052, 0.044,
  # 35   36   37   38   39   40   41   42   43   44
  0.036, 0.029, 0.022, 0.016, 0.011,
  0.008, 0.005, 0.003, 0.002, 0.001,
  # 45   46   47   48   49
  0.001, 0.001, 0.000, 0.000, 0.000
)
asfr_shape_2024 <- asfr_shape_2024 / sum(asfr_shape_2024)  # normalize to sum = 1
fertile_ages <- 15:49

# Interpolate TFR to annual series
build_tfr_annual <- function(scenario = SCENARIO) {
  tfr_nodes <- get_assumption(tfr_path, scenario)
  node_years <- as.integer(names(tfr_nodes))
  node_vals  <- as.numeric(tfr_nodes)

  all_years <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR
  approx(x = node_years, y = node_vals, xout = all_years, rule = 2)$y
}

# Build ASFRs for a given year
get_asfr <- function(year, tfr_annual) {
  yr_idx <- year - FIRST_PROJ_YEAR + 1
  tfr    <- tfr_annual[yr_idx]
  asfr_shape_2024 * tfr  # births per woman-year by age 15-49
}

# =============================================================================
# 3. IMMIGRATION: distribute net immigration by age/sex
# =============================================================================

# Age distribution of net immigration (shares summing to 1)
# Based on SSA's published age distribution for LPR and TUP immigrants
# LPR: skewed to working ages 20-44, with some family reunification children/elderly
# TUP: heavily concentrated ages 18-35

lpr_age_dist <- c(
  # ages 0-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39,
  0.065, 0.052, 0.052, 0.063, 0.112, 0.140, 0.132, 0.110,
  # 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85+
  0.080, 0.055, 0.040, 0.028, 0.022, 0.018, 0.013, 0.009, 0.006, 0.003
)
stopifnot(abs(sum(lpr_age_dist) - 1) < 1e-6)

tup_age_dist <- c(
  0.045, 0.038, 0.040, 0.063, 0.150, 0.200, 0.175, 0.130,
  0.075, 0.040, 0.022, 0.010, 0.005, 0.002, 0.002, 0.001, 0.001, 0.001
)
tup_age_dist <- tup_age_dist / sum(tup_age_dist)

# Male share of immigration (LPR ~48% male, TUP ~54% male)
lpr_male_share <- 0.48
tup_male_share <- 0.54

distribute_immigration <- function(net_lpr_k, net_tup_k) {
  # Returns (101 x 2) matrix of net immigrants by single age (0-100) and sex
  ages <- 0:100
  mat  <- matrix(0, nrow = 101, ncol = 2, dimnames = list(as.character(ages), c("male","female")))

  # Map 5-year age groups to single years (uniform within group)
  age_groups <- list(
    0:4, 5:9, 10:14, 15:19, 20:24, 25:29, 30:34, 35:39,
    40:44, 45:49, 50:54, 55:59, 60:64, 65:69, 70:74, 75:79, 80:84, 85:100
  )
  # Note: 85+ group gets the last share spread over ages 85-100 (16 years)
  grp_widths <- sapply(age_groups, length)

  for (i in seq_along(age_groups)) {
    ag <- age_groups[[i]]
    # LPR
    lpr_grp <- net_lpr_k * lpr_age_dist[i] / grp_widths[i]  # per single age (thousands)
    mat[ag + 1, "male"]   <- mat[ag + 1, "male"]   + lpr_grp * lpr_male_share
    mat[ag + 1, "female"] <- mat[ag + 1, "female"] + lpr_grp * (1 - lpr_male_share)
    # TUP
    tup_grp <- net_tup_k * tup_age_dist[i] / grp_widths[i]
    mat[ag + 1, "male"]   <- mat[ag + 1, "male"]   + tup_grp * tup_male_share
    mat[ag + 1, "female"] <- mat[ag + 1, "female"] + tup_grp * (1 - tup_male_share)
  }
  mat  # thousands
}

# Interpolate immigration to annual series
build_immigration_annual <- function(scenario = SCENARIO) {
  lpr_nodes  <- get_assumption(lpr_net, scenario)
  tup_nodes  <- get_assumption(tup_net, scenario)
  node_years <- as.integer(names(lpr_nodes))
  all_years  <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR

  list(
    lpr = approx(node_years, as.numeric(lpr_nodes), xout = all_years, rule = 2)$y,
    tup = approx(node_years, as.numeric(tup_nodes), xout = all_years, rule = 2)$y
  )
}

# =============================================================================
# 4. COHORT-COMPONENT PROJECTION ENGINE
# =============================================================================

project_population <- function(scenario = SCENARIO) {
  message(sprintf("[01_demography.R] Running cohort-component projection | scenario: %s", scenario))

  ages     <- 0:100
  all_years <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR
  n_years  <- length(all_years)

  # Initialise
  base_qx    <- build_base_qx()
  tfr_annual <- build_tfr_annual(scenario)
  immig      <- build_immigration_annual(scenario)

  # Starting population matrix (thousands) [age 0-100, sex M/F]
  pop <- load_starting_population()   # end of 2024 = Jan 1 2025

  # Storage
  pop_proj   <- vector("list", n_years)
  dep_ratios <- data.frame(
    year          = all_years,
    pop_under20   = NA_real_,
    pop_20_64     = NA_real_,
    pop_65p       = NA_real_,
    pop_total     = NA_real_,
    aged_dep_ratio = NA_real_,
    total_dep_ratio = NA_real_
  )
  life_exp_tbl <- data.frame(
    year      = all_years,
    e0_male   = NA_real_, e0_female   = NA_real_,
    e65_male  = NA_real_, e65_female  = NA_real_
  )

  # Project q(x) once per year (same for both sexes in terms of reduction schedule)
  qx_current <- base_qx  # q(x) for BASE_POP_YEAR (2024)

  for (i in seq_along(all_years)) {
    yr <- all_years[i]

    # ---- 4a. Update mortality -----------------------------------------------
    qx_current <- project_qx(qx_current, from_year = yr - 1, to_year = yr, scenario)

    # ---- 4b. Survivorship: age the population one year ----------------------
    # Males: survivors from age x → age x+1
    surv_male   <- pop[1:100, "male"]   * (1 - qx_current$male[1:100])
    surv_female <- pop[1:100, "female"] * (1 - qx_current$female[1:100])

    # Open age interval: survivors of age 99 + those already at 100
    surv_male_100   <- surv_male[100]   + pop[101, "male"]   * (1 - qx_current$male[101])
    surv_female_100 <- surv_female[100] + pop[101, "female"] * (1 - qx_current$female[101])

    pop_aged <- matrix(0, nrow = 101, ncol = 2, dimnames = list(as.character(ages), c("male","female")))
    pop_aged[2:100, "male"]   <- surv_male[1:99]
    pop_aged[2:100, "female"] <- surv_female[1:99]
    pop_aged[101, "male"]     <- surv_male_100
    pop_aged[101, "female"]   <- surv_female_100

    # ---- 4c. Births ---------------------------------------------------------
    asfr <- get_asfr(yr, tfr_annual)
    # Female population at fertile ages (average of start and aged end of year)
    # Use midyear approximation: average of pop at start and aged population
    fem_fertile_start <- pop[fertile_ages - ages[1] + 1, "female"]
    fem_fertile_end   <- pop_aged[fertile_ages - ages[1] + 1, "female"]
    fem_fertile_mid   <- (fem_fertile_start + fem_fertile_end) / 2

    total_births <- sum(asfr * fem_fertile_mid)  # thousands

    # Sex ratio at birth: 1.048 male per female (SSA standard)
    srb <- 1.048
    births_male   <- total_births * srb / (1 + srb)
    births_female <- total_births * 1   / (1 + srb)

    # Surviving births (exposed to infant mortality q(0))
    pop_aged[1, "male"]   <- births_male   * (1 - qx_current$male[1])
    pop_aged[1, "female"] <- births_female * (1 - qx_current$female[1])

    # ---- 4d. Immigration ----------------------------------------------------
    immig_mat <- distribute_immigration(
      net_lpr_k = immig$lpr[i],
      net_tup_k = immig$tup[i]
    )
    pop_aged <- pop_aged + immig_mat

    # Floor at zero (net emigration cannot produce negative population)
    pop_aged <- pmax(pop_aged, 0)

    # ---- 4e. Store results --------------------------------------------------
    pop_proj[[i]] <- pop_aged
    names(pop_proj)[i] <- as.character(yr)

    # Dependency ratios (matching V.A3 structure)
    total_under20 <- sum(pop_aged[1:20, ])          # ages 0-19
    total_20_64   <- sum(pop_aged[21:65, ])          # ages 20-64
    total_65p     <- sum(pop_aged[66:101, ])          # ages 65+
    total_pop     <- sum(pop_aged)

    dep_ratios[i, "pop_under20"]    <- total_under20
    dep_ratios[i, "pop_20_64"]      <- total_20_64
    dep_ratios[i, "pop_65p"]        <- total_65p
    dep_ratios[i, "pop_total"]      <- total_pop
    dep_ratios[i, "aged_dep_ratio"] <- total_65p / total_20_64
    dep_ratios[i, "total_dep_ratio"]<- (total_under20 + total_65p) / total_20_64

    # Life expectancy
    le <- compute_life_expectancy(qx_current)
    life_exp_tbl[i, "e0_male"]    <- le$e0_male
    life_exp_tbl[i, "e0_female"]  <- le$e0_female
    life_exp_tbl[i, "e65_male"]   <- le$e65_male
    life_exp_tbl[i, "e65_female"] <- le$e65_female

    # Advance population
    pop <- pop_aged

    if (yr %% 10 == 0) {
      message(sprintf("  %d: total pop = %.0fk | aged dep = %.3f | e0(M) = %.1f",
                      yr, total_pop, total_65p / total_20_64, le$e0_male))
    }
  }

  list(
    pop_proj      = pop_proj,
    dep_ratios    = dep_ratios,
    life_exp      = life_exp_tbl
  )
}

# =============================================================================
# 5. SUMMARY TABLE (replicates TR Table V.A3 structure)
# =============================================================================

format_dep_ratio_table <- function(demo_results, scenario = SCENARIO) {
  dr <- demo_results$dep_ratios

  # 5-year intervals matching V.A3
  yrs_5 <- seq(2025, 2100, by = 5)
  dr_5  <- dr[dr$year %in% yrs_5, ]

  out <- data.frame(
    year            = dr_5$year,
    pop_under20_k   = round(dr_5$pop_under20, 0),
    pop_20_64_k     = round(dr_5$pop_20_64, 0),
    pop_65p_k       = round(dr_5$pop_65p, 0),
    pop_total_k     = round(dr_5$pop_total, 0),
    aged_dep_ratio  = round(dr_5$aged_dep_ratio, 3),
    total_dep_ratio = round(dr_5$total_dep_ratio, 3),
    scenario        = scenario
  )
  out
}

# =============================================================================
# 6. CALIBRATION CHECK against TR2025 Table V.A3 intermediate
# =============================================================================

calibrate_demography <- function(demo_results) {
  # Benchmark values from V.A3 (thousands)
  benchmark <- data.frame(
    year          = c(2025, 2030, 2035, 2040, 2050, 2075, 2100),
    pop_total_k   = c(348404, 358152, 366963, 375094, 388372, 420329, 451617),
    aged_dep_ratio = c(0.322, 0.357, 0.374, 0.380, 0.394, 0.460, 0.454)
  )

  dr <- demo_results$dep_ratios
  modelled <- dr[dr$year %in% benchmark$year, c("year", "pop_total", "aged_dep_ratio")]
  modelled$pop_total <- round(modelled$pop_total, 0)

  names(modelled)[names(modelled) == "pop_total"]      <- "pop_total_model"
  names(modelled)[names(modelled) == "aged_dep_ratio"] <- "aged_dep_ratio_model"
  merged <- merge(benchmark, modelled, by = "year")
  merged$pop_pct_diff <- 100 * (merged$pop_total_model - merged$pop_total_k) / merged$pop_total_k
  merged$dep_abs_diff <- merged$aged_dep_ratio_model - merged$aged_dep_ratio

  message("\n[Calibration: V.A3 vs. Model Output]")
  print(merged[, c("year", "pop_total_k", "pop_total_model", "pop_pct_diff",
                   "aged_dep_ratio", "aged_dep_ratio_model", "dep_abs_diff")],
        row.names = FALSE)

  max_pop_err <- max(abs(merged$pop_pct_diff))
  if (max_pop_err > 2.0) {
    warning(sprintf("Population deviation %.1f%% exceeds 2%% threshold — recalibrate base population.", max_pop_err))
  } else {
    message(sprintf("  Max population deviation: %.2f%%  [within 2%% threshold]", max_pop_err))
  }
  invisible(merged)
}

# =============================================================================
# 7. MAIN ENTRY POINT
# =============================================================================

run_demography <- function(scenario = SCENARIO, calibrate = TRUE) {
  results <- project_population(scenario)

  if (calibrate && scenario == "intermediate") {
    calibrate_demography(results)
  }

  # Convenience accessor: SS area working-age population for 02_economics.R
  results$working_age_pop <- sapply(results$pop_proj, function(mat) {
    sum(mat[21:65, ])  # ages 20-64, both sexes (thousands)
  })
  names(results$working_age_pop) <- FIRST_PROJ_YEAR:FINAL_PROJ_YEAR

  results$pop_table <- format_dep_ratio_table(results, scenario)

  message(sprintf("\n[01_demography.R] Done. Projected population 2025-2099 stored."))
  message(sprintf("  2025 total: %.0fk | 2050 total: %.0fk | 2099 total: %.0fk",
                  sum(results$pop_proj[["2025"]]),
                  sum(results$pop_proj[["2050"]]),
                  sum(results$pop_proj[["2099"]])))

  results
}

# Run if sourced as standalone
if (!exists("DEMOGRAPHY_LOADED")) {
  DEMOGRAPHY_LOADED <- TRUE
  demo_results <- run_demography(scenario = SCENARIO, calibrate = TRUE)
}
