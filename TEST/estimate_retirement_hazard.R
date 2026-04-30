### Estimate Retirement Hazard Model using the SSA Benefits and            ###
### Earnings Public-Use File (BEPUF).                                       ###
###                                                                         ###
### The BEPUF contains:                                                     ###
###   - Actual annual earnings from W-2 / SE tax records (up to 2006)      ###
###   - OASDI benefit award year and monthly benefit amount                 ###
###   - Sex and birth year (no education or marital status in admin data)   ###
###   - ~1% sample of SSA records                                           ###
###                                                                         ###
### Estimation approach:                                                    ###
###   Expand each person into a discrete-time person-year panel             ###
###   from age 62 to min(award_age, 70). Outcome = 1 in the year they      ###
###   first claim. Estimate a logit hazard with SSW incentive measures      ###
###   as regressors. Education / marital status not available in BEPUF      ###
###   so fixed effects are restricted to sex × birth_decade.               ###
###                                                                         ###
### BEPUF file layout (SSA documentation varies by release year;            ###
### the 2006 BEPUF column names below follow SSA's codebook):               ###
###   SSN_MASKED / CASE_ID : anonymised record ID                           ###
###   BDATE_YR             : birth year                                     ###
###   SEX                  : 1=male 2=female                                ###
###   EARN_{YYYY}          : annual covered earnings for year YYYY          ###
###   AWARD_YR             : calendar year of first OASDI benefit           ###
###   AWARD_AGE            : age at first benefit (= award_yr - bdate_yr)   ###
###   PIA_AWARD            : PIA at award (monthly, nominal)                ###
###   BEN_AWARD            : first monthly benefit (nominal)                ###
###   BEND1_{YYYY}, BEND2_{YYYY} : bend-point $s for the award year        ###

library(dplyr)
library(tidyr)
library(fixest)
library(reticulate)

setwd("C:/Users/kchanwong/Documents/TEST")

# ─────────────────────────────────────────────────────────────────────────────
# 1. LOAD BEPUF
#    Adjust path/column names to match your actual BEPUF release.
#    The 2006 BEPUF is a fixed-width file; SSA provides a SAS read-in program.
#    Convert to CSV or use haven::read_sas() on the pre-converted SAS dataset.
# ─────────────────────────────────────────────────────────────────────────────
bepuf_raw <- read.csv("C:/Users/kchanwong/Documents/TEST/bepuf2006.csv",
                      stringsAsFactors = FALSE)

# Rename to standard internal names if needed
# bepuf_raw <- bepuf_raw %>% rename(CASE_ID = ..., SEX = ..., etc.)

# ─────────────────────────────────────────────────────────────────────────────
# 2. CLEAN AND RESTRICT SAMPLE
#    Keep retired-worker and delayed retirement credits only (OASI, not DI).
#    Restrict to those who claimed between ages 62 and 70.
# ─────────────────────────────────────────────────────────────────────────────
bepuf <- bepuf_raw %>%
  mutate(
    CLAIM_AGE  = AWARD_YR - BDATE_YR,
    BIRTH_DEC  = (BDATE_YR %/% 10L) * 10L,   # birth decade as cohort proxy
    # OASI-only flag: exclude disability conversions (some files have BEN_TYPE)
    OASI_ONLY  = if ("BEN_TYPE" %in% names(.)) BEN_TYPE %in% c("OA", "RIB") else TRUE
  ) %>%
  filter(
    OASI_ONLY,
    CLAIM_AGE >= 62L, CLAIM_AGE <= 70L,
    !is.na(SEX),
    BDATE_YR >= 1920L, BDATE_YR <= 1944L   # observed claiming window in data
  )

cat(sprintf("BEPUF sample: %d individuals, claiming ages %d–%d\n",
            nrow(bepuf), min(bepuf$CLAIM_AGE), max(bepuf$CLAIM_AGE)))

# ─────────────────────────────────────────────────────────────────────────────
# 3. AWI TABLE  (for indexing earnings to 2008 base)
# ─────────────────────────────────────────────────────────────────────────────
AWI_VEC <- c(
  "1951"=2799.16,"1952"=2973.32,"1953"=3139.44,"1954"=3155.64,
  "1955"=3301.44,"1956"=3532.36,"1957"=3641.72,"1958"=3673.80,
  "1959"=3855.80,"1960"=4007.12,"1961"=4086.76,"1962"=4291.40,
  "1963"=4396.64,"1964"=4576.32,"1965"=4658.72,"1966"=4938.36,
  "1967"=5213.44,"1968"=5571.76,"1969"=5893.76,"1970"=6186.24,
  "1971"=6497.08,"1972"=7133.80,"1973"=7580.16,"1974"=8030.76,
  "1975"=8630.92,"1976"=9226.48,"1977"=9779.44,"1978"=10556.03,
  "1979"=11479.46,"1980"=12513.46,"1981"=13773.10,"1982"=14531.34,
  "1983"=15239.24,"1984"=16135.07,"1985"=16822.51,"1986"=17321.82,
  "1987"=18426.51,"1988"=19334.04,"1989"=20099.55,"1990"=21027.98,
  "1991"=21811.60,"1992"=22935.42,"1993"=23132.67,"1994"=23753.53,
  "1995"=24705.66,"1996"=25913.90,"1997"=27426.00,"1998"=28861.44,
  "1999"=30469.84,"2000"=32154.82,"2001"=32921.92,"2002"=33252.09,
  "2003"=34064.95,"2004"=35648.55,"2005"=36952.94,"2006"=38651.41,
  "2007"=40405.48,"2008"=41334.97
)
AWI_BASE_2008 <- AWI_VEC["2008"]

# ─────────────────────────────────────────────────────────────────────────────
# 4. COMPUTE ACTUAL AIME FROM EARNINGS HISTORIES
#
#    BEPUF has EARN_{YYYY} columns for each year.
#    AIME = average of top 35 years of AWI-indexed annual earnings / 12.
#    Earnings are capped at the OASDI taxable maximum before indexing.
#    (Taxable max is automatically capped in admin records.)
# ─────────────────────────────────────────────────────────────────────────────

# Identify all earnings columns (names like EARN_1980, EARN_1981, ...)
earn_cols <- grep("^EARN_[0-9]{4}$", names(bepuf), value = TRUE)
earn_years <- as.integer(sub("EARN_", "", earn_cols))

# For each person, index each year's earnings to 2008 and keep top 35
compute_aime <- function(earn_row, award_yr) {
  # Index year = 2 years before entitlement year (SSA rule)
  # Earnings after (award_yr - 2) are not indexed — use nominal
  index_year <- award_yr - 2L
  indexed <- vapply(seq_along(earn_cols), function(j) {
    yr  <- earn_years[j]
    raw <- earn_row[j]
    if (is.na(raw) || raw <= 0) return(0.0)
    if (yr <= index_year) {
      awi_yr <- AWI_VEC[as.character(yr)]
      if (is.null(awi_yr) || is.na(awi_yr)) return(raw)   # pre-table: use nominal
      raw * (AWI_BASE_2008 / awi_yr)
    } else {
      raw   # post-index-year: use nominal
    }
  }, numeric(1))
  # Top-35-year average divided by 12
  sum(sort(indexed, decreasing = TRUE)[1:min(35, sum(indexed > 0))]) / 12.0
}

# Apply row-wise (slow but correct; vectorise if needed)
bepuf$AIME <- mapply(
  compute_aime,
  split(as.matrix(bepuf[, earn_cols]), seq_len(nrow(bepuf))),
  bepuf$AWARD_YR
)

cat(sprintf("AIME: median = $%.0f/mo, mean = $%.0f/mo\n",
            median(bepuf$AIME), mean(bepuf$AIME)))

# ─────────────────────────────────────────────────────────────────────────────
# 5. PIA AND SSW MACHINERY  (mirrors retirement_hazard.py exactly)
# ─────────────────────────────────────────────────────────────────────────────

PIA_BEND <- list(
  "2008"=c(711,4288),  "2009"=c(744,4483),  "2010"=c(761,4586),
  "2011"=c(749,4517),  "2012"=c(767,4624),  "2013"=c(791,4768),
  "2014"=c(816,4917),  "2015"=c(826,4980),  "2016"=c(856,5157),
  "2017"=c(885,5336),  "2018"=c(895,5397),  "2019"=c(926,5583),
  "2020"=c(960,5785),  "2021"=c(996,6002),  "2022"=c(1024,6172),
  "2023"=c(1115,6721), "2024"=c(1174,7078)
)
# For older award years, use 2008 bend points (earliest in table)
get_bend <- function(year) {
  yr_str <- as.character(pmax(pmin(year, 2024L), 2008L))
  PIA_BEND[[yr_str]]
}

compute_pia <- function(aime, year) {
  bp <- get_bend(year); b1 <- bp[1]; b2 <- bp[2]
  0.90 * pmin(aime, b1) +
  0.32 * pmax(0, pmin(aime - b1, b2 - b1)) +
  0.15 * pmax(0, aime - b2)
}

full_retirement_age <- function(birth_year) {
  fra <- rep(NA_real_, length(birth_year))
  fra[birth_year <= 1937] <- 65.0
  m1 <- birth_year > 1937 & birth_year <= 1942
  fra[m1] <- 65 + (birth_year[m1] - 1937) * 2 / 12
  fra[birth_year > 1942 & birth_year <= 1954] <- 66.0
  m2 <- birth_year > 1954 & birth_year <= 1959
  fra[m2] <- 66 + (birth_year[m2] - 1954) * 2 / 12
  fra[birth_year > 1959] <- 67.0
  fra
}

claiming_adj <- function(claim_age, fra) {
  md  <- round((claim_age - fra) * 12)
  me  <- pmax(0, -md)
  ea  <- ifelse(me <= 36,
           1 - (5/9/100)*me,
           1 - (5/9/100)*36 - (5/12/100)*(me-36))
  da  <- 1 + 0.08 * pmax(0, md) / 12
  pmin(pmax(ifelse(md >= 0, da, ea), 0.40), 1.32)
}

# Gompertz annuity factors (same parameterisation as before)
ssa_qx <- function(sex, age) {
  age <- pmin(pmax(age, 0), 110)
  if (sex == 1L) { modal <- 82.0; scale <- 10.5 }
  else           { modal <- 86.5; scale <- 11.0  }
  1 - exp(-exp((age - modal)/scale) * (1/scale))
}
build_annuity_table_r <- function(discount_rate = 0.03) {
  tbl <- list()
  for (sex in c(1L, 2L)) {
    for (start_age in 55:109) {
      pv <- 0; surv <- 1; disc <- 1
      for (age in start_age:110) {
        pv   <- pv + surv * disc
        q    <- ssa_qx(sex, age)
        surv <- surv * (1 - q)
        disc <- disc / (1 + discount_rate)
        if (surv < 1e-6) break
      }
      tbl[[paste0(sex,"_",start_age)]] <- pv
    }
  }
  tbl
}
ann_tbl <- build_annuity_table_r()

ssw_scalar <- function(aime, sex, birth_year, claim_age, award_yr,
                       discount_years = 0) {
  # discount_years: years from current age to claiming age (for projection)
  fra    <- full_retirement_age(birth_year)
  adj    <- claiming_adj(pmax(pmin(claim_age, 70), 62), fra)
  pia    <- compute_pia(aime, award_yr)
  key    <- paste0(sex, "_", pmax(pmin(as.integer(claim_age), 109L), 55L))
  af     <- vapply(key, function(k) { v <- ann_tbl[[k]]; if(is.null(v)) 0 else v }, numeric(1))
  pia * 12 * adj * af * (1/(1.03)^discount_years)
}

# ─────────────────────────────────────────────────────────────────────────────
# 6. EXPAND INTO PERSON-YEAR PANEL
#    Each person contributes one row per age from 62 to CLAIM_AGE.
#    RETIRE = 1 in the final row (the year they actually claim).
#
#    Expected SSW at each decision age:
#      We observe actual AIME at claim. For ages BEFORE claiming, project
#      AIME backward using the age-efficiency approximation:
#        AIME(a) ≈ AIME_claim × (n_at_a / n_at_claim)
#      where n = min(a - 21, 35). This is conservative; a full projection
#      would require the annual earnings columns (which we do have — but this
#      simplified approach avoids looping over 35 earnings years per person-age).
#
#    Accrual = E[SSW(a+1)] - E[SSW(a)]
#    PeakValue = max_{T in {a,...,70}} E[SSW(T)] - E[SSW(a)]
# ─────────────────────────────────────────────────────────────────────────────
panel_rows <- vector("list", nrow(bepuf))

for (i in seq_len(nrow(bepuf))) {
  row      <- bepuf[i, ]
  max_age  <- min(row$CLAIM_AGE, 70L)
  ages     <- 62L:max_age
  n_rows   <- length(ages)

  n_claim  <- pmin(row$CLAIM_AGE - 21L, 35L)

  df <- data.frame(
    CASE_ID    = row$CASE_ID,
    SEX        = row$SEX,
    BIRTH_YR   = row$BDATE_YR,
    BIRTH_DEC  = row$BIRTH_DEC,
    CLAIM_AGE  = row$CLAIM_AGE,
    AGE        = ages,
    RETIRE     = as.integer(ages == max_age),
    stringsAsFactors = FALSE
  )

  # AIME approximation at each decision age (rolling back from claimed AIME)
  n_at_age     <- pmin(ages - 21L, 35L)
  df$AIME_CURR <- row$AIME * n_at_age / pmax(n_claim, 1L)

  # SSW at each age (immediate claiming)
  df$SSW_NOW   <- ssw_scalar(df$AIME_CURR, df$SEX, df$BIRTH_YR,
                              df$AGE, row$AWARD_YR)

  # SSW at age+1 (one-year accrual, AIME updated one more year)
  n_next        <- pmin(ages + 1L - 21L, 35L)
  aime_next     <- row$AIME * n_next / pmax(n_claim, 1L)
  df$SSW_NEXT   <- ssw_scalar(aime_next, df$SEX, df$BIRTH_YR,
                               df$AGE + 1L, row$AWARD_YR, discount_years = 1)

  df$ACCRUAL    <- df$SSW_NEXT - df$SSW_NOW
  df$ACCR_RATE  <- df$ACCRUAL  / pmax(df$SSW_NOW, 1)

  # Peak value: max SSW over future claiming ages T in {age, ..., 70}
  # Approximation: compute SSW at each T ∈ {age,...,70} for this row
  pv_vec <- sapply(ages, function(a) {
    max_ssw <- -Inf
    n_a     <- pmin(a - 21L, 35L)
    for (T in a:70L) {
      n_T   <- pmin(T - 21L, 35L)
      aime_T <- row$AIME * n_T / pmax(n_claim, 1L)
      s     <- ssw_scalar(aime_T, row$SEX, row$BDATE_YR, T,
                          row$AWARD_YR, discount_years = T - a)
      if (s > max_ssw) max_ssw <- s
    }
    max_ssw - ssw_scalar(row$AIME * n_a / pmax(n_claim,1L),
                         row$SEX, row$BDATE_YR, a, row$AWARD_YR)
  })
  df$PEAK_VALUE      <- pv_vec
  df$PEAK_VALUE_RATE <- pv_vec / pmax(df$SSW_NOW, 1)

  panel_rows[[i]] <- df
}

panel <- bind_rows(panel_rows)
panel$AGE2       <- panel$AGE^2
panel$LOG_SSW    <- log(pmax(panel$SSW_NOW, 1))

cat(sprintf("\nPanel: %d person-year obs  |  retirements = %d  (%.1f%%)\n",
            nrow(panel),
            sum(panel$RETIRE),
            100 * mean(panel$RETIRE)))

# ─────────────────────────────────────────────────────────────────────────────
# 7. FIT RETIREMENT HAZARD MODELS
#
#    Model A: Accrual specification (one-year incentive measure)
#    Model B: Peak Value specification (Gruber & Wise 1998)
#    Both include:  log(SSW) + incentive + AGE + AGE² | SEX + BIRTH_DEC
#
#    Note: BEPUF has no education or marital status. We use sex × birth-decade
#    fixed effects as the best available demographic controls.
# ─────────────────────────────────────────────────────────────────────────────

# Model A — Accrual
hazard_A <- feglm(
  RETIRE ~ LOG_SSW + ACCR_RATE + AGE + AGE2 | SEX + BIRTH_DEC,
  data   = panel,
  family = binomial(link = "logit")
)
cat("\n── Model A (Accrual) ───────────────────────────────────────\n")
summary(hazard_A)

# Model B — Peak Value
hazard_B <- feglm(
  RETIRE ~ LOG_SSW + PEAK_VALUE_RATE + AGE + AGE2 | SEX + BIRTH_DEC,
  data   = panel,
  family = binomial(link = "logit")
)
cat("\n── Model B (Peak Value / Gruber-Wise) ─────────────────────\n")
summary(hazard_B)

# Sanity: fitted retirement rate by age should match raw claiming distribution
pred_check <- panel %>%
  group_by(AGE) %>%
  summarise(
    n         = n(),
    obs_rate  = mean(RETIRE),
    pred_A    = mean(predict(hazard_A, type = "response")),
    pred_B    = mean(predict(hazard_B, type = "response"))
  )
print(pred_check)

# ─────────────────────────────────────────────────────────────────────────────
# 8. EXTRACT COEFFICIENT DICTS FOR PYTHON
# ─────────────────────────────────────────────────────────────────────────────
extract_hazard_dict <- function(model, incentive_name) {
  betas <- coef(model)
  fe    <- fixef(model)
  # Build incentive beta with a generic key name Python knows
  incentive_beta        <- list()
  incentive_beta[[incentive_name]] <- unname(betas[incentive_name])

  out <- list(
    beta_ssw_log      = unname(betas["LOG_SSW"]),
    beta_age          = unname(betas["AGE"]),
    beta_age2         = unname(betas["AGE2"]),
    fixef_sex         = as.list(fe[["SEX"]]),
    fixef_birth_dec   = as.list(fe[["BIRTH_DEC"]]),
    # BEPUF has no education/marital status:
    fixef_educ        = list(),
    fixef_marst       = list(),
    outcome           = "binomial",
    family            = "binomial",
    incentive_var     = incentive_name,   # tells Python which column to pass
    n_obs             = as.integer(nobs(model))
  )
  # Merge incentive slope into main dict
  out[[paste0("beta_", incentive_name)]] <- incentive_beta[[incentive_name]]
  # Alias so Python predict_retirement_prob() finds the right key
  if (incentive_name == "ACCR_RATE")        out$beta_accrual_rate    <- out$beta_ACCR_RATE
  if (incentive_name == "PEAK_VALUE_RATE")  out$beta_peak_value_rate <- out$beta_PEAK_VALUE_RATE
  out
}

hazard_dict_A <- extract_hazard_dict(hazard_A, "ACCR_RATE")
hazard_dict_B <- extract_hazard_dict(hazard_B, "PEAK_VALUE_RATE")

# ─────────────────────────────────────────────────────────────────────────────
# 9. APPEND BOTH MODELS TO fitted_models.pkl
# ─────────────────────────────────────────────────────────────────────────────
pickle   <- import("pickle")
builtins <- import_builtins()

pkl_path <- file.path(getwd(), "fitted_models.pkl")

fh_r     <- builtins$open(pkl_path, "rb")
bundle   <- pickle$load(fh_r)
fh_r$close()

bundle[["hazard_model"]]   <- r_to_py(hazard_dict_A)   # default: accrual spec
bundle[["hazard_model_B"]] <- r_to_py(hazard_dict_B)   # peak-value spec

fh_w <- builtins$open(pkl_path, "wb")
pickle$dump(bundle, fh_w)
fh_w$close()

message(sprintf("Updated fitted_models.pkl  (%s)", pkl_path))

# ─────────────────────────────────────────────────────────────────────────────
# 10. ROUND-TRIP VERIFICATION
# ─────────────────────────────────────────────────────────────────────────────
fh_v     <- builtins$open(pkl_path, "rb")
reloaded <- pickle$load(fh_v)
fh_v$close()

cat("\n── Pickle top-level keys ───────────────────────────────────────\n")
cat(paste(names(reloaded), collapse=", "), "\n")

h <- reloaded$hazard_model
cat(sprintf("\nhazard_model (accrual spec, n=%d)\n", h$n_obs))
cat(sprintf("  β_log(SSW)   = % .5f\n", h$beta_ssw_log))
cat(sprintf("  β_accrual%%   = % .5f\n", h$beta_accrual_rate))
cat(sprintf("  β_age        = % .5f\n", h$beta_age))
cat(sprintf("  β_age²       = % .7f\n", h$beta_age2))

hB <- reloaded$hazard_model_B
cat(sprintf("\nhazard_model_B (peak-value spec, n=%d)\n", hB$n_obs))
cat(sprintf("  β_log(SSW)   = % .5f\n", hB$beta_ssw_log))
cat(sprintf("  β_peak_val%%  = % .5f\n", hB$beta_peak_value_rate))
cat(sprintf("  β_age        = % .5f\n", hB$beta_age))
cat(sprintf("  β_age²       = % .7f\n", hB$beta_age2))
