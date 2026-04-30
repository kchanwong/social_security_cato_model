### Estimate Thirty Five Year Earnings Equation ###
### Saves model as Python pickle via reticulate   ###

library(ipumsr)
library(dplyr)
library(fixest)
library(fredr)
library(reticulate)   # for pickling

setwd("C:/Users/kchanwong/Documents/TEST")

# Authenticate
set_ipums_api_key("", save = TRUE)
fredr_set_key("abce164b5784be00afcf9a19df04abde")

# ─────────────────────────────────────────────────────────────────────────────
# 1. PULL CPS ASEC DATA
# 35 years of March CPS ASEC: income years 1975–2009
# (survey years 1976–2010 in IPUMS CPS nomenclature)
# ─────────────────────────────────────────────────────────────────────────────
cps_extract <- define_extract_cps(
  description = "CBOLT earnings equation replication: 35yr CPS ASEC",
  samples     = paste0("cps", 1976:2010, "_03s"),   # March ASEC, survey years 1976-2010
  variables   = c(
    # Identifiers & weights
    "YEAR",       # Survey year
    "SERIAL",     # Household serial number
    "PERNUM",     # Person number within household
    "ASECWT",     # Person-level ASEC weight
    # Earnings
    "INCWAGE",    # Wage and salary income (prior year)
    "WKSWORK1",   # Weeks worked last year (continuous)
    # Demographics
    "AGE",
    "SEX",
    'LABFORCE',
    # Education (maps to <HS / HS / some college / BA+)
    "EDUC",
    # Marital status
    "MARST",
    # Children under ~6 (women's equation)
    "NCHILD",
    # Social Security beneficiary flag
    "INCSS",
    # Labour force / class of worker
    "CLASSWKR",
    "EMPSTAT"
  )
)

submitted   <- submit_extract(cps_extract)
downloadable <- wait_for_extract(submitted)
files       <- download_extract(downloadable, download_dir = "C:/Users/kritc/OneDrive/Documents/GitHub/social_security_cato_model/TEST")
raw         <- read_ipums_micro(files)

# ─────────────────────────────────────────────────────────────────────────────
# 2. INFLATION ADJUSTMENT  (March CPI-U, base year = 2008)
# ─────────────────────────────────────────────────────────────────────────────
MULTIPLIER_ADJUSTING <- fredr(
  series_id         = "CPIAUCSL",
  observation_start = as.Date("1947-01-01"),
  observation_end   = as.Date("2011-01-01")
) %>%
  mutate(
    YEAR  = as.integer(format(date, "%Y")),
    MONTH = as.integer(format(date, "%m"))
  ) %>%
  filter(MONTH == 3) %>%
  mutate(MULTIPLIER_2008 = value / value[YEAR == 2008]) %>%
  select(YEAR, MULTIPLIER_2008)

# ─────────────────────────────────────────────────────────────────────────────
# 3. CLEAN & RECODE
# ─────────────────────────────────────────────────────────────────────────────
EDUC_EDIT <- raw %>%
  mutate(
    EDUC_GROUPS = case_when(
      EDUC %in%  0:73   ~ "hs",            # NIU through 12th grade no diploma
      EDUC %in% 80:92   ~ "some_college",  # 1 yr college through Associate's
      EDUC %in% 100:125 ~ "ba_plus",       # Bachelor's and above
      .default = NA_character_             # 999 Missing/Unknown → dropped below
    ),
    YEAR_BORN  = YEAR - AGE,
    COHORT     = (YEAR_BORN %/% 10L) * 10L,
    RECEIVE_SS = ifelse(INCSS == 999999 | INCSS < 0, "yes", "no"),
    INCWAGE    = ifelse(INCWAGE %in% c(99999999L, 99999998L), 0L, INCWAGE)
  ) %>%
  inner_join(MULTIPLIER_ADJUSTING, by = "YEAR") %>%
  mutate(REAL_INCWAGE = INCWAGE * MULTIPLIER_2008) %>%
  filter(REAL_INCWAGE > 4200, !is.na(EDUC_GROUPS))

# ─────────────────────────────────────────────────────────────────────────────
# 4. FIT WAGE EQUATION
# REAL_INCWAGE ~ AGE + COHORT  |  SEX + EDUC_GROUPS + RECEIVE_SS
# Fixed effects absorb all between-group level differences.
# ─────────────────────────────────────────────────────────────────────────────
wage_model <- feols(
  REAL_INCWAGE ~ AGE + COHORT | SEX + EDUC_GROUPS + RECEIVE_SS,
  data    = EDUC_EDIT,
  weights = ~ASECWT
)


# Re-estimate on log(REAL_INCWAGE) — CBOLT uses a log-earnings equation so that
# the residual (PED = ln actual − ln predicted) is well-defined and symmetric.
wage_model_log <- feols(
  log(REAL_INCWAGE) ~ AGE + COHORT | SEX + EDUC_GROUPS + RECEIVE_SS,
  data    = EDUC_EDIT,
  weights = ~ASECWT
)
summary(wage_model_log)

# ─────────────────────────────────────────────────────────────────────────────
# 5. FULL CPS SAMPLE FOR LFP / EMPLOYMENT MODELS
#    Uses all CPS obs with a valid labour-force status (no wage floor).
#    Same education recodes as the wage sample; age 20–80; no Armed Forces.
# ─────────────────────────────────────────────────────────────────────────────
# LABFORCE codes: 1 = NILF, 2 = In LF
# EMPSTAT codes : 10/12 = employed, 20/21 = unemployed
CPS_FULL <- raw %>%
  mutate(
    EDUC_GROUPS = case_when(
      EDUC %in%  0:73   ~ "hs",
      EDUC %in% 80:92   ~ "some_college",
      EDUC %in% 100:125 ~ "ba_plus",
      .default = NA_character_
    ),
    YEAR_BORN  = YEAR - AGE,
    COHORT     = (YEAR_BORN %/% 10L) * 10L,
    RECEIVE_SS = ifelse(INCSS == 999999 | INCSS < 0, "yes", "no"),
    IN_LF      = case_when(
      LABFORCE == 2L ~ 1L,   # In labour force
      LABFORCE == 1L ~ 0L,   # Not in labour force (NILF)
      TRUE           ~ NA_integer_
    ),
    EMPLOYED   = case_when(
      EMPSTAT %in% c(10L, 12L) ~ 1L,   # At work / has job, not at work
      EMPSTAT %in% c(20L, 21L) ~ 0L,   # Unemployed (experienced / new)
      TRUE                     ~ NA_integer_
    )
  ) %>%
  filter(
    AGE >= 20, AGE <= 80,
    !is.na(EDUC_GROUPS),
    EMPSTAT != 1L          # Drop Armed Forces
  )

# ─────────────────────────────────────────────────────────────────────────────
# 6. FIT LFP MODEL (logistic)
#    Pr(in labour force) ~ AGE + COHORT | SEX + EDUC_GROUPS + RECEIVE_SS
#    Used in simulation for within-cell propensity *ranking* only —
#    BLS / TR2025 cell-level targets set the actual rates.
# ─────────────────────────────────────────────────────────────────────────────
lfp_data  <- CPS_FULL %>% filter(!is.na(IN_LF))
lfp_model <- feglm(
  IN_LF ~ AGE + COHORT | SEX + EDUC_GROUPS + RECEIVE_SS,
  data    = lfp_data,
  family  = binomial(link = "logit"),
  weights = ~ASECWT
)
summary(lfp_model)

# ─────────────────────────────────────────────────────────────────────────────
# 7. FIT EMPLOYMENT MODEL (logistic, conditional on being in LF)
#    Pr(employed | in LF) ~ AGE + COHORT | SEX + EDUC_GROUPS + RECEIVE_SS
# ─────────────────────────────────────────────────────────────────────────────
emp_data  <- CPS_FULL %>% filter(IN_LF == 1L, !is.na(EMPLOYED))
emp_model <- feglm(
  EMPLOYED ~ AGE + COHORT | SEX + EDUC_GROUPS + RECEIVE_SS,
  data    = emp_data,
  family  = binomial(link = "logit"),
  weights = ~ASECWT
)
summary(emp_model)

# ─────────────────────────────────────────────────────────────────────────────
# 8. EXPORT ALL THREE MODELS AS Python PICKLE via reticulate
#
# Python can't load feols / feglm objects natively, so we extract:
#   • beta_age, beta_cohort   — continuous slope coefficients
#   • fixef_sex / _educ / _ss — fixed-effect intercepts per level
#   • outcome, family         — tells simulation.py how to form predictions
#
# Prediction in Python:
#   linear_pred = beta_age*age + beta_cohort*cohort
#                 + fixef_sex[sex] + fixef_educ[educ] + fixef_ss[ss]
#   wage model  : predicted_log_wage = linear_pred          (outcome = "log")
#   LFP / emp   : Pr(yes) = sigmoid(linear_pred)
# ─────────────────────────────────────────────────────────────────────────────
extract_coef_dict <- function(model, outcome = "log", family = "gaussian") {
  betas <- coef(model)
  fe    <- fixef(model)
  list(
    beta_age    = unname(betas["AGE"]),
    beta_cohort = unname(betas["COHORT"]),
    fixef_sex   = as.list(fe[["SEX"]]),
    fixef_educ  = as.list(fe[["EDUC_GROUPS"]]),
    fixef_ss    = as.list(fe[["RECEIVE_SS"]]),
    outcome     = outcome,
    family      = family,
    n_obs       = as.integer(nobs(model))
  )
}

bundle <- list(
  wage_model = extract_coef_dict(wage_model_log, outcome = "log",      family = "gaussian"),
  lfp_model  = extract_coef_dict(lfp_model,      outcome = "binomial", family = "binomial"),
  emp_model  = extract_coef_dict(emp_model,       outcome = "binomial", family = "binomial")
)

# Write via Python pickle (reticulate calls the stdlib pickle — always available)
pickle   <- import("pickle")
builtins <- import_builtins()

out_path <- file.path(getwd(), "fitted_models.pkl")
fh       <- builtins$open(out_path, "wb")
pickle$dump(r_to_py(bundle), fh)
fh$close()

message(sprintf("Saved fitted_models.pkl  (%s)", out_path))

# ─────────────────────────────────────────────────────────────────────────────
# 9. VERIFY ROUND-TRIP
# ─────────────────────────────────────────────────────────────────────────────
fh2      <- builtins$open(out_path, "rb")
reloaded <- pickle$load(fh2)
fh2$close()

cat("\n── Pickle keys ──────────────────────────\n")
cat("Top-level :", paste(names(reloaded), collapse = ", "), "\n")
cat("wage keys :", paste(names(reloaded$wage_model), collapse = ", "), "\n")

cat(sprintf("\nwage  β_age = %.5f   β_cohort = %.6f   n = %d\n",
            reloaded$wage_model$beta_age,
            reloaded$wage_model$beta_cohort,
            reloaded$wage_model$n_obs))
cat(sprintf("lfp   β_age = %.5f   β_cohort = %.6f   n = %d\n",
            reloaded$lfp_model$beta_age,
            reloaded$lfp_model$beta_cohort,
            reloaded$lfp_model$n_obs))
cat(sprintf("emp   β_age = %.5f   β_cohort = %.6f   n = %d\n",
            reloaded$emp_model$beta_age,
            reloaded$emp_model$beta_cohort,
            reloaded$emp_model$n_obs))

cat("\nwage fixef_educ:\n")
print(reloaded$wage_model$fixef_educ)
cat("\nlfp  fixef_sex:\n")
print(reloaded$lfp_model$fixef_sex)
