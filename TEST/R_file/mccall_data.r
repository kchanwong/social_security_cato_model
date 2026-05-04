# =============================================================================
# McCall Model Data: 2007-2024
# Produces three outputs:
#   1. unemp_rates.csv        — unemployment rate by sex × educ × age_band × year
#   2. wage_distributions.csv — empirical wage CDF (99 quantiles) by cell
#   3. separation_rates.csv   — P(employed t → unemployed t+1) by cell
#
# Source: IPUMS CPS ASEC with adjacent-year linking via CPSIDV
# =============================================================================

library(ipumsr)
library(dplyr)
library(tidyr)
library(readr)

readRenviron("C:/Users/kchanwong/Documents/API_KEY.Renviron")
set_ipums_api_key(Sys.getenv('IPUMS_API'))

BASE_DIR <- "C:/Users/kchanwong/Documents/GitHub/social_security_cato_model/TEST"
OUT_DIR  <- file.path(BASE_DIR, "Data_Output")
XML_DIR  <- file.path(BASE_DIR, "IPUMS_XML")

# =============================================================================
# PART 1 — SUBMIT / DOWNLOAD IPUMS EXTRACT
# Samples: March ASEC 2008-2025 (covering reference years 2007-2024)
# =============================================================================

dat_path <- file.path(BASE_DIR, "cps_00131.dat.gz")

if (!file.exists(dat_path)) {
  message("Submitting IPUMS CPS extract ...")

  # March ASEC sample IDs: cps2008_03s ... cps2025_03s
  asec_samples <- paste0("cps", 2008:2025, "_03s")

  extract <- define_extract_micro(
    collection = 'cps',
    description = "McCall model: ASEC 2008-2025 with empstat + incwage",
    samples     = asec_samples,
    variables   = c(
      # linking / technical
      "CPSIDP", "CPSIDV", "ASECFLAG", "ASECWT",
      # demographics
      "AGE", "SEX", "MARST", "EDUC",
      # labor market
      "LABFORCE", "EMPSTAT", "INCWAGE", "UHRSWORKLY", "WKSWORK1"
    )
  )

  submitted    <- submit_extract(extract)
  message("Waiting for extract to complete ...")
  downloadable <- wait_for_extract(submitted)
  paths        <- download_extract(downloadable,
                                   download_dir = XML_DIR,
                                   overwrite    = TRUE)
  message("Downloaded: ", paste(paths, collapse = ", "))
} else {
  message("dat.gz already present — skipping API request.")
}

# =============================================================================
# PART 2 — LOAD AND RECODE
# =============================================================================
ddi <- read_ipums_ddi(paths)
raw <- read_ipums_micro(ddi) |>
  filter(
    ASECFLAG == 1L,
    AGE      >= 16L,
    LABFORCE %in% c(1L, 2L)
  ) |>
  mutate(
    year = as.integer(YEAR),
    sex  = as.integer(SEX),

    educ = case_when(
      EDUC <= 73L                         ~ "hs",
      EDUC %in% c(80L,81L,90L,91L,92L)   ~ "some_college",
      EDUC >= 100L                        ~ "ba_plus",
      TRUE                                ~ NA_character_
    ),

    age_band = case_when(
      AGE >= 16L & AGE <= 24L ~ "16-24",
      AGE >= 25L & AGE <= 34L ~ "25-34",
      AGE >= 35L & AGE <= 44L ~ "35-44",
      AGE >= 45L & AGE <= 54L ~ "45-54",
      AGE >= 55L & AGE <= 64L ~ "55-64",
      AGE >= 65L              ~ "65+",
      TRUE                    ~ NA_character_
    ),

    # EMPSTAT: 10-19 employed, 20-29 unemployed, 30+ NILF
    emp_status = case_when(
      EMPSTAT >= 10L & EMPSTAT <= 19L ~ "employed",
      EMPSTAT >= 20L & EMPSTAT <= 29L ~ "unemployed",
      TRUE                            ~ "nilf"
    ),

    # Positive wage; top-code flag = 99999999
    wage = if_else(INCWAGE > 0L & INCWAGE < 99999990L,
                   as.numeric(INCWAGE), NA_real_),

    wt = as.numeric(ASECWT)
  ) |>
  filter(!is.na(educ), !is.na(age_band))

# =============================================================================
# PART 3 — UNEMPLOYMENT RATES
# =============================================================================
unemp_rates <- raw |>
  filter(LABFORCE == 2L) |>
  group_by(year, sex, educ, age_band) |>
  summarise(
    n_wt     = sum(wt, na.rm = TRUE),
    unemp_wt = sum(wt[emp_status == "unemployed"], na.rm = TRUE),
    .groups  = "drop"
  ) |>
  mutate(unemp_rate = unemp_wt / n_wt) |>
  select(year, sex, educ, age_band, unemp_rate, n_wt)
write_csv(unemp_rates, file.path(OUT_DIR, "unemp_rates.csv"))
message("unemp_rates.csv  — ", nrow(unemp_rates), " rows")

# =============================================================================
# PART 4 — WAGE DISTRIBUTIONS (99-quantile grid per cell)
# =============================================================================
wtd_quantile <- function(x, w, probs = seq(0.01, 0.99, by = 0.01)) {
  ok  <- !is.na(x) & x > 0 & !is.na(w) & w > 0
  if (sum(ok) < 20L) return(rep(NA_real_, length(probs)))
  x <- x[ok]; w <- w[ok]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw  <- cumsum(w) / sum(w)
  vapply(probs, \(p) x[which.min(abs(cw - p))], numeric(1L))
}

probs_grid <- seq(0.01, 0.99, by = 0.01)

wage_distributions <- raw |>
  filter(emp_status == "employed", !is.na(wage)) |>
  group_by(year, sex, educ, age_band) |>
  summarise(
    quantile = list(probs_grid),
    wage_q   = list(wtd_quantile(wage, wt, probs_grid)),
    .groups  = "drop"
  ) |>
  unnest(cols = c(quantile, wage_q))

write_csv(wage_distributions, file.path(OUT_DIR, "wage_distributions.csv"))
message("wage_distributions.csv  — ", nrow(wage_distributions), " rows")

# =============================================================================
# PART 5 — SEPARATION RATES (adjacent-year link via CPSIDV)
# =============================================================================

snap_t <- raw |>
  select(year, CPSIDV, sex, educ, age_band, emp_status, wt) |>
  rename_with(\(x) paste0(x, "_1"), .cols = -c(year, CPSIDV))

snap_t1 <- raw |>
  select(year, CPSIDV, emp_status) |>
  rename(emp_status_2 = emp_status) |>
  mutate(year = year - 1L)           # align to base year

linked <- snap_t |>
  inner_join(snap_t1, by = c("year", "CPSIDV")) |>
  filter(
    emp_status_1 %in% c("employed", "unemployed"),
    emp_status_2 %in% c("employed", "unemployed")
  )

separation_rates <- linked |>
  filter(emp_status_1 == "employed") |>
  group_by(year, sex = sex_1, educ = educ_1, age_band = age_band_1) |>
  summarise(
    n_wt    = sum(wt_1, na.rm = TRUE),
    sep_wt  = sum(wt_1[emp_status_2 == "unemployed"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(sep_rate = sep_wt / n_wt) |>
  select(year, sex, educ, age_band, sep_rate, n_wt)

write_csv(separation_rates, file.path(OUT_DIR, "separation_rates.csv"))
message("separation_rates.csv  — ", nrow(separation_rates), " rows")

# =============================================================================
# PART 6 — SANITY CHECK
# =============================================================================

cat("\n--- Unemployment rates 2019 (prime age) ---\n")
unemp_rates |>
  filter(year == 2019L, age_band %in% c("25-34", "35-44", "45-54")) |>
  arrange(sex, educ, age_band) |>
  mutate(unemp_pct = round(unemp_rate * 100, 1)) |>
  print(n = 30L)

cat("\n--- Separation rates 2019 (prime age) ---\n")
separation_rates |>
  filter(year == 2019L, age_band %in% c("25-34", "35-44", "45-54")) |>
  arrange(sex, educ, age_band) |>
  mutate(sep_pct = round(sep_rate * 100, 1)) |>
  print(n = 30L)
