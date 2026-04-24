#### FIX EST
library(fixest)
library(dplyr)
library(httr2)
library(wbstats)   



# ── OECD country codes ────────────────────────────────────────────────────────
oecd_iso3 <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT",
  "SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA"
)

# ── 1. Pull TFR from OECD SDMX API ───────────────────────────────────────────
# New OECD Data Explorer API (SDMX 2.1, CSV format)
# Dataflow: OECD.ELS.FAM, DSD_FAMILY@DF_FERT_RATE
# Indicator: FERTILITY_RATE | TOTAL | Annual

fetch_oecd_tfr <- function() {
  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/",
    "OECD.ELS.FAM,DSD_FAMILY@DF_FERT_RATE/",
    paste(oecd_iso3, collapse = "+"),
    ".FERTILITY_RATE.TOTAL.A",
    "?startPeriod=1980&endPeriod=2023",
    "&dimensionAtObservation=AllDimensions",
    "&format=csvfilewithlabels"
  )

  resp <- request(url) %>%
    req_timeout(60) %>%
    req_perform()

  resp %>%
    resp_body_string() %>%
    read_csv(show_col_types = FALSE) %>%
    select(
      country    = `Reference area`,
      iso3       = REF_AREA,
      year       = TIME_PERIOD,
      tfr        = OBS_VALUE
    ) %>%
    mutate(
      year = as.integer(year),
      tfr  = as.numeric(tfr)
    ) %>%
    filter(!is.na(tfr))
}

# ── 2. World Bank fallback (identical data, always works) ────────────────────
fetch_wb_tfr <- function() {
  wb_tfr <- wb_data(
    indicator = "SP.DYN.TFRT.IN",
    country   = oecd_iso3,
    start_date = 1980,
    end_date   = 2023,
    return_wide = FALSE
  )

  wb_tfr %>%
    select(
      country = country,
      iso3    = iso3c,
      year    = date,
      tfr     = value
    ) %>%
    filter(!is.na(tfr))
}

# ── 3. Load data (try OECD first, fall back to World Bank) ───────────────────
oecd_tfr <- tryCatch(
  fetch_oecd_tfr(),
  error = function(e) {
    message("OECD API failed: ", conditionMessage(e))
    message("Falling back to World Bank data (same series)...")
    fetch_wb_tfr()
  }
)

# Quick check

# ── 4. Beta-convergence ───────────────────────────────────────────────────────
# Regress 10-year change in TFR on lagged TFR level
# β < 0 → countries with high TFR fall faster → convergence

conv_data <- oecd_tfr %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(
    tfr_lag10 = lag(tfr, 10),
    d_tfr     = tfr - tfr_lag10
  ) %>%
  ungroup() %>%
  filter(!is.na(tfr_lag10), !is.na(d_tfr))

# Two-way FE: country + year absorbs level differences and common trends
beta_model <- feols(
  d_tfr ~ tfr_lag10 | iso3 + year,
  data  = conv_data,
  vcov  = "twoway"
)

summary(beta_model)

# Expected: tfr_lag10 coefficient negative and significant
# Interpretation: a 1-unit higher starting TFR → d_tfr lower by |β| over 10 years

# ── 5. Sigma-convergence: is cross-country spread shrinking? ─────────────────
sigma_data <- oecd_tfr %>%
  group_by(year) %>%
  summarise(
    sigma     = sd(tfr, na.rm = TRUE),
    mean_tfr  = mean(tfr, na.rm = TRUE),
    cv        = sigma / mean_tfr,    # coefficient of variation
    .groups   = "drop"
  )
library(ggplot2)
sigma_plot <- sigma_data %>%
  ggplot(aes(year, sigma)) +
  geom_line(linewidth = 1.1, color = "#2c3e50") +
  geom_smooth(method = "lm", se = TRUE,
              color = "#e74c3c", linetype = "dashed") +
  labs(
    title    = "Sigma-convergence in OECD fertility rates, 1980–2023",
    subtitle = "Cross-country standard deviation of TFR",
    y        = "SD of TFR across OECD",
    x        = NULL,
    caption  = "Source: OECD Family Database / World Bank WDI"
  ) +
  theme_minimal(base_size = 13)

print(sigma_plot)

# ── 6. US residual from OECD mean ────────────────────────────────────────────
# Country + year FE soaks up both fixed country levels and common year trends
# Residual = how much the US sits ABOVE OR BELOW what its FE predicts
# Shrinking positive residual = US losing its exceptionalism

fe_model <- feols(
  tfr ~ 1 | iso3 + year,
  data = oecd_tfr
)

oecd_tfr$resid <- residuals(fe_model)

us_resid_plot <- oecd_tfr %>%
  filter(iso3 == "USA") %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1.2, color = "#2980b9") +
  geom_point(size = 2, color = "#2980b9") +
  annotate("rect",
           xmin = 2007, xmax = 2023,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "#e74c3c") +
  annotate("text", x = 2015, y = Inf,
           label = "Post-2007\ncollapse",
           vjust = 1.5, size = 3.5, color = "#e74c3c") +
  labs(
    title    = "US TFR residual from OECD mean (country + year FE)",
    subtitle = "Positive = US above what its country FE predicts; zero = indistinguishable from peers",
    y        = "US deviation from OECD average",
    x        = NULL,
    caption  = "Source: OECD Family Database / World Bank WDI"
  ) +
  theme_minimal(base_size = 13)

print(us_resid_plot)
setwd("C:/Users/kchanwong/Documents/SSA_LR/csvs")
 oecd_tfr %>%
  filter(iso3 == "USA") %>%
  select(country, year, resid, tfr) %>%
  write.csv("fig_2.csv")




# ── 7. The money table: where is the US now vs. peers? ───────────────────────
oecd_tfr %>%
  filter(year == max(year)) %>%
  arrange(desc(tfr)) %>%
  mutate(rank = row_number()) %>%
  select(rank, country, iso3, year, tfr) %>%
  print(n = 40)



library(HMDHFDplus)
library(ggplot2)
asfr_period <- readHFDweb("USA", "asfrVH",   # period ASFR
                            username = "kchanwong@cato.org",
                            password = "Must@ng5!") 
ASFR <- as_tibble(asfr_period) %>% 
    na.omit() %>% 
    mutate(YEAR_ALIVE = Cohort + Age) %>%
    filter(ASFR > 0)
asfr_period <- ASFR %>%
  filter(!OpenInterval,
         YEAR_ALIVE >= 2000,
         Age %in% 15:44) %>%
  mutate(age_group = case_when(
    Age %in% 15:19 ~ "15-19",
    Age %in% 20:24 ~ "20-24",
    Age %in% 25:29 ~ "25-29",
    Age %in% 30:34 ~ "30-34",
    Age %in% 35:39 ~ "35-39",
    Age %in% 40:44 ~ "40-44"
  )) %>%
  group_by(YEAR_ALIVE, age_group) %>%
  summarise(ASFR = sum(ASFR), .groups = "drop")


library(tidyverse)
library(zoo)
library(lmtest)    # Granger causality
library(tseries)   # ADF stationarity test

# ── 1. Load and clean ─────────────────────────────────────────────────────────
df_raw <- read_csv("data-qZRYI.csv") %>%
  rename(
    date   = 1,
    ideal  = `Average ideal number of children`,
    tfr    = `U.S. fertility rate`
  ) %>%
  mutate(
    year  = as.integer(str_extract(date, "^\\d{4}")),
    ideal = as.numeric(ideal),
    tfr   = as.numeric(tfr)
  )

# Multiple Gallup polls within same year → take the mean
df_annual <- df_raw %>%
  group_by(year) %>%
  summarise(
    ideal = mean(ideal, na.rm = TRUE),
    tfr   = mean(tfr,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Fill in missing years so we have a regular grid
  complete(year = full_seq(year, 1)) %>%
  arrange(year)

# ── 2. Interpolate gaps to get regular annual series ─────────────────────────
# Linear interpolation — appropriate for slow-moving demographic series
df_interp <- df_annual %>%
  mutate(
    ideal_interp = na.approx(ideal, na.rm = FALSE, rule = 2),
    tfr_interp   = na.approx(tfr,   na.rm = FALSE, rule = 2)
  ) %>%
  filter(!is.na(ideal_interp), !is.na(tfr_interp))

cat("Years covered after interpolation:", 
    min(df_interp$year), "–", max(df_interp$year), "\n")
cat("N =", nrow(df_interp), "\n\n")

# ── 3. Plot raw series ────────────────────────────────────────────────────────
df_interp %>%
  pivot_longer(cols = c(ideal_interp, tfr_interp),
               names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
    ideal_interp = "Ideal family size (Gallup)",
    tfr_interp   = "Actual TFR"
  )) %>%
  ggplot(aes(year, value, color = series)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 2.1, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("Ideal family size (Gallup)" = "#27ae60",
                                "Actual TFR" = "#e74c3c")) +
  labs(title  = "Ideal family size vs. actual TFR, USA 1936–2024",
       y = "Children per woman", x = NULL, color = NULL,
       caption = "Sources: Gallup; CDC/NCHS") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ── 4. Stationarity tests (required before Granger) ─────────────────────────
cat("=== ADF stationarity tests (H0: unit root) ===\n")
cat("Ideal:\n")
print(adf.test(df_interp$ideal_interp))
cat("\nTFR:\n")
print(adf.test(df_interp$tfr_interp))

# If non-stationary, difference the series
df_interp <- df_interp %>%
  mutate(
    d_ideal = c(NA, diff(ideal_interp)),
    d_tfr   = c(NA, diff(tfr_interp))
  ) %>%
  filter(!is.na(d_ideal), !is.na(d_tfr))

cat("\nADF on differenced ideal:\n")
print(adf.test(df_interp$d_ideal))
cat("\nADF on differenced TFR:\n")
print(adf.test(df_interp$d_tfr))

# ── 5. Cross-correlation function ─────────────────────────────────────────────
# CCF tells you: at what lag does ideal best predict TFR (or vice versa)?
# Negative lag: ideal leads TFR (ideal predicts future TFR)
# Positive lag: TFR leads ideal (TFR predicts future ideal)
par(mfrow = c(1,2))
ccf(df_interp$d_ideal, df_interp$d_tfr,
    lag.max = 10,
    main = "CCF: Δideal → ΔTFR\n(neg. lag = ideal leads TFR)",
    ylab = "Correlation")

ccf(df_interp$d_tfr, df_interp$d_ideal,
    lag.max = 10,
    main = "CCF: ΔTFR → Δideal\n(neg. lag = TFR leads ideal)",
    ylab = "Correlation")

# ── 6. Granger causality test ─────────────────────────────────────────────────
# "Does knowing past ideal improve forecast of TFR beyond TFR's own lags?"
cat("\n=== Granger causality (2 lags) ===\n")

cat("\nH0: ideal does NOT Granger-cause TFR\n")
grangertest(d_tfr ~ d_ideal, order = 2, data = df_interp)

cat("\nH0: TFR does NOT Granger-cause ideal\n")
grangertest(d_ideal ~ d_tfr, order = 2, data = df_interp)

# Try lag 1 and lag 3 as robustness
for (k in 1:4) {
  cat(sprintf("\nLag %d — ideal → TFR p = %.3f | TFR → ideal p = %.3f\n",
    k,
    grangertest(d_tfr   ~ d_ideal, order = k, data = df_interp)$`Pr(>F)`[2],
    grangertest(d_ideal ~ d_tfr,   order = k, data = df_interp)$`Pr(>F)`[2]
  ))
}

# ── 7. Distributed lag regression ─────────────────────────────────────────────
# If ideal leads TFR: regress TFR on lagged ideal
# This gives you a coefficient you can report
cat("\n=== Distributed lag: TFR ~ ideal(t-1) + ideal(t-2) ===\n")

df_lag <- df_interp %>%
  mutate(
    ideal_l1 = lag(ideal_interp, 1),
    ideal_l2 = lag(ideal_interp, 2),
    ideal_l5 = lag(ideal_interp, 5)
  ) %>%
  filter(!is.na(ideal_l2))

m1 <- lm(tfr_interp ~ ideal_l1,            data = df_lag)
m2 <- lm(tfr_interp ~ ideal_l1 + ideal_l2, data = df_lag)
m3 <- lm(tfr_interp ~ ideal_l5,            data = df_lag)

cat("\nModel 1: TFR ~ ideal(t-1)\n"); summary(m1)
cat("\nModel 2: TFR ~ ideal(t-1) + ideal(t-2)\n"); summary(m2)
cat("\nModel 3: TFR ~ ideal(t-5)\n"); summary(m3)

# ── 8. Scatter with lag ───────────────────────────────────────────────────────
df_lag %>%
  ggplot(aes(ideal_l1, tfr_interp)) +
  geom_point(aes(color = year), size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#2c3e50") +
  scale_color_viridis_c(name = "Year") +
  labs(
    title    = "Does last year's ideal predict this year's TFR?",
    subtitle = sprintf("R² = %.2f", summary(m1)$r.squared),
    x        = "Ideal family size (t-1)",
    y        = "Actual TFR (t)"
  ) +
  theme_minimal(base_size = 13)


library(tidyverse)
library(scales)

# ── Cleaner table version ─────────────────────────────────────────────────────
offset_table <- asfr_period %>%
  filter(YEAR_ALIVE %in% c(2007, 2023)) %>%
  group_by(age_group) %>%
  summarise(change = diff(ASFR), .groups = "drop") %>%
  mutate(
    direction  = ifelse(change > 0, "Gain", "Loss"),
    change_pct = round(change / 
                 filter(asfr_period, YEAR_ALIVE == 2007)$ASFR * 100, 1)
  )

net <- tibble(
  age_group  = "NET",
  change     = sum(offset_table$change),
  direction  = "Loss",
  change_pct = NA_real_
)
library(scales)
offset_table %>% select(age_group, change) %>% write.csv('table_1.csv')
offset_table %>%
  bind_rows(net) %>%
  mutate(
    label = sprintf("%+.3f children/woman", change),
    age_group = factor(age_group,
                       levels = c("15-19","20-24","25-29",
                                  "30-34","35-39","40-44","NET"))
  ) %>%
  ggplot(aes(x = age_group, y = change, fill = direction)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = label,
                vjust = ifelse(change < 0, 1.4, -0.5)),
            size = 3.8, fontface = "bold") +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "grey30") +
  # Separate the NET bar visually
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "grey50") +
  annotate("text", x = 7, y = -0.05,
           label = "Net loss\n0.495 children\nper woman",
           size = 3.5, color = "#c0392b", fontface = "italic") +
  scale_fill_manual(values = c("Gain" = "#27ae60", "Loss" = "#e74c3c")) +
  scale_y_continuous(limits = c(-0.35, 0.12),
                     labels = label_number(style_positive = "plus")) +
  labs(
    title    = "Did fertility gains at older ages offset losses at younger ages? (2007–2023)",
    subtitle = "Trustees' tempo argument requires gains (green) to cancel losses (red) — they don't.",
    x        = "Age group",
    y        = "Change in births per woman",
    fill     = NULL,
    caption  = "Source: Human Fertility Database. Net = sum across all age groups."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.major.x = element_blank()
  )





library(dplyr)

# Load data
df <- read.csv("C:/Users/kchanwong/Downloads/TFR_PROJ_DIFF_SOURCE.csv")
# Settings
sources <- c("CBO_PROJ", "SSA_PROJ", "UN_PROJ", "CENSUS")
labels  <- c("CBO", "SSA", "UN", "German Path", "Census Bureau")
colors  <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd")
ltypes  <- c(1, 1, 1, 2, 1)  # 2 = dashed for German Path
df %>% select(-UN_PROJ) %>% na.omit() %>% write.csv('fig_1.csv')
# Set up plot area
plot(
  df$YEAR, df$SSA_PROJ,
  xlab = "Year",
  ylab = "Total Fertility Rate",
  main = "U.S. Total Fertility Rate Projections by Source",
  type = 'l',
  lwd = 3,
  ylim = c(1.5,2)
)
df$CENSUS
sources <- c("CBO_PROJ", "SSA_PROJ",  "CENSUS")
labels  <- c("CBO", "SSA", "Census Bureau")
colors  <- c("#1f77b4", "#d62728", "#9467bd")
ltypes  <- c(1, 1, 1, 1, 1)  # 2 = dashed for German Path
lwd <- c(3,3,3,3,3)
mtext("2025–2100", side = 3, line = 0.3, cex = 0.85, col = "gray40")
# Draw each line
for (i in seq_along(sources)) {
  lines(df$YEAR, df[[sources[i]]], col = colors[i],  lty = ltypes[i], lwd = 3)
}
df[['CENSUS']]
# Legend
legend(
  "bottomright",
  legend  = labels,
  col     = colors,
  lty     = ltypes,
  lwd     = 1.8,
  bty     = "n",
  cex     = 0.9
)
###
library(dplyr)
library(readxl)

# ── File paths ────────────────────────────────────────────────────────────────
path_census <- "C:/Users/kchanwong/Downloads/np2023_d1_mid.csv"
path_cbo    <- "C:/Users/kchanwong/Downloads/61879-Data (1).xlsx"
path_ssa    <- "C:/Users/kchanwong/Downloads/SingleYearTRTables_TR2025.xlsx"

# ── 1. Census Bureau ──────────────────────────────────────────────────────────
census <- read.csv(path_census) %>%
  filter(SEX == 0, ORIGIN == 0, RACE == 0) %>%
  select(Year = YEAR, Pop = TOTAL_POP) %>%
  mutate(Pop = Pop / 1e6, Source = "Census Bureau")

# ── 2. CBO (Figure 2: Population Size by Age Group) ──────────────────────────



# Read raw to map structure (no skipping)
raw <- read_excel("C:/Users/kchanwong/Documents/CBO_CSV.xlsx" , skip = 8, col_names = FALSE)

current_year <- NA
results <- list()

for (i in seq_len(nrow(raw))) {
  print(i)
  row <- raw[i, ]
  # Year header: col B is a 4-digit year
  val <- suppressWarnings(as.integer(row[[2]]))
  if (!is.na(val) && val >= 2020 && val <= 2100) {
    current_year <- val
    next
  }
  # Data row: col A = age, col B = total
  age   <- row[[1]]
  total <- suppressWarnings(as.numeric(row[[2]]))
  if (!is.na(current_year) && !is.na(total) && !is.na(age)) {
    results[[length(results) + 1]] <- tibble(Year = current_year, Age = age, Total = total)
  }
}

cbo <- bind_rows(results)
CBO_TOTAL <- cbo %>% group_by(Year) %>% summarise(Total = (1/1e6) * sum(Total))

# ── 3. UN World Population Prospects ─────────────────────────────────────────
# ── 4. SSA Trustees Report 2025 (Table V.A3) — Intermediate scenario only ────
# The sheet stacks Historical → Intermediate → Low-cost → High-cost sections.
# We track the current section and keep only Historical + Intermediate.
ssa_raw <- read_excel(path_ssa, sheet = "V.A3", col_names = FALSE)

scenario  <- "Historical"
ssa_rows  <- list()

for (i in seq_len(nrow(ssa_raw))) {
  yr_val  <- as.character(ssa_raw[[1]][i])
  tot_val <- ssa_raw[[5]][i]

  if (is.na(yr_val)) next

  # Detect section headers
  if (grepl("Intermediate", yr_val, ignore.case = TRUE)) { scenario <- "Intermediate"; next }
  if (grepl("Low|High",     yr_val, ignore.case = TRUE)) { scenario <- "Other";        next }

  # Only keep Historical and Intermediate rows
  if (scenario %in% c("Historical", "Intermediate")) {
    yr_num  <- suppressWarnings(as.integer(gsub("[^0-9]", "", yr_val)))
    tot_num <- suppressWarnings(as.numeric(tot_val))
    if (!is.na(yr_num) && !is.na(tot_num) && yr_num >= 1941) {
      ssa_rows[[length(ssa_rows) + 1]] <-
        data.frame(Year = yr_num, Pop = tot_num / 1e3, Source = "SSA")
    }
  }
}

ssa <- do.call(rbind, ssa_rows)
library(tidyr)
# ── Combine — focus on 2020 onward for context ────────────────────────────────
all_pop <- bind_rows(census,
CBO_TOTAL %>% rename(Pop = 'Total') %>% mutate(Source = 'CBO'), ssa) %>%
  filter(Year >= 2020) %>%
  pivot_wider(names_from = Source, values_from = Pop)
all_pop %>% write.csv('figure_4.csv')
# ── Plot ──────────────────────────────────────────────────────────────────────
sources <- c("Census Bureau", "CBO",  "SSA")
colors  <- c("Census Bureau" = "#9467bd",
             "CBO"           = "#1f77b4",
             "SSA"           = "#d62728")
ltypes  <- c("Census Bureau" = 1, "CBO" = 2, "SSA" = 1)

plot(
  NA,
  xlim = range(all_pop$Year),
  ylim = range(unlist(all_pop[, sources], use.names = FALSE), na.rm = TRUE),
  xlab = "Year",
  ylab = "Population (millions)",
  main = "U.S. Population Projections by Source",
  las  = 1,
  bty  = "l"
)

# Shade projection period
rect(2025, par("usr")[3], par("usr")[2], par("usr")[4],
     col = adjustcolor("grey85", alpha.f = 0.4), border = NA)
abline(v = 2025, lty = 3, col = "grey50")
text(2026, par("usr")[4] * 0.99, "Projections \u2192",
     adj = c(0, 1), cex = 0.75, col = "grey40")

for (s in sources) {
  lines(all_pop$Year, all_pop[[s]],
        col = colors[s], lty = ltypes[s], lwd = 1.8)
}

legend("topleft",
       legend = sources,
       col    = colors[sources],
       lty    = ltypes[sources],
       lwd    = 1.8,
       bty    = "n",
       cex    = 0.85)
###
library(dplyr)
library(readxl)

# ── 1. Census: (65+) / (25–64) — now matches CBO's age definition exactly ────
census_raw <- read.csv(path_census) %>%
  filter(SEX == 0, ORIGIN == 0, RACE == 0)

census_dep <- census_raw %>%
  mutate(
    pop_25_64 = rowSums(across(all_of(paste0("POP_", 20:64)))),
    pop_65p   = rowSums(across(all_of(paste0("POP_", 65:100)))),
    DepRatio  = pop_65p / pop_25_64
  ) %>%
  select(Year = YEAR, DepRatio) %>%
  mutate(Source = "Census Bureau")

# ── 2. CBO Figure 3: invert (25–64)/(65+) → (65+)/(25–64) ───────────────────
cbo_dep <- cbo %>% group_by(Year) %>% 
    summarise(DepRatio = sum(Total[Age %in% 65:100])/sum(Total[Age %in% 20:64])) %>%
    mutate(Source = 'CBO')


# ── 3. SSA V.A3: intermediate only, 20–64 base (finest available) ────────────
ssa_raw  <- read_excel(path_ssa, sheet = "V.A3", col_names = FALSE)
scenario <- "Historical"
ssa_rows <- list()

for (i in seq_len(nrow(ssa_raw))) {
  yr_val   <- as.character(ssa_raw[[1]][i])
  pop_2064 <- ssa_raw[[3]][i]   # 20-64 column
  pop_65p  <- ssa_raw[[4]][i]   # 65+ column

  if (is.na(yr_val)) next
  if (grepl("Intermediate", yr_val, ignore.case = TRUE)) { scenario <- "Intermediate"; next }
  if (grepl("Low|High",     yr_val, ignore.case = TRUE)) { scenario <- "Other";        next }

  if (scenario %in% c("Historical", "Intermediate")) {
    yr_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", yr_val)))
    d      <- suppressWarnings(as.numeric(pop_65p) / as.numeric(pop_2064))
    if (!is.na(yr_num) && !is.na(d) && yr_num >= 1941)
      ssa_rows[[length(ssa_rows) + 1]] <-
        data.frame(Year = yr_num, DepRatio = d, Source = "SSA")
  }
}

ssa_dep <- do.call(rbind, ssa_rows)

# ── Combine ───────────────────────────────────────────────────────────────────
all_dep <- bind_rows(census_dep, cbo_dep, ssa_dep) %>%
  filter(Year >= 2020) %>%
  pivot_wider(names_from = Source, values_from = DepRatio)

# ── Plot ──────────────────────────────────────────────────────────────────────
sources <- c("Census Bureau", "CBO", "SSA")
colors  <- c("Census Bureau" = "#9467bd", "CBO" = "#1f77b4", "SSA" = "#d62728")
ltypes  <- c("Census Bureau" = 1, "CBO" = 2, "SSA" = 1)
labels  <- c("Census Bureau" = "Census (65+ / 25\u201364)",
             "CBO"           = "CBO (65+ / 25\u201364)",
             "SSA"           = "SSA (65+ / 20\u201364)")

plot(
  NA,
  xlim = range(all_dep$Year),
  ylim = range(unlist(all_dep[, sources], use.names = FALSE), na.rm = TRUE),
  xlab = "Year",
  ylab = "Old-Age Dependency Ratio",
  main = "U.S. Old-Age Dependency Ratio Projections by Source",
  las  = 1,
  bty  = "l"
)

rect(2025, par("usr")[3], par("usr")[2], par("usr")[4],
     col = adjustcolor("grey85", alpha.f = 0.4), border = NA)
abline(v = 2025, lty = 3, col = "grey50")
text(2026, par("usr")[4] * 0.99, "Projections \u2192",
     adj = c(0, 1), cex = 0.75, col = "grey40")

for (s in sources) {
  lines(all_dep$Year, all_dep[[s]], col = colors[s], lty = ltypes[s], lwd = 1.8)
}

legend("topleft",
       legend = labels[sources],
       col    = colors[sources],
       lty    = ltypes[sources],
       lwd    = 1.8,
       bty    = "n",
       cex    = 0.85)

mtext("Note: SSA V.A3 only publishes 20\u201364 as working-age group; cannot match CBO's 25\u201364 cutoff.",
      side = 1, line = 4, cex = 0.68, col = "grey40", adj = 0)
### ER ####
dfCBO_PROJ <- read.csv('C:/Users/kchanwong/Documents/GitHub/social_security_cato_model/SSA_LR/lr_model/python/TFR_PROJ_CBO_TFR.csv')
dfCENSUS_PROJ <- read.csv('C:/Users/kchanwong/Documents/GitHub/social_security_cato_model/SSA_LR/lr_model/python/TFR_PROJ_CENSUS_TFR.csv')
dfSSA_PROJ <- read.csv('C:/Users/kchanwong/Documents/GitHub/social_security_cato_model/SSA_LR/lr_model/python/TFR_PROJ_intermediate.csv') %>% 
  na.omit()
plot(dfCBO_PROJ$year, dfSSA_PROJ$balance_rate, type = 'l', lwd = 3, ylim = c(-10, 0))
lines(dfCBO_PROJ$year, dfCBO_PROJ$balance_rate,  lwd = 3, col = 'darkgreen')
lines(dfCBO_PROJ$year, dfCENSUS_PROJ$balance_rate,  lwd = 3, col = 'darkred')
