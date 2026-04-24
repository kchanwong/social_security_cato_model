library(ipumsr)
library(dplyr)
library(broom)
library(demography)
library(forecast)

setwd("C:/Users/kchanwong/Documents/TEST")
df <- read.csv('initial_simulation.csv')
df <- as_tibble(df)

### Fertility ###
birth_by_age_raw <- read.csv("C:/Users/kchanwong/Downloads/Natality, 2007-2024.csv")
colnames(birth_by_age_raw) <- c('Notes', 'Age_Band', 'Age_Code', 'Year', 'Year_Code', 'Births')
birth_by_age_raw <- birth_by_age_raw %>% na.omit() 

### Raw Population Counts ###
raw_population <- read.csv("C:/Users/kchanwong/Downloads/SSPopJan_TR2023.csv")
grouped_population <- as_tibble(raw_population) %>% 
  select(Year, Age, F.Tot) %>%
  mutate(Age_Band = case_when(
    Age < 15              ~ "Under 15 years",
    Age >= 15 & Age < 20  ~ "15-19 years",
    Age >= 20 & Age < 25  ~ "20-24 years",
    Age >= 25 & Age < 30  ~ "25-29 years",
    Age >= 30 & Age < 35  ~ "30-34 years",
    Age >= 35 & Age < 40  ~ "35-39 years",
    Age >= 40 & Age < 45  ~ "40-44 years",
    Age >= 45 & Age < 50  ~ "45-49 years",
    Age >= 50             ~ "50 years and over"
  )) %>%
  group_by(Year, Age_Band) %>%
  summarise(Total = sum(F.Tot), .groups = "drop")

### ASFR Calculate ###
asfr <- birth_by_age_raw %>% 
  inner_join(grouped_population, by = c("Year", "Age_Band")) %>%
  filter(!Age_Band %in% c("Under 15 years", "50 years and over")) %>% 
  mutate(ASFR = Births / Total)

# ── Hyndman-Ullah FDA ─────────────────────────────────────────────────────────

# Age band midpoints — fdm() needs a numeric age axis
age_midpoints <- c(17, 22, 27, 32, 37, 42, 47)
age_band_levels <- c("15-19 years","20-24 years","25-29 years",
                     "30-34 years","35-39 years","40-44 years","45-49 years")

# Pivot to matrix: rows = age (ordered), cols = year
asfr_wide <- asfr %>%
  filter(Age_Band %in% age_band_levels) %>%
  mutate(Age_Band = factor(Age_Band, levels = age_band_levels)) %>%
  select(Year, Age_Band, ASFR) %>%
  tidyr::pivot_wider(names_from = Year, values_from = ASFR) %>%
  arrange(Age_Band) %>%
  select(-Age_Band) %>%
  as.matrix()

rownames(asfr_wide) <- age_midpoints
years_obs <- as.integer(colnames(asfr_wide))

# Build demogdata object
# pop = matrix of 1s because ASFR is already computed (not raw rates)
fert_obj <- demogdata(
  data  = asfr_wide,
  pop   = matrix(1, nrow = nrow(asfr_wide), ncol = ncol(asfr_wide)),
  ages  = age_midpoints,
  years = years_obs,
  type  = "fertility",
  label = "USA",
  name  = "ASFR"
)

# Fit functional model
# order = 2: level factor + slope factor; safe given only ~18 time points
fit_fdm <- fdm(fert_obj, order = 2)
# Forecast to 2100
h <- 2100 - max(years_obs)
fc_fdm <- forecast(fit_fdm, h = 2100 - max(years_obs))

# ── Extract to tidy data frame ────────────────────────────────────────────────
forecast_years <- seq(max(years_obs) + 1, 2100)
asfr_forecast <- as.data.frame(fc_fdm$rate) 
asfr_forecast_point <- as_tibble(asfr_forecast) %>% 
    select(-contains('upper')) %>% 
    select(-contains('lower'))
library(tidyr)
FORECASTED_ASFR <- asfr_forecast_point %>% mutate(
    AGE_BAND = age_band_levels
) %>%
    pivot_longer(-AGE_BAND, names_to = "Year", values_to = "ASFR_forecast") %>%
    mutate(Year = as.integer(gsub("asfr\\.", "", Year))) %>%
    rename(ASFR = ASFR_forecast)

asfr %>% 
    select(AGE_BAND = Age_Band, Year, ASFR) %>% 
    rbind(FORECASTED_ASFR) %>%
    arrange(AGE_BAND) %>% 
    group_by(Year) %>% summarise(TFR = 5 * sum(ASFR)) %>% print(n = 100)



  setNames(forecast_years) %>%
  mutate(Age_Band = age_band_levels) %>%
  tidyr::pivot_longer(-Age_Band, names_to = "Year", values_to = "ASFR_forecast") %>%
  mutate(Year = as.integer(Year))

# Optionally bind observed + forecast into one series
asfr_observed <- asfr %>%
  filter(Age_Band %in% age_band_levels) %>%
  select(Year, Age_Band, ASFR_forecast = ASFR) %>%
  mutate(Year = as.integer(Year))

asfr_full <- bind_rows(
  mutate(asfr_observed, series = "observed"),
  mutate(asfr_forecast, series = "forecast")
)
