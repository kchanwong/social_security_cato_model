library(dplyr)
library(tidycensus)
library(stringr)
library(ipumsr)
library(usincometaxes)
library(haven)
library(purrr)
library(progressr)

### read income ###
DF_INCOME <- read_ipums_micro("C:/Users/kritc/Downloads/usa_00195.xml")
TAX_SIM_NEED <- DF_INCOME %>%
    filter(INCWAGE != 999999, INCWAGE != 999998, INCWAGE > 0, AGE >= 18)  %>%
    select(STATEFIP, PUMA, PERWT, pwages = INCWAGE, page = AGE) %>%
    mutate(state = state.abb[match(as.character(as_factor(STATEFIP)), state.name)]) %>%    
    mutate(mstat = 'single') %>%
    select(-STATEFIP) %>%
    mutate(taxsimid = 1:n(), year = 2023)

batches <- TAX_SIM_NEED %>%
    mutate(batch = (row_number() - 1) %/% 100000) %>%
    group_split(batch)
TAX_SIMULATED <- vector("list", length(batches))
for (i in seq_along(batches)) {
    cat("Batch", i, "of", length(batches), "\n")
    TAX_SIMULATED[[i]] <- taxsim_calculate_taxes(batches[[i]])
}
TAX_SIMULATED <- bind_rows(TAX_SIMULATED)
TAX_SIM_NEED %>% inner_join(
    TAX_SIMULATED %>% select(taxsimid, fiitax, siitax, fica, tfica)
) %>%
mutate(effective_tax_rate = (fiitax + siitax + fica + tfica)/pwages)
with_progress({
    p <- progressor(along = batches)
    TAX_SIMULATED <- map_dfr(batches, \(x) { p(); taxsim_calculate_taxes(x) })
})
TAX_SIMULATED <- taxsim_calculate_taxes(TAX_SIM_NEED %>% head(70000))
TAX_SIMULATED 
HIGH_INCOME_EARNERS <- DF_INCOME %>% filter(INCWAGE != 999999, 
                            INCWAGE != 999998, INCWAGE > 168600)
HIGH_INCOME_EARNERS 
### join PUMA to counties ###
PUMA_TO_COUNTY <- read.csv(
    "C:/Users/kritc/Downloads/geocorr2022_2615405257.csv"
)
as_tibble(PUMA_TO_COUNTY) 
HIGH_INCOME_EARNERS %>% 
    group_by(
        PUMA
    ) 
census_api_key('')

# --- SSA County Data ---
PER_COUNTY <- read.csv("C:/Users/kritc/Downloads/oasdi_county_stacked.csv") %>%
    as_tibble() %>%
    mutate(t5_di_spouses_k = as.integer(t5_di_spouses_k)) %>%
    mutate(t5_di_spouses_k = ifelse(is.na(t5_di_spouses_k), 0, t5_di_spouses_k)) %>%
    mutate(
        TOTAL_OASDI_SPENDING_K    = t5_total_k - (t5_di_disabled_workers_k + t5_di_spouses_k + t5_di_children_k),
        TOTAL_OASDI_BENEFICIARIES = t4_total - (t4_di_disabled_workers + t4_di_spouses + t4_di_children),
        PER_OASDI_SPENDING        = (1e3 * TOTAL_OASDI_SPENDING_K) / TOTAL_OASDI_BENEFICIARIES,
        TRUST_FUND_LOSE           = 0.24 * PER_OASDI_SPENDING
    ) %>%
    select(STATE, COUNTY, ANSI, PER_OASDI_SPENDING, t4_ret_retired_workers, TRUST_FUND_LOSE)

# --- BEA GDP by County (handle VA combined independent cities) ---
BEA_CROSSWALK <- read.csv("C:/Users/kritc/Downloads/BEA_FIPS.csv") %>%
    as_tibble() %>%
    rename(BEA_FIPS = 1, BEA_Name = 2, FIPS = 3, Place_Name = 4) %>%
    mutate(BEA_FIPS = as.character(BEA_FIPS), ANSI = as.integer(FIPS)) %>%
    group_by(BEA_FIPS) %>%
    mutate(n_components = n()) %>%
    ungroup()

GDP_RAW <- read.csv("C:/Users/kritc/Downloads/Table.csv") %>%
    as_tibble() %>%
    filter(LineCode == 3) %>%
    select(GeoFIPS, GDP_2024 = X2024) %>%
    mutate(GeoFIPS = str_trim(as.character(GeoFIPS)), GDP_2024 = as.numeric(GDP_2024))

GDP_CLEAN <- bind_rows(
    GDP_RAW %>%
        filter(!GeoFIPS %in% BEA_CROSSWALK$BEA_FIPS) %>%
        mutate(ANSI = as.integer(GeoFIPS)) %>%
        select(ANSI, GDP_2024),
    GDP_RAW %>%
        filter(GeoFIPS %in% BEA_CROSSWALK$BEA_FIPS) %>%
        inner_join(BEA_CROSSWALK, by = c("GeoFIPS" = "BEA_FIPS")) %>%
        mutate(GDP_2024 = GDP_2024 / n_components) %>%
        select(ANSI, GDP_2024)
)

PER_COUNTY <- PER_COUNTY %>%
    left_join(GDP_CLEAN, by = "ANSI")

# --- County to CD Crosswalk ---
CROSSWALK_FIPS_TO_CD <- read.csv("C:/Users/kritc/Downloads/geocorr2022_2615408404.csv") %>%
    as_tibble()

# --- CD-Level Aggregation ---
CD_DATA <- CROSSWALK_FIPS_TO_CD %>%
    rename(ANSI = county) %>%
    group_by(ANSI) %>%
    mutate(county_share = pop20 / sum(pop20)) %>%
    ungroup() %>%
    inner_join(
        PER_COUNTY %>% select(ANSI, t4_ret_retired_workers, TRUST_FUND_LOSE, GDP_2024),
        by = 'ANSI'
    ) %>%
    group_by(cd119, stab) %>%
    summarise(
        MEAN_LOSS_PER_PERSON = weighted.mean(TRUST_FUND_LOSE, t4_ret_retired_workers * county_share),
        TOTAL_RETIREES       = sum(t4_ret_retired_workers * county_share),
        GDP                  = sum(GDP_2024 * county_share),
        .groups = 'drop'
    )

# --- ACS CD Population ---
POP_CD_2024 <- get_acs(
    geography = "congressional district",
    variables = "B01003_001",
    year      = 2024,
    survey    = "acs5"
)

# --- Legislators ---
state_legislators <- read.csv("C:/Users/kritc/Downloads/legislators-current.csv")
state_legislators <- as_tibble(state_legislators) %>%
    select(full_name, stab = state, cd119 = district, party)
# --- Final Result ---
RESULT <- POP_CD_2024 %>%
    mutate(
        cd119 = coalesce(as.integer(str_extract(NAME, "(?<=District )\\d+")), 0L),
        stab  = state.abb[match(str_trim(str_extract(NAME, "[^,]+$")), state.name)]
    ) %>%
    select(stab, GEOID, cd119, ACS_POP = estimate) %>%
    left_join(CD_DATA, by = c("stab", "cd119")) %>%
    na.omit() %>%
    mutate(
        GDP_LOSS_PERC = 100 * (12 * MEAN_LOSS_PER_PERSON * TOTAL_RETIREES) / (GDP * 1e3),
        PERC_RETIREES = 100 * TOTAL_RETIREES / ACS_POP
    ) %>%
    select(-ACS_POP, -GDP)
FINAL_DF <- RESULT %>%
    left_join(state_legislators) %>%
    mutate(
        full_name = ifelse(is.na(full_name), 'Vacant', full_name),
        party     = ifelse(is.na(party), 'Vacant', party)
    ) %>%
    mutate(state_fips = substr(GEOID, 1, 2)) %>%
    arrange(desc(GDP_LOSS_PERC)) %>%
    mutate(GDP_RANK = row_number())
FINAL_DF %>% write.csv('loss_per_cd.csv', row.names = FALSE)
