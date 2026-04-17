library(tidyverse)
library(arrow)
library(haven)

# This R script cleans the raw PSID data, and only keep variables related to income predictions.
# It starts with J360066_v1.dta, which is the Stata file after running the do-file on the 
# original J360066.txt file (both acquired directly from PSID website)

psid_extract_head <- read_dta("J360066_v1.dta")

psid_extract_head <- psid_extract_head %>%
  # rename indicators for the unique id for each person in PSID
  rename("release_num" = "ER30000",
         "interview_num_1968" = "ER30001",
         "person_num_1968" = "ER30002") %>%
  # generate a unique ID for each person in PSID
  mutate(person_id = 1000*interview_num_1968 + person_num_1968)


# === Rename Variables ====
# Rename the varibles (all definitions can be found in PSID do file)
variable_labels <- c(
  release_num = "release_num",
  interview_num_1968 = "interview_num_1968",
  person_num_1968 = "person_num_1968",
  ER32000 = "SEX OF INDIVIDUAL",
  ER32006 = "WHETHER SAMPLE OR NONSAMPLE",
  ER32052 = "YEAR THIS INDIVIDUAL'S COHORT BEGAN",
  V1 = "RELEASE NUMBER",
  V74 = "HDS LABOR INCOME 67",
  V75 = "WIFE LBR INCOME 67",
  ER30003 = "RELATIONSHIP TO HEAD 68",
  ER30004 = "AGE OF INDIVIDUAL 68",
  V441 = "RELEASE NUMBER",
  V514 = "LABOR INC HEAD 68",
  V516 = "LABOR INC WIFE 68",
  ER30020 = "1969 INTERVIEW NUMBER",
  ER30021 = "SEQUENCE NUMBER 69",
  ER30022 = "RELATIONSHIP TO HEAD 69",
  ER30023 = "AGE OF INDIVIDUAL 69",
  V1101 = "RELEASE NUMBER",
  V1196 = "LABOR INC HEAD 69",
  V1198 = "LABOR INC WIFE 69",
  ER30043 = "1970 INTERVIEW NUMBER",
  ER30044 = "SEQUENCE NUMBER 70",
  ER30045 = "RELATIONSHIP TO HEAD 70",
  ER30046 = "AGE OF INDIVIDUAL 70",
  V1801 = "RELEASE NUMBER",
  V1897 = "LABOR INC HEAD 70",
  V1899 = "LABOR INC WIFE 70",
  ER30067 = "1971 INTERVIEW NUMBER",
  ER30068 = "SEQUENCE NUMBER 71",
  ER30069 = "RELATIONSHIP TO HEAD 71",
  ER30070 = "AGE OF INDIVIDUAL 71",
  V2401 = "RELEASE NUMBER",
  V2498 = "LABOR INC HEAD 71",
  V2500 = "LABOR INC WIFE 71",
  ER30091 = "1972 INTERVIEW NUMBER",
  ER30092 = "SEQUENCE NUMBER 72",
  ER30093 = "RELATIONSHIP TO HEAD 72",
  ER30094 = "AGE OF INDIVIDUAL 72",
  V3001 = "RELEASE NUMBER",
  V3051 = "HDS TOT LABOR Y 72",
  V3053 = "WFS LABOR INC 72",
  ER30117 = "1973 INTERVIEW NUMBER",
  ER30118 = "SEQUENCE NUMBER 73",
  ER30119 = "RELATIONSHIP TO HEAD 73",
  ER30120 = "AGE OF INDIVIDUAL 73",
  V3401 = "RELEASE NUMBER",
  V3463 = "TOT LABOR INC-HD 73",
  V3465 = "TOT LABOR INC-WF 73",
  ER30138 = "1974 INTERVIEW NUMBER",
  ER30139 = "SEQUENCE NUMBER 74",
  ER30140 = "RELATIONSHIP TO HEAD 74",
  ER30141 = "AGE OF INDIVIDUAL 74",
  V3801 = "RELEASE NUMBER",
  V3863 = "HEAD LABOR Y 74",
  V3865 = "WIFE LABOR Y 74",
  ER30160 = "1975 INTERVIEW NUMBER",
  ER30161 = "SEQUENCE NUMBER 75",
  ER30162 = "RELATIONSHIP TO HEAD 75",
  ER30163 = "AGE OF INDIVIDUAL 75",
  V4301 = "RELEASE NUMBER",
  V4373 = "ANNUAL WAGES H8 75",
  V4379 = "WIFES ANNUAL WAGE H25 75",
  ER30188 = "1976 INTERVIEW NUMBER",
  ER30189 = "SEQUENCE NUMBER 76",
  ER30190 = "RELATIONSHIP TO HEAD 76",
  ER30191 = "AGE OF INDIVIDUAL 76",
  V5201 = "RELEASE NUMBER",
  V5283 = "HEAD 1976 WAGES",
  V5289 = "WIFE 1976 WAGES",
  ER30217 = "1977 INTERVIEW NUMBER",
  ER30218 = "SEQUENCE NUMBER 77",
  ER30219 = "RELATIONSHIP TO HEAD 77",
  ER30220 = "AGE OF INDIVIDUAL 77",
  V5701 = "RELEASE NUMBER",
  V5782 = "HEAD 1977 WAGES",
  V5788 = "WIFE 1977 WAGE",
  ER30246 = "1978 INTERVIEW NUMBER",
  ER30247 = "SEQUENCE NUMBER 78",
  ER30248 = "RELATIONSHIP TO HEAD 78",
  ER30249 = "AGE OF INDIVIDUAL 78",
  V6301 = "RELEASE NUMBER",
  V6391 = "HEAD 1978 WAGES",
  V6398 = "WIFE 1978 LABOR/WAGE",
  ER30283 = "1979 INTERVIEW NUMBER",
  ER30284 = "SEQUENCE NUMBER 79",
  ER30285 = "RELATIONSHIP TO HEAD 79",
  ER30286 = "AGE OF INDIVIDUAL 79",
  V6901 = "RELEASE NUMBER",
  V6981 = "HEAD 1979 WAGES",
  V6988 = "WIFE 1979 LABOR/WAGE",
  ER30313 = "1980 INTERVIEW NUMBER",
  ER30314 = "SEQUENCE NUMBER 80",
  ER30315 = "RELATIONSHIP TO HEAD 80",
  ER30316 = "AGE OF INDIVIDUAL 80",
  V7501 = "RELEASE NUMBER",
  V7573 = "HEAD 1980 WAGES",
  V7580 = "WIFE 1980 LABOR/WAGE",
  ER30343 = "1981 INTERVIEW NUMBER",
  ER30344 = "SEQUENCE NUMBER 81",
  ER30345 = "RELATIONSHIP TO HEAD 81",
  ER30346 = "AGE OF INDIVIDUAL 81",
  V8201 = "RELEASE NUMBER",
  V8265 = "HEAD 1981 WAGES",
  V8273 = "WIFE 1981 LABOR/WAGE",
  ER30373 = "1982 INTERVIEW NUMBER",
  ER30374 = "SEQUENCE NUMBER 82",
  ER30375 = "RELATIONSHIP TO HEAD 82",
  ER30376 = "AGE OF INDIVIDUAL 82",
  V8801 = "RELEASE NUMBER",
  V8873 = "HEAD 1982 WAGES",
  V8881 = "WIFE 1982 LABOR/WAGE",
  ER30399 = "1983 INTERVIEW NUMBER",
  ER30400 = "SEQUENCE NUMBER 83",
  ER30401 = "RELATIONSHIP TO HEAD 83",
  ER30402 = "AGE OF INDIVIDUAL 83",
  V10001 = "RELEASE NUMBER",
  V10257 = "ACC HEAD 1983 WAGES",
  V10263 = "WIFE 1983 LABOR/Y",
  ER30429 = "1984 INTERVIEW NUMBER",
  ER30430 = "SEQUENCE NUMBER 84",
  ER30431 = "RELATIONSHIP TO HEAD 84",
  ER30432 = "AGE OF INDIVIDUAL 84",
  V11101 = "RELEASE NUMBER",
  V11397 = "HEAD 84 WAGES",
  V11404 = "WIFE 84 LABOR/WAGE",
  ER30463 = "1985 INTERVIEW NUMBER",
  ER30464 = "SEQUENCE NUMBER 85",
  ER30465 = "RELATIONSHIP TO HEAD 85",
  ER30466 = "AGE OF INDIVIDUAL 85",
  V12501 = "RELEASE NUMBER",
  V12796 = "HEAD 85 WAGES",
  V12803 = "WIFE 85 LABOR/WAGE",
  ER30498 = "1986 INTERVIEW NUMBER",
  ER30499 = "SEQUENCE NUMBER 86",
  ER30500 = "RELATIONSHIP TO HEAD 86",
  ER30501 = "AGE OF INDIVIDUAL 86",
  V13701 = "RELEASE NUMBER",
  V13898 = "HEAD 86 WAGES",
  V13905 = "WIFE 86 LABOR/WAGE",
  ER30535 = "1987 INTERVIEW NUMBER",
  ER30536 = "SEQUENCE NUMBER 87",
  ER30537 = "RELATIONSHIP TO HEAD 87",
  ER30538 = "AGE OF INDIVIDUAL 87",
  V14801 = "RELEASE NUMBER",
  V14913 = "HEAD 87 WAGES",
  V14920 = "WIFE 87 LABOR/WAGE",
  ER30570 = "1988 INTERVIEW NUMBER",
  ER30571 = "SEQUENCE NUMBER 88",
  ER30572 = "RELATION TO HEAD 88",
  ER30573 = "AGE OF INDIVIDUAL 88",
  V16301 = "RELEASE NUMBER",
  V16413 = "HEAD 88 WAGES",
  V16420 = "WIFE 88 LABOR/WAGE",
  ER30606 = "1989 INTERVIEW NUMBER",
  ER30607 = "SEQUENCE NUMBER 89",
  ER30608 = "RELATION TO HEAD 89",
  ER30609 = "AGE OF INDIVIDUAL 89",
  V17701 = "RELEASE NUMBER",
  V17829 = "HEAD 89 WAGES",
  V17836 = "WIFE 89 LABOR/WAGE",
  ER30642 = "1990 INTERVIEW NUMBER",
  ER30643 = "SEQUENCE NUMBER 90",
  ER30644 = "RELATION TO HEAD 90",
  ER30645 = "AGE OF INDIVIDUAL 90",
  V19001 = "RELEASE NUMBER",
  V19129 = "HEAD 90 WAGES",
  V19136 = "WIFE 90 LABOR/WAGE",
  ER30689 = "1991 INTERVIEW NUMBER",
  ER30690 = "SEQUENCE NUMBER 91",
  ER30691 = "RELATION TO HEAD 91",
  ER30692 = "AGE OF INDIVIDUAL 91",
  V20301 = "RELEASE NUMBER",
  V20429 = "HEAD 91 WAGES",
  V20436 = "WIFE 91 LABOR/WAGE",
  ER30733 = "1992 INTERVIEW NUMBER",
  ER30734 = "SEQUENCE NUMBER 92",
  ER30735 = "RELATION TO HEAD 92",
  ER30736 = "AGE OF INDIVIDUAL 92",
  V21601 = "RELEASE NUMBER",
  V23323 = "HD 1992 TOTAL LABOR INCOME",
  V23324 = "WF 1992 TOTAL LABOR INCOME",
  ER30806 = "1993 INTERVIEW NUMBER",
  ER30807 = "SEQUENCE NUMBER 93",
  ER30808 = "RELATION TO HEAD 93",
  ER30809 = "AGE OF INDIVIDUAL 93",
  ER2001 = "RELEASE NUMBER",
  ER4140 = "LABOR INCOME OF HEAD-1993",
  ER4144 = "LABOR INCOME OF WIFE-1993",
  ER33101 = "1994 INTERVIEW NUMBER",
  ER33102 = "SEQUENCE NUMBER 94",
  ER33103 = "RELATION TO HEAD 94",
  ER33104 = "AGE OF INDIVIDUAL 94",
  ER5001 = "RELEASE NUMBER",
  ER6980 = "LABOR INCOME OF HEAD-1994",
  ER6984 = "LABOR INCOME OF WIFE-1994",
  ER33201 = "1995 INTERVIEW NUMBER",
  ER33202 = "SEQUENCE NUMBER 95",
  ER33203 = "RELATION TO HEAD 95",
  ER33204 = "AGE OF INDIVIDUAL 95",
  ER7001 = "RELEASE NUMBER",
  ER9231 = "LABOR INCOME OF HEAD-1995",
  ER9235 = "LABOR INCOME OF WIFE-1995",
  ER33301 = "1996 INTERVIEW NUMBER",
  ER33302 = "SEQUENCE NUMBER 96",
  ER33303 = "RELATION TO HEAD 96",
  ER33304 = "AGE OF INDIVIDUAL 96",
  ER10001 = "RELEASE NUMBER",
  ER12080 = "LABOR INCOME-HEAD 96",
  ER12082 = "LABOR INCOME-WIFE 96",
  ER33401 = "1997 INTERVIEW NUMBER",
  ER33402 = "SEQUENCE NUMBER 97",
  ER33403 = "RELATION TO HEAD 97",
  ER33404 = "AGE OF INDIVIDUAL 97",
  ER13001 = "RELEASE NUMBER",
  ER16463 = "LABOR INCOME-HEAD 98",
  ER16465 = "LABOR INCOME-WIFE 98",
  ER33501 = "1999 INTERVIEW NUMBER",
  ER33502 = "SEQUENCE NUMBER 99",
  ER33503 = "RELATION TO HEAD 99",
  ER33504 = "AGE OF INDIVIDUAL 99",
  ER17001 = "RELEASE NUMBER",
  ER18561 = "G13 WAGES/SALARY OF HEAD 00",
  ER18930 = "WAGES/SALARY OF WIFE 00",
  ER33601 = "2001 INTERVIEW NUMBER",
  ER33602 = "SEQUENCE NUMBER 01",
  ER33603 = "RELATION TO HEAD 01",
  ER33604 = "AGE OF INDIVIDUAL 01",
  ER21001 = "RELEASE NUMBER",
  ER21929 = "G13 WAGES/SALARY OF HEAD 02",
  ER22300 = "WAGES/SALARY OF WIFE 02",
  ER33701 = "2003 INTERVIEW NUMBER",
  ER33702 = "SEQUENCE NUMBER 03",
  ER33703 = "RELATION TO HEAD 03",
  ER33704 = "AGE OF INDIVIDUAL 03",
  ER25001 = "RELEASE NUMBER",
  ER25910 = "G13 WAGES/SALARY OF HEAD 04",
  ER26281 = "WAGES/SALARY OF WIFE 04",
  ER33801 = "2005 INTERVIEW NUMBER",
  ER33802 = "SEQUENCE NUMBER 05",
  ER33803 = "RELATION TO HEAD 05",
  ER33804 = "AGE OF INDIVIDUAL 05",
  ER36001 = "RELEASE NUMBER",
  ER36928 = "G13 WAGES/SALARY OF HEAD 06",
  ER37299 = "WAGES/SALARY OF WIFE 06",
  ER33901 = "2007 INTERVIEW NUMBER",
  ER33902 = "SEQUENCE NUMBER 07",
  ER33903 = "RELATION TO HEAD 07",
  ER33904 = "AGE OF INDIVIDUAL 07",
  ER42001 = "RELEASE NUMBER",
  ER42919 = "G13 WAGES/SALARY OF HEAD 08",
  ER43290 = "WAGES/SALARY OF WIFE 08",
  ER34001 = "2009 INTERVIEW NUMBER",
  ER34002 = "SEQUENCE NUMBER 09",
  ER34003 = "RELATION TO HEAD 09",
  ER34004 = "AGE OF INDIVIDUAL 09",
  ER47301 = "RELEASE NUMBER",
  ER48241 = "G13 WAGES/SALARY OF HEAD 10",
  ER48615 = "WAGES/SALARY OF WIFE 10",
  ER34101 = "2011 INTERVIEW NUMBER",
  ER34102 = "SEQUENCE NUMBER 11",
  ER34103 = "RELATION TO HEAD 11",
  ER34104 = "AGE OF INDIVIDUAL 11",
  ER53001 = "RELEASE NUMBER",
  ER53935 = "G13 WAGES/SALARY-HEAD 12",
  ER54309 = "WAGES/SALARY OF WIFE 12",
  ER34201 = "2013 INTERVIEW NUMBER",
  ER34202 = "SEQUENCE NUMBER 13",
  ER34203 = "RELATION TO HEAD 13",
  ER34204 = "AGE OF INDIVIDUAL 13",
  ER60001 = "RELEASE NUMBER",
  ER60994 = "G13 WAGES/SALARY-HEAD 14",
  ER61349 = "G52 WAGES/SALARY OF SPOUSE 14",
  ER34301 = "2015 INTERVIEW NUMBER",
  ER34302 = "SEQUENCE NUMBER 15",
  ER34303 = "RELATION TO HEAD 15",
  ER34305 = "AGE OF INDIVIDUAL 15",
  ER66001 = "RELEASE NUMBER",
  ER67046 = "G13 WAGES/SALARY-REFERENCE PERSON 16",
  ER67401 = "G13 WAGES/SALARY OF SPOUSE 16",
  ER34501 = "2017 INTERVIEW NUMBER",
  ER34502 = "SEQUENCE NUMBER 17",
  ER34503 = "RELATION TO REFERENCE PERSON 17",
  ER34504 = "AGE OF INDIVIDUAL 17",
  ER72001 = "RELEASE NUMBER",
  ER73069 = "G13 WAGES/SALARY-REFERENCE PERSON 18",
  ER73424 = "G13 WAGES/SALARY OF SPOUSE 18",
  ER34701 = "2019 INTERVIEW NUMBER",
  ER34702 = "SEQUENCE NUMBER 19",
  ER34703 = "RELATION TO REFERENCE PERSON 19",
  ER34704 = "AGE OF INDIVIDUAL 19",
  ER78001 = "RELEASE NUMBER",
  ER79146 = "G13 WAGES/SALARY-REFERENCE PERSON 20",
  ER79526 = "G13 WAGES/SALARY OF SPOUSE 20",
  ER34901 = "2021 INTERVIEW NUMBER",
  ER34902 = "SEQUENCE NUMBER 21",
  ER34903 = "RELATION TO REFERENCE PERSON 21",
  ER34904 = "AGE OF INDIVIDUAL 21",
  ER82001 = "RELEASE NUMBER",
  ER83121 = "G13 WAGES/SALARY-REFERENCE PERSON 22",
  ER83495 = "G13 WAGES/SALARY OF SPOUSE 22",
  ER35101 = "2023 INTERVIEW NUMBER",
  ER35102 = "SEQUENCE NUMBER 23",
  ER35103 = "RELATION TO REFERENCE PERSON 23",
  ER35104 = "AGE OF INDIVIDUAL 23",
  person_id = "person_id"
)

# === Apply the rename dictionary ====
rename_map <- setNames(names(variable_labels), variable_labels)
# colnames(psid2) <- rename_map[colnames(psid2)]
colnames(psid_extract_head) <- variable_labels[colnames(psid_extract_head)]

# Save the renamed PSID .rds data
saveRDS(psid_extract_head, "psid_extractHD_renamed.rds")

# Load the data
psid_extract_head <- readRDS("psid_extractHD_renamed.rds")

# Select all the variables for income(wage income/labor income)
psid_hd_sel1 <- psid_extract_head %>%
  select(person_id,`SEX OF INDIVIDUAL`,contains("INC"))
psid_hd_sel2 <- psid_extract_head %>%
  select(person_id,contains("WAGE"))
overlap <- setdiff(intersect(colnames(psid_hd_sel1), colnames(psid_hd_sel2)), "person_id")
psid_hd_sel3 <- psid_extract_head %>%
  select(person_id,contains("LABOR"))
# Delete the overlaps
overlap2 <- setdiff(intersect(colnames(psid_hd_sel3), colnames(psid_hd_sel2)), "person_id")
overlap3 <- setdiff(intersect(colnames(psid_hd_sel3), colnames(psid_hd_sel1)), "person_id")

# Merge all the income variables together
psid_hd_inc <- psid_hd_sel1 %>%
  left_join(
    psid_hd_sel2 %>% 
      select(-any_of(overlap)),
    by = "person_id") %>%
  left_join(
    psid_hd_sel3 %>%
      select(-any_of(overlap2)) %>%
      select(-any_of(overlap3)),
    by = "person_id"
  )

write_dataset(psid_hd_inc, "interdata/psid_HDinc_full")

psid_hd_inc_ar <- open_dataset("interdata/psid_HDinc_full")
psid_hd_inc <- psid_hd_inc_ar %>%
  collect()

# === Amendment for original data====
# The current Head Income Data does not have income info year 1893
# Generate Append df for head's income for 1983 PSID
J360038_v1 <- read_dta("C:/Users/YundiHou/OneDrive - Cato Institute/Desktop/ss_is/revision/initial/J360038/J360038_v1.dta")
psid_append1 <- J360038_v1 %>%
  select(ER30000, ER30001, ER30002, V10256) %>%
  rename("release_num" = "ER30000",
         "interview_num_1968" = "ER30001",
         "person_num_1968" = "ER30002",
         "HEAD 1983 WAGES" = "V10256")
write_dataset(psid_append1, "interdata/psid_append1",
              existing_data_behavior = "overwrite")

# === Correct the HEAD 1983 WAGES ====
psid_append1_ar <- open_dataset("interdata/psid_append1")
psid_append1 <- psid_append1_ar %>%
  collect()

# Merge the 1983 PSID Income
psid_hd_inc2 <- psid_hd_inc %>%
  left_join(psid_append1 %>%
              mutate(person_id = (1000*interview_num_1968 + person_num_1968)) %>%
              select(-release_num, -person_num_1968, -interview_num_1968),
            by = "person_id") %>%
  select(-`ACC HEAD 1983 WAGES`)
write_dataset(psid_hd_inc2, "interdata/psid_HDinc_full2",
              existing_data_behavior = "overwrite")

# === Rename Income Var for Regex ====
# ===== income name dict1 full ======
psid_inc_ar <- open_dataset("interdata/psid_inc_full2")
psid_inc <- psid_inc_ar %>%
  collect()

# Rename all the income to such format: "income year(2 or 4 digit)"
# so that we could use regression expression to get the 2-digit year info from variables
psid_hd_inc2 <- psid_hd_inc2 %>%
  rename(
    "HEAD WAGES 76" = "HEAD 1976 WAGES",
    "WIFE WAGES 76" = "WIFE 1976 WAGES",
    "HEAD WAGES 77" = "HEAD 1977 WAGES",
    "WIFE WAGES 77" = "WIFE 1977 WAGE",
    "HEAD WAGES 78" = "HEAD 1978 WAGES",
    "WIFE LABOR/WAGE 78" = "WIFE 1978 LABOR/WAGE", 
    "HEAD WAGES 1979" = "HEAD 1979 WAGES",
    "WIFE WAGES 1979" = "WIFE 1979 LABOR/WAGE",
    "HEAD WAGES 80" = "HEAD 1980 WAGES",
    "WIFE WAGES 80" = "WIFE 1980 LABOR/WAGE",
    "HEAD WAGES 81" = "HEAD 1981 WAGES",
    "WIFE WAGES 81" = "WIFE 1981 LABOR/WAGE",
    "HEAD WAGES 82" = "HEAD 1982 WAGES",
    "WIFE LABOR/WAGE 82" = "WIFE 1982 LABOR/WAGE",
    "HEAD WAGES 1983" = "HEAD 1983 WAGES",
    "WIFE WAGES 1983" = "WIFE 1983 LABOR/Y",
    "HEAD WAGES 84" = "HEAD 84 WAGES",
    "WIFE LABOR/WAGE 84" = "WIFE 84 LABOR/WAGE",
    V12796 = "HEAD 85 WAGES",
    V12803 = "WIFE 85 LABOR/WAGE",
    "HEAD WAGES 85" = "HEAD 85 WAGES",
    "WIFE LABOR/WAGE 85" = "WIFE 85 LABOR/WAGE",
    # V13898 = "HEAD 86 WAGES",
    # V13905 = "WIFE 86 LABOR/WAGE",
    "HEAD WAGES 86" = "HEAD 86 WAGES",
    "WIFE LABOR/WAGE 86" = "WIFE 86 LABOR/WAGE",
    "HEAD WAGES 87" = "HEAD 87 WAGES",
    "WIFE LABOR/WAGE 87" = "WIFE 87 LABOR/WAGE",
    "HEAD WAGES 88" = "HEAD 88 WAGES",
    "WIFE LABOR/WAGE 88" = "WIFE 88 LABOR/WAGE",
    "HEAD WAGES 89" = "HEAD 89 WAGES",
    "WIFE LABOR/WAGE 89" = "WIFE 89 LABOR/WAGE",
    "HEAD WAGES 90" = "HEAD 90 WAGES",
    "WIFE LABOR/WAGE 90" = "WIFE 90 LABOR/WAGE",
    "HEAD WAGES 91" = "HEAD 91 WAGES",
    "WIFE LABOR/WAGE 91" = "WIFE 91 LABOR/WAGE",
    "HD TOTAL LABOR INCOME 92" = "HD 1992 TOTAL LABOR INCOME",
    "WF TOTAL LABOR INCOME 92" = "WF 1992 TOTAL LABOR INCOME",
  )
write_dataset(psid_hd_inc2, "interdata/psid_HDinc_full3",
              existing_data_behavior = "overwrite")

# === Income of Head ====
# Load the saved data
psid_income_hd <- open_dataset("interdata/psid_HDinc_full3") %>%
  collect()

# Select all the variables for head income
psid_income_head1 <- psid_income_hd %>%
  select(person_id,contains("HD")) 
psid_income_head2 <- psid_income_hd %>%
  select(person_id,contains("HEAD"))
psid_income_head3 <- psid_income_hd %>%
  select(person_id,contains("REFERENCE"))
psid_income_head4 <- psid_income_hd %>%
  select(person_id,`ANNUAL WAGES H8 75`)

# Join all head-income variables together
psid_income_head <- psid_income_head1 %>%
  left_join(psid_income_head2,
            by = "person_id") %>%
  left_join(psid_income_head3,
            by = "person_id") %>%
  left_join(psid_income_head4,
            by = "person_id")

# Pivot the dataframe into a long df version with person_id, year, income
psid_hdinc_long <- psid_income_head %>%
  pivot_longer(
    cols = -person_id,
    names_to = "income_def",
    values_to = "income_value"
  ) %>%
  # extract corresponding year info from variable name
  mutate(
    year2 = as.integer(str_extract(income_def, "\\d{2}$"))
  ) %>%
  # Transform year into 4 digit version
  mutate(year = if_else(
    year2 > 25, (1900+year2), (2000+year2)
  )) %>%
  select(-year2)
write_dataset(psid_hdinc_long, "interdata/psid_HeadIncome_long1",
              existing_data_behavior = "overwrite")

# === PSID HEAD AGE ====
# Get the age for heads
psid_extract_head <- readRDS("psid_extractHD_renamed.rds")

psid_hd_age <- psid_extract_head %>%
  select(person_id,contains("AGE OF IN")) %>%
  mutate(across(starts_with("AGE"), ~ na_if(., 999))) 
psid_hd_age_long <- psid_hd_age %>%
  pivot_longer(
    cols = -person_id,
    names_to = "age_def",
    values_to = "age"
  ) %>%
  mutate(
    year2 = as.integer(str_extract(age_def, "\\d{2}$"))
  ) %>%
  mutate(year = if_else(
    year2 > 25, (1900+year2), (2000+year2)
  )) %>%
  select(-year2)

write_dataset(psid_hd_age_long, "interdata/psid_HeadAge_long",
              existing_data_behavior = "overwrite")

# === Cohort & Others ====
psid_hd_age_long <- open_dataset("interdata/psid_HeadAge_long") %>%
  collect()

# Calculate Birth Year & 5-year Cohort
psid_hd_birthyr <- psid_hd_age_long  %>%
  filter(age != 0) %>%
  mutate(birth_year_raw = year - age) %>%
  group_by(person_id, birth_year_raw) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(person_id) %>%
  arrange(desc(n), birth_year_raw) %>%  
  slice(1) %>%
  ungroup() %>%
  select(person_id, birth_year = birth_year_raw) %>%
  # generate the 5 year cohort
  mutate(cohort_5yr = (floor(birth_year/5)*5))

# Sex & Sample
psid_extract_head <- readRDS("psid_extractHD_renamed.rds")
psid_sex <- psid_extract_head %>%
  select(release_num, person_id,
         `SEX OF INDIVIDUAL`,
         `WHETHER SAMPLE OR NONSAMPLE`) %>%
  rename(sex = `SEX OF INDIVIDUAL`,
         sample_nonsample = `WHETHER SAMPLE OR NONSAMPLE`)
psid_hd_age_add <- psid_hd_age_long %>%
  left_join(psid_sex,
            by = "person_id") %>%
  left_join(psid_hd_birthyr ,
            by = "person_id")

write_dataset(psid_hd_age_add, "interdata/psid_HeadAge_add1",
              existing_data_behavior = "overwrite")

# === Merge Demo with Inc ====
psid_hdinc_long <- open_dataset("interdata/psid_HeadIncome_long1") %>%
  collect()
psid_hdinc_full <- psid_hdinc_long %>%
  left_join(psid_hd_age_add,
            by = c("person_id", "year"))

write_dataset(psid_hdinc_full, "interdata/psid_HeadInc_full",
              existing_data_behavior = "overwrite")

# === Merge AWI ====
psid_hdinc_full <- open_dataset("interdata/psid_HeadInc_full") %>%
  collect()

df_econ_assumptions <- read_csv("df_econ_assumptions.csv")

AWI_df <- df_econ_assumptions %>%
  select(REFYEAR, AWI, ALTERNATIVE) %>%
  rename(year = REFYEAR) %>%
  arrange(year) %>%
  filter(year <= 2022) %>%
  select(-ALTERNATIVE) %>%
  filter(is.na(AWI) == FALSE) %>%
  bind_rows(data.frame(
    year = 1951:1969,
    AWI = c(2799.16, 2973.32,
            3139.44, 3155.64,
            3301.44, 3532.26,
            3641.72, 3673.80,
            3855.80, 4007.12,
            4086.76, 4291.40,
            4396.64, 4576.32,
            4658.72, 4938.36,
            5213.44, 5571.76,
            5893.76)
  )) %>%
  arrange(year) 

psid_hdinc_adj <- psid_hdinc_full %>%
  filter(age > 0) %>%
  filter(is.na(age) == FALSE) %>%
  left_join(AWI_df,
            by = "year") %>%
  filter(age >= 18) %>%
  mutate(sex = if_else(
    sex == 1 , 0, 1
  )) %>%
  mutate(sex = factor(sex,
                      levels = c(0, 1))) %>%
  mutate(age_sq = age*age) %>%
  mutate(income_real_a = income_value/AWI)

write_dataset(psid_hdinc_adj , "interdata/psid_HeadInc_AWIadj",
              existing_data_behavior = "overwrite")

psid_hdinc_adj %>%
  count(sex)

psid_hdinc_adj <- open_dataset("interdata/psid_HeadInc_AWIadj_ver2") |>
  collect()

# Select the observations for a Quantile Rnadom Forest Model
psid_hdinc_adj2 <- psid_hdinc_adj |>
  filter(age <= 70) |>
  filter(income_value < 999999) |>
  filter(is.na(income_value) == FALSE) |>
  # !!! This step will drop all the 0 income in the PSID
  # !!! I did this because there is no 0 income for people in the labor force in our simulation data
  # !!! But anyone who works on this model should be very cautious about this before proceeding from here
  filter(income_value > 0)

# Generate the variables need for the Quantile Random Forest Model
psid_hdinc_model <- psid_hdinc_adj2 %>%
  arrange(person_id, year) %>%
  group_by(person_id) %>%
  mutate(
    year_next = lead(year),
    age_next = lead(age),
    income_real_next = lead(income_real_a),
    gap = year_next - year
  ) %>%
  ungroup() %>%
  filter(
    gap == 1,
    !is.na(age),
    !is.na(sex),
    !is.na(cohort_5yr),
    !is.na(income_real_a),
    !is.na(income_real_next)
  ) %>%
  mutate(
    sex = factor(sex, levels = c(0, 1))
  )

# Drop the invalid values for income
psid_hdinc_model <- psid_hdinc_model |>
  filter(income_value < 999999)

# save the data
saveRDS(psid_hdinc_model, "psid_hdinc_model_v2.RDS")


