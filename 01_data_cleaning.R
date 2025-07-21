########################## Load Required Libraries ########################## 

library(data.table)    
library(dplyr)         
library(readr)        
library(tidyr)         

########################## NIS Denominator Creation ########################## 

# Load in NIS denominator files 
nis_2014_denom <- read_csv('~/Desktop/HCUP Paper Repository/nis_2014_denom.csv')
nis_2015_denom <- read_csv('~/Desktop/HCUP Paper Repository/nis_2015_denom.csv')
nis_2016_denom <- read_csv('~/Desktop/HCUP Paper Repository/nis_2016_denom.csv')
nis_2017_denom <- read_csv('~/Desktop/HCUP Paper Repository/nis_2017_denom.csv')
nis_2018_denom <- read_csv('~/Desktop/HCUP Paper Repository/nis_2018_denom.csv')
nis_2019_denom <- read_csv('~/Desktop/HCUP Paper Repository/nis_2019_denom.csv')
nis_2020_denom <- read_csv('~/Desktop/HCUP Paper Repository/nis_2020_denom.csv')

# Bind them together
nis_denom_all_years <- bind_rows(
  nis_2014_denom,
  nis_2015_denom,
  nis_2016_denom,
  nis_2017_denom,
  nis_2018_denom,
  nis_2019_denom,
  nis_2020_denom
) %>% select(-1)

# Group by relevant variables and aggregate number of encounters
nis_denom_aggregated <- nis_denom_all_years %>%
  group_by(HOSP_DIVISION, FEMALE, RACE, AGE_GROUP, YEAR) %>%
  summarise(nEncounters = sum(nEncounters, na.rm = TRUE), .groups = "drop") %>%
  rename(age_group = AGE_GROUP, censusdivision = HOSP_DIVISION, encounters = nEncounters)

# Process "age_group" column to match OERWD
nis_denom_aggregated <- nis_denom_aggregated %>%
  mutate(age_group = case_when(
    age_group == "18-34" ~ "18 to 34",
    age_group == "35-64" ~ "35 to 64",
    age_group == "65+"   ~ "65 and Over",
    TRUE ~ age_group
  )) 

# Add weights corresponing to NIS survey methods
nis_denom_aggregated <- nis_denom_aggregated %>% mutate(weight = 5)


########################## OERWD Denominator Creation ########################## 

# Load in OERWD denominator file
oerwd_month <- fread('~/Desktop/HCUP Paper Repository/rwd_denominators_month.csv')

# Cleaning
oerwd_month <- oerwd_month %>% 
  rename(YEAR = date2_year, nRWDPatient_m = nPatient, nRWDEncounter_m = nEncounter) %>%
  filter(!is.na(prefstate), YEAR %in% 2014:2020) %>%
  select(-V1)

oerwd_year <- oerwd_month[, .(nRWDEncounter_y = sum(nRWDEncounter_m)), 
                          by = .(YEAR, prefstate, prefethnicity, prefrace, prefgender, age_group)]
oerwd_denom <- oerwd_year[age_group != "0 to 17"]

# Create census division variable from "prefstate" column 
oerwd_censusdivisions <- fread('~/Desktop/HCUP Paper Repository/censusdivisions.csv')
oerwd_denom <- full_join(oerwd_denom, oerwd_censusdivisions, by = "prefstate") %>% as.data.table()

oerwd_denom <- oerwd_denom[, .(nRWDEncounter_y = sum(nRWDEncounter_y)), 
                           by = .(YEAR, censusdivision, prefethnicity, prefrace, prefgender, age_group)]

# Match NIS "RACE" variable
oerwd_denom[, RACE := fcase(
  prefrace == "White" & prefethnicity %in% c("Non-Hispanic", "Unknown"), 1,
  prefrace == "Black or African American" & prefethnicity %in% c("Non-Hispanic", "Unknown"), 2,
  prefethnicity == "Hispanic or Latino", 3,
  prefrace %in% c("Asian", "Native Hawaiian or Other Pacific Islander") & prefethnicity %in% c("Non-Hispanic", "Unknown"), 4,
  prefrace == "American Indian or Alaska Native" & prefethnicity %in% c("Non-Hispanic", "Unknown"), 5,
  prefrace == "Other" & prefethnicity %in% c("Unknown", "Non-Hispanic"), 6,
  prefrace == "Unknown" & prefethnicity == "Non-Hispanic", 7,
  prefrace == "Unknown" & prefethnicity == "Hispanic or Latino", 3,
  prefrace == "Unknown" & prefethnicity == "Unknown", 7
)]

# Match NIS "FEMALE" variable
oerwd_denom[, FEMALE := fcase(
  prefgender == "Female", 1,
  prefgender == "Male", 0,
  prefgender == "Other or Unknown", 2
)]

oerwd_denom <- oerwd_denom[, .(nRWDEncounter_y = sum(nRWDEncounter_y)), 
                           by = .(YEAR, censusdivision, RACE, FEMALE, age_group)]

# Drop "Unknown" and NaN values
oerwd_denom <- oerwd_denom[
  !is.na(censusdivision) & RACE != 7 & FEMALE != 2 & age_group != "Unknown"
]

setnames(oerwd_denom, "nRWDEncounter_y", "encounters")


########################## NIS Aspergillosis Table Creation ########################## 

asp_2014_2020_combined_clean <- fread("~/Desktop/HCUP Paper Repository/asp_2014_2020_combined_clean.csv")

asp_2014_2020_combined_clean <- asp_2014_2020_combined_clean %>%
  mutate(ASP = 1,
         age_group = case_when(
           AGE < 18 ~ "Under 18",
           AGE >= 18 & AGE < 35 ~ "18 to 34",
           AGE >= 35 & AGE < 65 ~ "35 to 64",
           AGE >= 65 ~ "65 and Over"
         )
  )

asp_2014_2020_combined_clean <- asp_2014_2020_combined_clean[age_group != "Under 18"]

asp_2014_2020_summary <- asp_2014_2020_combined_clean[, .(nEncounters = sum(ASP)), 
                                                      .(YEAR, HOSP_DIVISION, 
                                                        FEMALE, RACE, 
                                                        age_group)]

asp_2014_2020_summary <- asp_2014_2020_summary[complete.cases(asp_2014_2020_summary)]

# fill 0's for combiantions of variables
race_vals <- c(1, 3, 2, 4, 6, 5)
age_vals <- c("65 and Over", "35 to 64", "18 to 34")
female_vals <- c(0, 1)
year_vals <- 2014:2020
div_vals <- 1:9

full_grid <- CJ(
  RACE = race_vals,
  age_group = age_vals,
  FEMALE = female_vals,
  YEAR = year_vals,
  HOSP_DIVISION = div_vals,
  unique = TRUE
)

asp_2014_2020_summary_full <- merge(
  full_grid,
  asp_2014_2020_summary,
  by = c("RACE", "age_group", "FEMALE", "YEAR", "HOSP_DIVISION"),
  all.x = TRUE
)

asp_2014_2020_summary_full[is.na(nEncounters), nEncounters := 0]
asp_2014_2020_summary_full = asp_2014_2020_summary_full %>% rename(censusdivision = HOSP_DIVISION, cases = nEncounters)

nis_asp_final <- merge(
  asp_2014_2020_summary_full,
  nis_denom_aggregated,
  by = c("RACE", "age_group", "FEMALE", "YEAR", "censusdivision"),
  all = TRUE
) %>% mutate(id = 0)


########################## NIS Histoplasmosis Table Creation ########################## 

hist_2014_2020_combined_clean <- fread("~/Desktop/HCUP Paper Repository/hist_2014_2020_combined_clean.csv")

hist_2014_2020_combined_clean <- hist_2014_2020_combined_clean %>%
  mutate(hist = 1,
         age_group = case_when(
           AGE < 18 ~ "Under 18",
           AGE >= 18 & AGE < 35 ~ "18 to 34",
           AGE >= 35 & AGE < 65 ~ "35 to 64",
           AGE >= 65 ~ "65 and Over"
         )
  )

hist_2014_2020_combined_clean <- hist_2014_2020_combined_clean[age_group != "Under 18"]

hist_2014_2020_summary <- hist_2014_2020_combined_clean[, .(nEncounters = sum(hist)), 
                                                        .(YEAR, HOSP_DIVISION, 
                                                          FEMALE, RACE, 
                                                          age_group)]

hist_2014_2020_summary <- hist_2014_2020_summary[complete.cases(hist_2014_2020_summary)]

race_vals <- c(1, 3, 2, 4, 6, 5)
age_vals <- c("65 and Over", "35 to 64", "18 to 34")
female_vals <- c(0, 1)
year_vals <- 2014:2020
div_vals <- 1:9

full_grid <- CJ(
  RACE = race_vals,
  age_group = age_vals,
  FEMALE = female_vals,
  YEAR = year_vals,
  HOSP_DIVISION = div_vals,
  unique = TRUE
)

hist_2014_2020_summary_full <- merge(
  full_grid,
  hist_2014_2020_summary,
  by = c("RACE", "age_group", "FEMALE", "YEAR", "HOSP_DIVISION"),
  all.x = TRUE
)

hist_2014_2020_summary_full[is.na(nEncounters), nEncounters := 0]
hist_2014_2020_summary_full = hist_2014_2020_summary_full %>% rename(censusdivision = HOSP_DIVISION, cases = nEncounters)

nis_hist_final <- merge(
  hist_2014_2020_summary_full,
  nis_denom_aggregated,
  by = c("RACE", "age_group", "FEMALE", "YEAR", "censusdivision"),
  all = TRUE 
) %>% mutate(id = 0)




########################## OERWD Cases Table Creation ########################## 

oerwd_w <- fread("~/Desktop/HCUP Paper Repository/ocrwd_cohort_year_full_w.csv") %>%
  rename(
    cases = nEncounters,
    encounters = nRWDEncounter_y,
    cases_w = nEncounters_w,
    encounters_w = nRWDEncounter_y_w) %>% 
  filter(RACE != 7, FEMALE != 2, age_group != "Unknown") %>%
  mutate(id = 1)

oerwd_w = oerwd_w %>% select(-cases_w, -encounters_w)

oerwd_asp <- oerwd_w %>%
  filter(Disease == "Aspergillosis") %>%
  select(-Disease)

oerwd_hist <- oerwd_w %>%
  filter(Disease == "Histoplasmosis") %>%
  select(-Disease)

asp_final <- bind_rows(oerwd_asp, nis_asp_final) %>%
  mutate(
    RACE = as.factor(RACE),
    censusdivision = as.factor(censusdivision),
    age_group = factor(age_group, levels = c("18 to 34", "35 to 64", "65 and Over")),
    YEAR = factor(YEAR),
    FEMALE = factor(FEMALE, levels = c(0, 1)),
    id = as.factor(id),
    weight = ifelse(id == 1, 1, weight) # unweighted OERWD
  ) %>%
  drop_na(censusdivision, RACE, FEMALE, age_group, YEAR, cases, encounters)

asp_final_w <- bind_rows(oerwd_asp, nis_asp_final) %>%
  mutate(
    RACE = as.factor(RACE),
    censusdivision = as.factor(censusdivision),
    age_group = factor(age_group, levels = c("18 to 34", "35 to 64", "65 and Over")),
    YEAR = factor(YEAR),
    FEMALE = factor(FEMALE, levels = c(0, 1)),
    id = as.factor(id)
  ) %>%
  drop_na(censusdivision, RACE, FEMALE, age_group, YEAR, cases, encounters)

hist_final <- bind_rows(oerwd_hist, nis_hist_final) %>%
  mutate(
    RACE = as.factor(RACE),
    censusdivision = as.factor(censusdivision),
    age_group = factor(age_group, levels = c("18 to 34", "35 to 64", "65 and Over")),
    YEAR = factor(YEAR),
    FEMALE = factor(FEMALE, levels = c(0, 1)),
    id = as.factor(id),
    weight = ifelse(id == 1, 1, weight) # unweighted OERWD
  ) %>%
  drop_na(censusdivision, RACE, FEMALE, age_group, YEAR, cases, encounters)

hist_final_w <- bind_rows(oerwd_hist, nis_hist_final) %>%
  mutate(
    RACE = as.factor(RACE),
    censusdivision = as.factor(censusdivision),
    age_group = factor(age_group, levels = c("18 to 34", "35 to 64", "65 and Over")),
    YEAR = factor(YEAR),
    FEMALE = factor(FEMALE, levels = c(0, 1)),
    id = as.factor(id)
  ) %>%
  drop_na(censusdivision, RACE, FEMALE, age_group, YEAR, cases, encounters)


