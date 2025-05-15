###############################################################################
# Purpose: Clean HCUP NIS and OERWD Data - Aspergillosis and Histoplasmosis
#
# Written by: Juliana GE Bartels
# Last Updated: 5/15/2025
###############################################################################
# Load libraries
library(tidyverse)
library(data.table)
library(tictoc)
library(survey)
library(boot)
###############################################################################
# Clean NIS Aspergillosis and Histoplasmosis Data

################### Load NIS Hospital Files ####################################
# Load 2014 Hospital file
NIS_2014_Hospital <- as.data.table(read.table("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/NIS_Hospitalfiles/NIS_2014_Hospital.ASC", 
                                              quote="\"", comment.char="")) %>%
  rename("DISCWT" = "V1", "HOSP_LOC_TEACH" = "V2", "HOSP_DIVISION" = "V3", 
         "HOSP_NIS" = "V4", "HOSP_REGION" = "V5", "H_CONTROLNIS_STRATUM" = "V6", 
         "N_DISC_U" = "V7", "N_HOSP_U" = "V8", "S_DISC_U" = "V9", "S_HOSP_U" = "V10", 
         "TOTAL_DISCYEAR" = "V11" )

# Make the combined columns (H_CONTROLNIS_STRATUM, TOTAL_DISCYEAR) into a character vector
NIS_2014_Hospital$`H_CONTROLNIS_STRATUM` <- as.character(NIS_2014_Hospital$`H_CONTROLNIS_STRATUM`)
NIS_2014_Hospital$`TOTAL_DISCYEAR` <- as.character(NIS_2014_Hospital$`TOTAL_DISCYEAR`)

# Separate the 'H_CONTROLNIS_STRATUM' column by its first character
NIS_2014_Hospital <- separate(NIS_2014_Hospital, H_CONTROLNIS_STRATUM, into = c("H_CONTROL", "NIS_STRATUM"), sep = 1)
# Separate 'TOTAL_DISCYEAR' column into TOTAL_DISC and YEAR
NIS_2014_Hospital$TOTAL_DISC <- sub(".{4}$", "", NIS_2014_Hospital$TOTAL_DISCYEAR)
NIS_2014_Hospital$YEAR <- substr(NIS_2014_Hospital$TOTAL_DISCYEAR, nchar(NIS_2014_Hospital$TOTAL_DISCYEAR) - 3, nchar(NIS_2014_Hospital$TOTAL_DISCYEAR))
NIS_2014_Hospital <- NIS_2014_Hospital %>%
  dplyr::select(-TOTAL_DISCYEAR)

# Convert back to integers
NIS_2014_Hospital$`H_CONTROL` <- as.integer(NIS_2014_Hospital$`H_CONTROL`)
NIS_2014_Hospital$`NIS_STRATUM` <- as.integer(NIS_2014_Hospital$`NIS_STRATUM`)
NIS_2014_Hospital$`TOTAL_DISC` <- as.integer(NIS_2014_Hospital$`TOTAL_DISC`)
NIS_2014_Hospital$`YEAR` <- as.integer(NIS_2014_Hospital$`YEAR`)

# Load 2015 Hospital file
NIS_2015_Hospital <- as.data.table(read.table("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/NIS_Hospitalfiles/NIS_2015_Hospital.ASC", 
                                              quote="\"", comment.char="")) %>%
  rename("DISCWT" = "V1", "HOSP_LOC_TEACH" = "V2", "HOSP_DIVISION" = "V3", 
         "HOSP_NIS" = "V4", "HOSP_REGION" = "V5", "H_CONTROLNIS_STRATUM" = "V6",
         "N_DISC_U" = "V7", "N_HOSP_U" = "V8", "S_DISC_U" = "V9", "S_HOSP_U" = "V10",
         "TOTAL_DISCYEAR" = "V11" )

# Make the combined columns (H_CONTROLNIS_STRATUM, TOTAL_DISCYEAR) into a character vector
NIS_2015_Hospital$`H_CONTROLNIS_STRATUM` <- as.character(NIS_2015_Hospital$`H_CONTROLNIS_STRATUM`)
NIS_2015_Hospital$`TOTAL_DISCYEAR` <- as.character(NIS_2015_Hospital$`TOTAL_DISCYEAR`)

# Separate the 'H_CONTROLNIS_STRATUM' column by its first character
NIS_2015_Hospital <- separate(NIS_2015_Hospital, H_CONTROLNIS_STRATUM, into = c("H_CONTROL", "NIS_STRATUM"), sep = 1)
# Separate 'TOTAL_DISCYEAR' column into TOTAL_DISC and YEAR
NIS_2015_Hospital$TOTAL_DISC <- sub(".{4}$", "", NIS_2015_Hospital$TOTAL_DISCYEAR)
NIS_2015_Hospital$YEAR <- substr(NIS_2015_Hospital$TOTAL_DISCYEAR, nchar(NIS_2015_Hospital$TOTAL_DISCYEAR) - 3, nchar(NIS_2015_Hospital$TOTAL_DISCYEAR))
NIS_2015_Hospital <- NIS_2015_Hospital %>%
  dplyr::select(-TOTAL_DISCYEAR)

# Convert back to integers
NIS_2015_Hospital$`H_CONTROL` <- as.integer(NIS_2015_Hospital$`H_CONTROL`)
NIS_2015_Hospital$`NIS_STRATUM` <- as.integer(NIS_2015_Hospital$`NIS_STRATUM`)
NIS_2015_Hospital$`TOTAL_DISC` <- as.integer(NIS_2015_Hospital$`TOTAL_DISC`)
NIS_2015_Hospital$`YEAR` <- as.integer(NIS_2015_Hospital$`YEAR`)

# Load 2016 Hospital file
NIS_2016_Hospital <- as.data.table(read.table("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/NIS_Hospitalfiles/NIS_2016_Hospital.ASC", 
                                              quote="\"", comment.char="")) %>%
  rename("DISCWT" = "V1", "HOSP_LOC_TEACH" = "V2", "HOSP_DIVISION" = "V3", 
         "HOSP_NIS" = "V4", "HOSP_REGION" = "V5", "H_CONTROLNIS_STRATUM" = "V6",
         "N_DISC_U" = "V7", "N_HOSP_U" = "V8", "S_DISC_U" = "V9", "S_HOSP_U" = "V10", 
         "TOTAL_DISCYEAR" = "V11" )

# Make combined columns (H_CONTROLNIS_STRATUM, TOTAL_DISCYEAR) into a character vector
NIS_2016_Hospital$`H_CONTROLNIS_STRATUM` <- as.character(NIS_2016_Hospital$`H_CONTROLNIS_STRATUM`)
NIS_2016_Hospital$`TOTAL_DISCYEAR` <- as.character(NIS_2016_Hospital$`TOTAL_DISCYEAR`)

# Separate the 'H_CONTROLNIS_STRATUM' column by its first character
NIS_2016_Hospital <- separate(NIS_2016_Hospital, H_CONTROLNIS_STRATUM, into = c("H_CONTROL", "NIS_STRATUM"), sep = 1)
# Separate 'TOTAL_DISCYEAR' column into TOTAL_DISC and YEAR
NIS_2016_Hospital$TOTAL_DISC <- sub(".{4}$", "", NIS_2016_Hospital$TOTAL_DISCYEAR)
NIS_2016_Hospital$YEAR <- substr(NIS_2016_Hospital$TOTAL_DISCYEAR, nchar(NIS_2016_Hospital$TOTAL_DISCYEAR) - 3, nchar(NIS_2016_Hospital$TOTAL_DISCYEAR))
NIS_2016_Hospital <- NIS_2016_Hospital %>%
  dplyr::select(-TOTAL_DISCYEAR)

# Convert back to integers
NIS_2016_Hospital$`H_CONTROL` <- as.integer(NIS_2016_Hospital$`H_CONTROL`)
NIS_2016_Hospital$`NIS_STRATUM` <- as.integer(NIS_2016_Hospital$`NIS_STRATUM`)
NIS_2016_Hospital$`TOTAL_DISC` <- as.integer(NIS_2016_Hospital$`TOTAL_DISC`)
NIS_2016_Hospital$`YEAR` <- as.integer(NIS_2016_Hospital$`YEAR`)

# Load 2017 Hospital file
NIS_2017_Hospital <- as.data.table(read.table("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/NIS_Hospitalfiles/NIS_2017_Hospital.ASC", 
                                              quote="\"", comment.char="")) %>%
  rename("DISCWT" = "V1", "HOSP_LOC_TEACH" = "V2", "HOSP_DIVISION" = "V3", 
         "HOSP_NIS" = "V4", "HOSP_REGION" = "V5", "H_CONTROLNIS_STRATUM" = "V6", 
         "N_DISC_U" = "V7", "N_HOSP_U" = "V8", "S_DISC_U" = "V9", "S_HOSP_U" = "V10", 
         "TOTAL_DISCYEAR" = "V11" )

# Make the combined columns (H_CONTROLNIS_STRATUM, TOTAL_DISCYEAR) into a character vector
NIS_2017_Hospital$`H_CONTROLNIS_STRATUM` <- as.character(NIS_2017_Hospital$`H_CONTROLNIS_STRATUM`)
NIS_2017_Hospital$`TOTAL_DISCYEAR` <- as.character(NIS_2017_Hospital$`TOTAL_DISCYEAR`)

# Separate the 'H_CONTROLNIS_STRATUM' column by its first character
NIS_2017_Hospital <- separate(NIS_2017_Hospital, H_CONTROLNIS_STRATUM, into = c("H_CONTROL", "NIS_STRATUM"), sep = 1)
# Separate 'TOTAL_DISCYEAR' column into TOTAL_DISC and YEAR
NIS_2017_Hospital$TOTAL_DISC <- sub(".{4}$", "", NIS_2017_Hospital$TOTAL_DISCYEAR)
NIS_2017_Hospital$YEAR <- substr(NIS_2017_Hospital$TOTAL_DISCYEAR, nchar(NIS_2017_Hospital$TOTAL_DISCYEAR) - 3, nchar(NIS_2017_Hospital$TOTAL_DISCYEAR))
NIS_2017_Hospital <- NIS_2017_Hospital %>%
  dplyr::select(-TOTAL_DISCYEAR)

# Convert back to integers
NIS_2017_Hospital$`H_CONTROL` <- as.integer(NIS_2017_Hospital$`H_CONTROL`)
NIS_2017_Hospital$`NIS_STRATUM` <- as.integer(NIS_2017_Hospital$`NIS_STRATUM`)
NIS_2017_Hospital$`TOTAL_DISC` <- as.integer(NIS_2017_Hospital$`TOTAL_DISC`)
NIS_2017_Hospital$`YEAR` <- as.integer(NIS_2017_Hospital$`YEAR`)

#Load 2018 Hospital file
NIS_2018_Hospital <- as.data.table(read.table("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/NIS_Hospitalfiles/NIS_2018_Hospital.ASC", 
                                              quote="\"", comment.char="")) %>%
  rename("DISCWT" = "V1", "HOSP_LOC_TEACH" = "V2", "HOSP_DIVISION" = "V3", 
         "HOSP_NIS" = "V4", "HOSP_REGION" = "V5", "H_CONTROLNIS_STRATUM" = "V6", 
         "N_DISC_U" = "V7", "N_HOSP_U" = "V8", "S_DISC_U" = "V9", "S_HOSP_U" = "V10", 
         "TOTAL_DISCYEAR" = "V11" )

# Make combined columns (H_CONTROLNIS_STRATUM, TOTAL_DISCYEAR) into a character vector
NIS_2018_Hospital$`H_CONTROLNIS_STRATUM` <- as.character(NIS_2018_Hospital$`H_CONTROLNIS_STRATUM`)
NIS_2018_Hospital$`TOTAL_DISCYEAR` <- as.character(NIS_2018_Hospital$`TOTAL_DISCYEAR`)

# Separate the 'H_CONTROLNIS_STRATUM' column by its first character
NIS_2018_Hospital <- separate(NIS_2018_Hospital, H_CONTROLNIS_STRATUM, into = c("H_CONTROL", "NIS_STRATUM"), sep = 1)
# Separate 'TOTAL_DISCYEAR' column into TOTAL_DISC and YEAR
NIS_2018_Hospital$TOTAL_DISC <- sub(".{4}$", "", NIS_2018_Hospital$TOTAL_DISCYEAR)
NIS_2018_Hospital$YEAR <- substr(NIS_2018_Hospital$TOTAL_DISCYEAR, nchar(NIS_2018_Hospital$TOTAL_DISCYEAR) - 3, nchar(NIS_2018_Hospital$TOTAL_DISCYEAR))
NIS_2018_Hospital <- NIS_2018_Hospital %>%
  dplyr::select(-TOTAL_DISCYEAR)

# Convert back to integers
NIS_2018_Hospital$`H_CONTROL` <- as.integer(NIS_2018_Hospital$`H_CONTROL`)
NIS_2018_Hospital$`NIS_STRATUM` <- as.integer(NIS_2018_Hospital$`NIS_STRATUM`)
NIS_2018_Hospital$`TOTAL_DISC` <- as.integer(NIS_2018_Hospital$`TOTAL_DISC`)
NIS_2018_Hospital$`YEAR` <- as.integer(NIS_2018_Hospital$`YEAR`)

#Load 2019 Hospital file
NIS_2019_Hospital <- as.data.table(read.table("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/NIS_Hospitalfiles/NIS_2019_Hospital.ASC", 
                                              quote="\"", comment.char="")) %>%
  rename("DISCWT" = "V1", "HOSP_LOC_TEACH" = "V2", "HOSP_DIVISION" = "V3", 
         "HOSP_NIS" = "V4", "HOSP_REGION" = "V5", "H_CONTROLNIS_STRATUM" = "V6", 
         "N_DISC_U" = "V7", "N_HOSP_U" = "V8", "S_DISC_U" = "V9", "S_HOSP_U" = "V10",
         "TOTAL_DISCYEAR" = "V11" )

# Make the combined columns (H_CONTROLNIS_STRATUM, TOTAL_DISCYEAR) into a character vector
NIS_2019_Hospital$`H_CONTROLNIS_STRATUM` <- as.character(NIS_2019_Hospital$`H_CONTROLNIS_STRATUM`)
NIS_2019_Hospital$`TOTAL_DISCYEAR` <- as.character(NIS_2019_Hospital$`TOTAL_DISCYEAR`)

# Separate the 'H_CONTROLNIS_STRATUM' column by its first character
NIS_2019_Hospital <- separate(NIS_2019_Hospital, H_CONTROLNIS_STRATUM, into = c("H_CONTROL", "NIS_STRATUM"), sep = 1)
# Separate 'TOTAL_DISCYEAR' column into TOTAL_DISC and YEAR
NIS_2019_Hospital$TOTAL_DISC <- sub(".{4}$", "", NIS_2019_Hospital$TOTAL_DISCYEAR)
NIS_2019_Hospital$YEAR <- substr(NIS_2019_Hospital$TOTAL_DISCYEAR, nchar(NIS_2019_Hospital$TOTAL_DISCYEAR) - 3, nchar(NIS_2019_Hospital$TOTAL_DISCYEAR))
NIS_2019_Hospital <- NIS_2019_Hospital %>%
  dplyr::select(-TOTAL_DISCYEAR)

# Convert back to integers
NIS_2019_Hospital$`H_CONTROL` <- as.integer(NIS_2019_Hospital$`H_CONTROL`)
NIS_2019_Hospital$`NIS_STRATUM` <- as.integer(NIS_2019_Hospital$`NIS_STRATUM`)
NIS_2019_Hospital$`TOTAL_DISC` <- as.integer(NIS_2019_Hospital$`TOTAL_DISC`)
NIS_2019_Hospital$`YEAR` <- as.integer(NIS_2019_Hospital$`YEAR`)

#Load 2020 Hospital file
NIS_2020_Hospital <- as.data.table(read.table("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/NIS_Hospitalfiles/NIS_2020_Hospital.ASC", 
                                              quote="\"", comment.char="")) %>%
  rename("DISCWT" = "V1", "HOSP_LOC_TEACH" = "V2", "HOSP_DIVISION" = "V3", "HOSP_NIS" = "V4", "HOSP_REGION" = "V5", "H_CONTROLNIS_STRATUM" = "V6", "N_DISC_U" = "V7", "N_HOSP_U" = "V8", "S_DISC_U" = "V9", "S_HOSP_U" = "V10", "TOTAL_DISCYEAR" = "V11" )

# Make the combined columns (H_CONTROLNIS_STRATUM, TOTAL_DISCYEAR) into a character vector
NIS_2020_Hospital$`H_CONTROLNIS_STRATUM` <- as.character(NIS_2020_Hospital$`H_CONTROLNIS_STRATUM`)
NIS_2020_Hospital$`TOTAL_DISCYEAR` <- as.character(NIS_2020_Hospital$`TOTAL_DISCYEAR`)

# Separate the 'H_CONTROLNIS_STRATUM' column by its first character
NIS_2020_Hospital <- separate(NIS_2020_Hospital, H_CONTROLNIS_STRATUM, into = c("H_CONTROL", "NIS_STRATUM"), sep = 1)
# Separate 'TOTAL_DISCYEAR' column into TOTAL_DISC and YEAR
NIS_2020_Hospital$TOTAL_DISC <- sub(".{4}$", "", NIS_2020_Hospital$TOTAL_DISCYEAR)
NIS_2020_Hospital$YEAR <- substr(NIS_2020_Hospital$TOTAL_DISCYEAR, nchar(NIS_2020_Hospital$TOTAL_DISCYEAR) - 3, nchar(NIS_2020_Hospital$TOTAL_DISCYEAR))
NIS_2020_Hospital <- NIS_2020_Hospital %>%
  dplyr::select(-TOTAL_DISCYEAR)

# Convert back to integers
NIS_2020_Hospital$`H_CONTROL` <- as.integer(NIS_2020_Hospital$`H_CONTROL`)
NIS_2020_Hospital$`NIS_STRATUM` <- as.integer(NIS_2020_Hospital$`NIS_STRATUM`)
NIS_2020_Hospital$`TOTAL_DISC` <- as.integer(NIS_2020_Hospital$`TOTAL_DISC`)
NIS_2020_Hospital$`YEAR` <- as.integer(NIS_2020_Hospital$`YEAR`)

# Append these datasets together
NIS_2014_2020_Hospital <- rbind(NIS_2014_Hospital, NIS_2015_Hospital, NIS_2016_Hospital, NIS_2017_Hospital, NIS_2018_Hospital, NIS_2019_Hospital, NIS_2020_Hospital)

# Select Year, NIS_STRATUM, HOSP_NIS, and DISC_WT
NIS_2014_2020_Hospital <- NIS_2014_2020_Hospital %>%
  dplyr::select(NIS_STRATUM, HOSP_NIS, DISCWT, YEAR)

################### Load HCUP NIS data for Aspergillosis #######################
# Load HCUP NIS data for Aspergillosis
asp_2014_2020_combined_clean <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/asp_2014_2020_combined_clean.csv")

# Create age categories
asp_2014_2020_combined_clean <- asp_2014_2020_combined_clean %>%
  mutate(ASP = 1,
         age_group = case_when(
           AGE < 18 ~ "Under 18",
           AGE >= 18 & AGE < 35 ~ "18 to 34",
           AGE >= 35 & AGE < 65 ~ "35 to 64",
           AGE >= 65 ~ "65 and Over"
         )
  )
# Remove encounters for patients under 18
asp_2014_2020_combined_clean <- asp_2014_2020_combined_clean[age_group != "Under 18"]

# Create a summary table of aspergillosis encounters
asp_2014_2020_summary <- asp_2014_2020_combined_clean[, .(nEncounters = sum(ASP)), 
                                                      .(YEAR, HOSP_DIVISION, 
                                                        FEMALE, RACE, 
                                                        age_group)]

################ Weighted total encounters per year for Aspergillosis ##########

# Using survey package in R to apply weights and HOSP_NIS clusters and 
# NIS_STRATUM strata

# Append HOSPITAL data to aspergillosis data
all_asp_2014_2020 <- merge(asp_2014_2020_combined_clean, NIS_2014_2020_Hospital, 
                           by = c("HOSP_NIS", "NIS_STRATUM", "DISCWT", "YEAR"), 
                           all = TRUE)

# Define survey design
asp_design <- svydesign(ids=~HOSP_NIS, strata=~NIS_STRATUM, weights=~DISCWT, 
                        data=all_asp_2014_2020, nest = TRUE)

# Calculate the sum and standard error by year 
totenc_weighted_asp <- as.data.frame(svyby(~ASP, by = ~YEAR, design = asp_design, 
                                           FUN = svytotal, na.rm = TRUE, 
                                           vartype = c('se', 'ci')))

# Export totenc_weighted_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_asp.csv"
write.csv(totenc_weighted_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error for ALL YEARS
totenc_weighted_total_asp <- as.data.frame(svytotal(~ASP, design = asp_design, 
                                                    FUN = svytotal, na.rm = TRUE))

# Export totenc_weighted_total_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_total_asp.csv"
write.csv(totenc_weighted_total_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by division 
totenc_weighted_div_asp <- as.data.frame(svyby(~ASP, by = ~HOSP_DIVISION, 
                                               design = asp_design, FUN = svytotal, 
                                               na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_div_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_div_asp.csv"
write.csv(totenc_weighted_div_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by division and year
totenc_weighted_divyear_asp <- as.data.frame(svyby(~ASP, by = ~HOSP_DIVISION + YEAR, 
                                                   design = asp_design, FUN = svytotal,
                                                   na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_divyear_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_divyear_asp.csv"
write.csv(totenc_weighted_divyear_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by female 
totenc_weighted_female_asp <- as.data.frame(svyby(~ASP, by = ~FEMALE, 
                                                  design = asp_design, 
                                                  FUN = svytotal, na.rm = TRUE, 
                                                  vartype = c('se', 'ci')))

# Export totenc_weighted_female_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_female_asp.csv"
write.csv(totenc_weighted_female_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by race 
totenc_weighted_race_asp <- as.data.frame(svyby(~ASP, by = ~RACE, design = asp_design, 
                                                FUN = svytotal, na.rm = TRUE, 
                                                vartype = c('se', 'ci')))

# Export totenc_weighted_race_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_race_asp.csv"
write.csv(totenc_weighted_race_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by race 
totenc_weighted_age_asp <- as.data.frame(svyby(~ASP, by = ~age_group, 
                                               design = asp_design, FUN = svytotal, 
                                               na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_age_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_age_asp.csv"
write.csv(totenc_weighted_age_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by all variables
totenc_weighted_racegender_asp <- as.data.frame(svyby(~ASP, by = ~RACE + FEMALE, 
                                                      design = asp_design, 
                                                      FUN = svytotal, na.rm = TRUE, 
                                                      vartype = c('se', 'ci')))

# Export totenc_weighted_racegender_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_racegender_asp.csv"
write.csv(totenc_weighted_racegender_asp, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by all variables
totenc_weighted_racegenderage_asp <- as.data.frame(svyby(~ASP, by = ~RACE + FEMALE + age_group, design = asp_design, FUN = svytotal, na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_racegenderage_asp to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_racegenderage_asp.csv"
write.csv(totenc_weighted_racegenderage_asp, file = file_path, row.names = FALSE)

################### Load HCUP NIS data for Histoplasmosis #######################
# Load HCUP NIS data for Histoplasmosis
hist_2014_2020_combined_clean <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/hist_2014_2020_combined_clean.csv")

hist_2014_2020_combined_clean <- hist_2014_2020_combined_clean %>%
  mutate(HIST = 1,
         age_group = case_when(
           AGE < 18 ~ "Under 18",
           AGE >= 18 & AGE < 35 ~ "18 to 34",
           AGE >= 35 & AGE < 65 ~ "35 to 64",
           AGE >= 65 ~ "65 and Over"
         )
  )
hist_2014_2020_combined_clean <- hist_2014_2020_combined_clean[age_group != "Under 18"]

# Create a summary table of aspergillosis encounters
hist_2014_2020_summary <- hist_2014_2020_combined_clean[, .(nEncounters = sum(HIST)), 
                                                        .(YEAR, HOSP_DIVISION, 
                                                          FEMALE, RACE, 
                                                          age_group)]

################ Weighted total encounters per year for Histoplasmosis ##########
# Using survey package in R to apply weights and HOSP_NIS clusters and NIS_STRATUM strata

#Append HOSPITAL data to aspergillosis data
all_hist_2014_2020 <- merge(hist_2014_2020_combined_clean, NIS_2014_2020_Hospital, 
                            by = c("HOSP_NIS", "NIS_STRATUM", "DISCWT", "YEAR"), 
                            all = TRUE)

# Define survey design
hist_design <- svydesign(ids=~HOSP_NIS, strata=~NIS_STRATUM, weights=~DISCWT, 
                         data=all_hist_2014_2020, nest = TRUE)

# Calculate the sum and standard error by year 
totenc_weighted_hist <- as.data.frame(svyby(~HIST, by = ~YEAR, design = hist_design, 
                                            FUN = svytotal, na.rm = TRUE, 
                                            vartype = c('se', 'ci')))

# Calculate the sum and standard error for ALL YEARS
totenc_weighted_total_hist <- as.data.frame(svytotal(~HIST, design = hist_design, 
                                                     FUN = svytotal, na.rm = TRUE))

# Export totenc_weighted_total_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_total_hist.csv"
write.csv(totenc_weighted_total_hist, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by female 
totenc_weighted_div_female <- as.data.frame(svyby(~HIST, by = ~FEMALE, 
                                                  design = hist_design, FUN = svytotal, 
                                                  na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_div_female to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_div_female.csv"

write.csv(totenc_weighted_div_female, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by division 
totenc_weighted_div_hist <- as.data.frame(svyby(~HIST, by = ~HOSP_DIVISION, 
                                                design = hist_design, FUN = svytotal, 
                                                na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_div_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_div_hist.csv"

write.csv(totenc_weighted_div_hist, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by division and year
totenc_weighted_divyear_hist <- as.data.frame(svyby(~HIST, by = ~HOSP_DIVISION + YEAR, 
                                                    design = hist_design, FUN = svytotal, 
                                                    na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_hist.csv"

write.csv(totenc_weighted_hist, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by female 
totenc_weighted_female_hist <- as.data.frame(svyby(~HIST, by = ~FEMALE, 
                                                   design = hist_design, FUN = svytotal,
                                                   na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_female_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_female_hist.csv"
write.csv(totenc_weighted_female_hist, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by race 
totenc_weighted_race_hist <- as.data.frame(svyby(~HIST, by = ~RACE, 
                                                 design = hist_design, FUN = svytotal, 
                                                 na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_race_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_race_hist.csv"
write.csv(totenc_weighted_race_hist, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by age 
totenc_weighted_age_hist <- as.data.frame(svyby(~HIST, by = ~age_group, 
                                                design = hist_design, FUN = svytotal, 
                                                na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_age_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_age_hist.csv"
write.csv(totenc_weighted_age_hist, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by all variables
totenc_weighted_racegender_hist <- as.data.frame(svyby(~HIST, by = ~RACE + FEMALE, 
                                                       design = hist_design, 
                                                       FUN = svytotal, na.rm = TRUE, 
                                                       vartype = c('se', 'ci')))

# Export totenc_weighted_racegender_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_racegender_hist.csv"

write.csv(totenc_weighted_racegender_hist, file = file_path, row.names = FALSE)

# Calculate the sum and standard error by all variables
totenc_weighted_racegenderage_hist <- as.data.frame(svyby(~HIST, by = ~RACE + FEMALE + age_group, 
                                                          design = hist_design, FUN = svytotal, 
                                                          na.rm = TRUE, vartype = c('se', 'ci')))

# Export totenc_weighted_racegenderage_hist to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_racegenderage_hist.csv"
write.csv(totenc_weighted_racegenderage_hist, file = file_path, row.names = FALSE)


################### Female Denominators ########################################

hcup_denominators_female <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP Paper/Repos/data/hcup_denominators_female.csv") %>%
  mutate(DISCWT = 5)

# create survey design object to create weighted totals
female_denom_design <- svydesign(id =~HOSP_NIS, strata=~NIS_STRATUM, 
                                 weights=~DISCWT, data=hcup_denominators_female, 
                                 nest = TRUE)

# Calculate the total encounters (sum) and standard error by sex 
female_denom_sex <- as.data.frame(svyby(~ENCOUNTERS, by = ~FEMALE, 
                                        design = female_denom_design, FUN = svytotal, 
                                        na.rm = TRUE, vartype = c('se', 'ci'))) %>%
  rename(SE = se)

# Export female_denom_sex to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/female_denom_sex.csv"

write.csv(female_denom_sex, file = file_path, row.names = FALSE)

################### Age Denominators ########################################

hcup_denominators_age <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP Paper/Repos/data/hcup_denominators_age.csv") %>%
  filter(ENCOUNTERS != 0) %>%
  mutate(DISCWT = 5)

# create survey design object to create weighted totals
age_denom_design <- svydesign(id =~HOSP_NIS, strata=~NIS_STRATUM, weights=~DISCWT, 
                              data=hcup_denominators_age, nest = TRUE)

# Calculate the total encounters (sum) and standard error by age group 
age_denom_age_group <- as.data.frame(svyby(~ENCOUNTERS, by = ~AGE_GROUP, 
                                           design = age_denom_design, FUN = svytotal, 
                                           na.rm = TRUE, vartype = c('se', 'ci'))) %>%
  rename(SE = se)

# Export age_denom_age_group to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/age_denom_age_group.csv"

write.csv(age_denom_age_group, file = file_path, row.names = FALSE)


################### Race Denominators ########################################

hcup_denominators_race <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP Paper/Repos/data/hcup_denominators_race.csv") %>%
  mutate(DISCWT = 5)

# create survey design object to create weighted totals
race_denom_design <- svydesign(id =~HOSP_NIS, strata=~NIS_STRATUM, weights=~DISCWT, 
                               data=hcup_denominators_race, nest = TRUE)

# Calculate the total encounters (sum) and standard error by race 
race_denom_race <- as.data.frame(svyby(~ENCOUNTERS, by = ~RACE, 
                                       design = race_denom_design, FUN = svytotal, 
                                       na.rm = TRUE, vartype = c('se', 'ci'))) %>%
  rename(SE = se)

# Export race_denom_race to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/race_denom_race.csv"

write.csv(race_denom_race, file = file_path, row.names = FALSE)

################### Hospital Division Denominators ########################################

hcup_denominators_hospdiv <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP Paper/Repos/data/hcup_denominators_hospdiv.csv") %>%
  mutate(DISCWT = 5)

# create survey design object to create weighted totals
hospdiv_denom_design <- svydesign(id =~HOSP_NIS, strata=~NIS_STRATUM, weights=~DISCWT, 
                                  data=hcup_denominators_hospdiv, nest = TRUE)

# Calculate the total encounters (sum) and standard error by race 
hospdiv_denom_cd <- as.data.frame(svyby(~ENCOUNTERS, by = ~HOSP_DIVISION, 
                                        design = hospdiv_denom_design, FUN = svytotal, 
                                        na.rm = TRUE, vartype = c('se', 'ci'))) %>%
  rename(SE = se)

# Export hospdiv_denom_cd to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/hospdiv_denom_cd.csv"

write.csv(hospdiv_denom_cd, file = file_path, row.names = FALSE)

# Calculate the total encounters (sum) and standard error by year 
hospdiv_denom_year <- as.data.frame(svyby(~ENCOUNTERS, by = ~YEAR, 
                                          design = hospdiv_denom_design, FUN = svytotal, 
                                          na.rm = TRUE, vartype = c('se', 'ci')))%>%
  rename(SE = se)

# Export hospdiv_denom_year to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/hospdiv_denom_year.csv"

write.csv(hospdiv_denom_year, file = file_path, row.names = FALSE)

# Calculate the total encounters (sum) and standard error, years combined
hospdiv_denom_total <- as.data.frame(svytotal(~ENCOUNTERS, design = hospdiv_denom_design, 
                                              FUN = svytotal, na.rm = TRUE)) %>%
  rename(SE = ENCOUNTERS, ENCOUNTERS = total)

# Export hospdiv_denom_total to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/hospdiv_denom_total.csv"

write.csv(hospdiv_denom_total, file = file_path, row.names = FALSE)

################################################################################
################################################################################
# Clean OERWD Histoplasmosis and Aspergillosis Data

################### Load Data by Year ##########################################
# Load denominator_df_annual_2023.csv - by year, age strata, and state
denom_month <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/rwd_denominators_hcup.csv')
denom_month <- denom_month %>%
  rename(YEAR = date2_year, nRWDPatient_m = nPatient, nRWDEncounter_m = nEncounter)
denom_month <- denom_month[!is.na(prefstate)] 
denom_month <- denom_month %>%
  filter(YEAR %in% 2014:2020)
denom_month <- denom_month %>%
  dplyr::select(-V1)

# Make into annual denominators
denom_year <- denom_month[, .(nRWDEncounter_y = sum(nRWDEncounter_m)), 
                          .(YEAR, prefstate, prefurban, 
                            prefethnicity, prefrace, prefgender, 
                            age_group)]
denom_year <- denom_year[age_group != "0 to 17"]

# Load asp_cohort_summary and histo_cohort_summary - cases by month, age strata, and state
asp_cohort_summary <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/asp_summary_encounters_inpt.csv')
asp_cohort_summary <- asp_cohort_summary[!is.na(prefstate)]
asp_cohort_summary <- asp_cohort_summary[age_group != "Under 18"]
asp_cohort_year <- asp_cohort_summary[, .(nEncounters = sum(nEncounters)), 
                                      .(YEAR, prefstate, prefurban, 
                                        prefethnicity, prefrace, prefgender, 
                                        age_group)]
asp_cohort_year_full <- full_join(asp_cohort_year,
                                  denom_year, 
                                  by = c('YEAR', 'prefstate', 'prefurban', 
                                         'prefgender', 'prefrace', 'prefethnicity', 
                                         'age_group'))
asp_cohort_year_full <- asp_cohort_year_full %>%
  mutate(Disease = "Aspergillosis")

histo_cohort_summary <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/histo_summary_encounters_inpt.csv')
histo_cohort_summary <- histo_cohort_summary[!is.na(prefstate)]
histo_cohort_summary <- histo_cohort_summary[age_group != "Under 18"]
histo_cohort_year <- histo_cohort_summary[, .(nEncounters = sum(nEncounters)), 
                                          .(YEAR, prefstate, prefurban, 
                                            prefethnicity, prefrace, prefgender, 
                                            age_group)]
histo_cohort_year_full <- full_join(histo_cohort_year,
                                    denom_year, 
                                    by = c('YEAR', 'prefstate', 'prefurban', 
                                           'prefgender', 'prefrace', 'prefethnicity', 
                                           'age_group'))
histo_cohort_year_full <- histo_cohort_year_full %>%
  mutate(Disease = "Histoplasmosis")

ocrwd_cohort_year_full <- rbind(histo_cohort_year_full, asp_cohort_year_full)

################### Clean Data #################################################

# Replace NA patient numbers with 0
ocrwd_cohort_year_full[, `:=`(
  nEncounters = fifelse(is.na(nEncounters), 0, nEncounters),
  nRWDEncounter_y = fifelse(is.na(nRWDEncounter_y), 0, nRWDEncounter_y)
)]

# load census division dataframe
censusdivisions <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/censusdivisions.csv")
ocrwd_cohort_year_full <- full_join(ocrwd_cohort_year_full, censusdivisions, by = "prefstate")

# summarize by census division
ocrwd_cohort_year_full <- ocrwd_cohort_year_full[, .(nEncounters = sum(nEncounters), nRWDEncounter_y = sum(nRWDEncounter_y)), 
                                                 .(Disease, YEAR, censusdivision, 
                                                   prefethnicity, prefrace, prefgender, 
                                                   age_group)]

# create RACE variable to match HCUP
ocrwd_cohort_year_full[, RACE := fcase(
  prefrace == "White" & prefethnicity %in% c("Non-Hispanic", "Unknown"), 1,
  prefrace == "Black or African American" & prefethnicity %in% c("Non-Hispanic", "Unknown"), 2,
  prefethnicity == "Hispanic or Latino", 3,
  prefrace %in% c("Asian","Native Hawaiian or Other Pacific Islander") & prefethnicity %in% c("Non-Hispanic", "Unknown"), 4,
  prefrace == "American Indian or Alaska Native" & prefethnicity %in% c("Non-Hispanic", "Unknown"), 5,
  prefrace == "Other" & prefethnicity %in% c("Unknown", "Non-Hispanic"), 6,
  prefrace == "Unknown" & prefethnicity == "Non-Hispanic", 7,
  prefrace == "Unknown" & prefethnicity == "Hispanic or Latino", 3,
  prefrace == "Unknown" & prefethnicity == "Unknown", 7
)]

# Create FEMALE variable to match HCUP
ocrwd_cohort_year_full[, FEMALE := fcase(
  prefgender == "Female", 1,
  prefgender == "Male", 0,
  prefgender == "Other or Unknown", 2 
)]

# Group by RACE, FEMALE, YEAR, census division, age group
ocrwd_cohort_year_full <- ocrwd_cohort_year_full[, .(nEncounters = sum(nEncounters), nRWDEncounter_y = sum(nRWDEncounter_y)), 
                                                 .(Disease, YEAR, censusdivision, 
                                                   RACE, FEMALE, 
                                                   age_group)]
ocrwd_cohort_year_full <- ocrwd_cohort_year_full[!is.na(censusdivision)]

# Check number of encounters
ocrwd_cohort_year_full %>%
  group_by(Disease) %>%
  summarize(nEncounters = sum(nEncounters),
            nRWDEncounter_y = sum(nRWDEncounter_y))

# Export ocrwd_cohort_year_full to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full.csv"

write.csv(ocrwd_cohort_year_full, file = file_path, row.names = FALSE)

############################# Weighting ########################################

# Weighting to the NIS All Inpatient Discharges by Census Division
hcup_denominators_hospdiv_weights <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP Paper/Repos/data/hcup_denominators_hospdiv.csv") %>%
  mutate(DISCWT = 5) %>%
  rename(censusdivision = HOSP_DIVISION) %>%
  group_by(censusdivision, YEAR) %>%
  summarize(ENCOUNTERS = sum(ENCOUNTERS)*5)

# Summarize OCRWD Encounters by YEAR and Census Division
ocrwd_enctotal_2014_2020 <- ocrwd_cohort_year_full[, .(nEncounters = sum(nEncounters), nRWDEncounter_y = sum(nRWDEncounter_y)), 
                                                   .(Disease, YEAR, censusdivision)]

# Create table of weights by taking the ratio of NIS stays over OCRWD total encounters
weighttable <- full_join(hcup_denominators_hospdiv_weights, ocrwd_enctotal_2014_2020, 
                         by = c("censusdivision", "YEAR") ) %>%
  mutate(weight = ENCOUNTERS/nRWDEncounter_y)  %>%
  dplyr::select(-nEncounters, -nRWDEncounter_y, -ENCOUNTERS)

# Check weights
summary(weighttable$weight)

# Export weighttable to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/weighttable.csv"
write.csv(weighttable, file = file_path, row.names = FALSE)

# Add weights to full table and calculate 
ocrwd_cohort_year_full_w <- right_join(ocrwd_cohort_year_full,
                                       weighttable,
                                       by = c("Disease", "censusdivision", "YEAR"))

ocrwd_cohort_year_full_w <- ocrwd_cohort_year_full_w %>%
  mutate(nEncounters_w = nEncounters*weight,
         nRWDEncounter_y_w = nRWDEncounter_y*weight)

# Export ocrwd_cohort_year_full_w to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv"
write.csv(ocrwd_cohort_year_full_w, file = file_path, row.names = FALSE)
