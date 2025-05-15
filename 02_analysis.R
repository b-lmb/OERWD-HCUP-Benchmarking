###############################################################################
# Purpose: Analyses to compare encounter prevalence for Aspergillosis and Histoplasmosis
# in OERWD and HCUP NIS
#
# Written by: Juliana GE Bartels
# Last Updated: 5/15/2025
###############################################################################
# Load libraries
library(tidyverse)
library(data.table)
library(tictoc)
library(survey)
library(ggplot2)
library(magrittr)
library(Kendall)
###############################################################################
# Calculate number and proportion of encounters in OERWD 
###############################################################################
# Load OERWD data
ocrwd_cohort_year_full_w <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv")

# OERWD numbers for table 1 (NIS results in NIS cleaning files and outputs)
ocrwd_censusdivision <- ocrwd_cohort_year_full_w %>%
  group_by(censusdivision) %>%
  summarize(nRWDEncounter_y = sum(nRWDEncounter_y)) %>%
  mutate(percent = round(nRWDEncounter_y/sum(nRWDEncounter_y)*100,1))

ocrwd_race <- ocrwd_cohort_year_full_w %>%
  group_by(RACE) %>%
  summarize(nRWDEncounter_y = sum(nRWDEncounter_y)) %>%
  mutate(percent = round(nRWDEncounter_y/sum(nRWDEncounter_y)*100,1))

ocrwd_female <- ocrwd_cohort_year_full_w %>%
  group_by(FEMALE) %>%
  summarize(nRWDEncounter_y = sum(nRWDEncounter_y)) %>%
  mutate(percent = round(nRWDEncounter_y/sum(nRWDEncounter_y)*100,1))

ocrwd_age <- ocrwd_cohort_year_full_w %>%
  group_by(age_group) %>%
  summarize(nRWDEncounter_y = sum(nRWDEncounter_y)) %>%
  mutate(percent = round(nRWDEncounter_y/sum(nRWDEncounter_y)*100,1))

sum(ocrwd_censusdivision$nRWDEncounter_y)
###############################################################################
#  Prevalence by Year in HCUP NIS and OERWD
###############################################################################
# Aspergillosis NIS
totenc_weighted_asp <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_asp.csv") %>%
  rename(nNISEncounters_asp = ASP, seNISEncounters_asp = se, NISEncounters_asp_ci_l = ci_l, NISEncounters_asp_ci_u = ci_u )

# Histoplasmosis NIS
totenc_weighted_hist <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_hist.csv") %>%
  rename(nNISEncounters_hist = HIST, seNISEncounters_hist = se, NISEncounters_hist_ci_l = ci_l, NISEncounters_hist_ci_u = ci_u )

# Denominators
hospdiv_denom_year <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/hospdiv_denom_year.csv") %>%
  rename(nNISEncounters = ENCOUNTERS,seNISEncounters = SE, NISEncounters_ci_l = ci_l, NISEncounters_ci_u = ci_u ) %>%
  dplyr::select(-YEAR)

# Calculate prevalence for NIS Aspergillosis
NIS_asp <- cbind(totenc_weighted_asp, hospdiv_denom_year) %>%
  mutate(prev = nNISEncounters_asp/nNISEncounters * 10000, 
         se_prev = sqrt(nNISEncounters_asp/nNISEncounters * (1 - (nNISEncounters_asp/nNISEncounters)) / nNISEncounters) * 10000,
         prev_ci_l = prev - 1.96 * se_prev,
         prev_ci_u = prev + 1.96 * se_prev,
         Disease = "Aspergillosis") %>%
  dplyr::select(Disease, YEAR, prev, prev_ci_l, prev_ci_u)

# Calculate prevalence for NIS Histoplasmosis
NIS_hist <- cbind(totenc_weighted_hist, hospdiv_denom_year) %>%
  mutate(prev = nNISEncounters_hist/nNISEncounters * 10000, 
         se_prev = sqrt(nNISEncounters_hist/nNISEncounters * (1 - (nNISEncounters_hist/nNISEncounters)) / nNISEncounters) * 10000,
         prev_ci_l = prev - 1.96 * se_prev,
         prev_ci_u = prev + 1.96 * se_prev,
         Disease = "Histoplasmosis") %>%
  dplyr::select(Disease, YEAR, prev, prev_ci_l, prev_ci_u)

NIS_all <- rbind(NIS_asp, NIS_hist)

# OERWD Cohort Data
ocrwd_cohort_year_full_w <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv')

ocrwd_cohort_prev <- ocrwd_cohort_year_full_w %>%
  group_by(Disease, YEAR) %>%
  summarize(nEncounters_w = sum(nEncounters_w),
            nRWDEncounter_y_w = sum(nRWDEncounter_y_w)) %>%
  mutate(prevalence = nEncounters_w/nRWDEncounter_y_w * 10000,
         se_prevalence = sqrt((nEncounters_w / nRWDEncounter_y_w) * (1 - (nEncounters_w / nRWDEncounter_y_w)) / nRWDEncounter_y_w) * 10000,
         lower_ci_prevalence = prevalence - 1.96 * se_prevalence,
         upper_ci_prevalence = prevalence + 1.96 * se_prevalence)

all_data <- full_join(NIS_all, ocrwd_cohort_prev, by = c("Disease", "YEAR"))

all_data %>% filter(Disease == "Histoplasmosis") %$% prev

# compare the trends using Kendall's Tau - Histoplasmosis
cor.test(all_data %>% filter(Disease == "Histoplasmosis") %$% prev, # NIS 
         all_data %>% filter(Disease == "Histoplasmosis") %$% prevalence, # RWD Enc
         method = "kendall")

# compare the trends using Kendall's Tau - Aspergillosis
cor.test(all_data %>% filter(Disease == "Aspergillosis") %$% prev, # NIS 
         all_data %>% filter(Disease == "Aspergillosis") %$% prevalence, # RWD Enc
         method = "kendall")

# Mann Kendall to see the direction of trend over time - Histoplasmosis 
MannKendall(all_data %>% filter(Disease == "Histoplasmosis") %$% prev) # NIS
MannKendall(all_data %>% filter(Disease == "Histoplasmosis") %$% prevalence) # RWD Enc

# Mann Kendall to see the direction of trend over time - Aspergillosis 
MannKendall(all_data %>% filter(Disease == "Aspergillosis") %$% prev) # NIS
MannKendall(all_data %>% filter(Disease == "Aspergillosis") %$% prevalence) # RWD Enc

# t-test to compare the two linear trends - Histoplasmosis
t.test(all_data %>% filter(Disease == "Histoplasmosis") %$% prev, # NIS
       all_data %>% filter(Disease == "Histoplasmosis") %$% prevalence # RWD
)

# t-test to compare the two linear trends - Aspergillosis
t.test(all_data %>% filter(Disease == "Aspergillosis") %$% prev, # NIS
       all_data %>% filter(Disease == "Aspergillosis") %$% prevalence # RWD
)

###############################################################################
# Calculate prevalence, relative difference in encounter totals and prevalence,
# t-values, and p-values for Aspergillosis and Histoplasmosis in HCUP NIS and OERWD
###############################################################################
############################## OVERALL #######################################
# Upload Aspergillosis Encounters NIS
totenc_weighted_total_asp <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_total_asp.csv") %>%
  rename(nNISEncounters = total, seNISEncounters = ASP) %>%
  mutate(Disease = "Aspergillosis")

# Upload Histoplasmosis Encounters NIS
hospdiv_denom_total <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/hospdiv_denom_total.csv") %>%
  rename(nNISEncounters_tot = ENCOUNTERS, seNISEncounters_tot = SE)

totenc_weighted_total_asp <- cbind(totenc_weighted_total_asp, hospdiv_denom_total)

totenc_weighted_total_hist <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_total_hist.csv") %>%
  rename(nNISEncounters = total, seNISEncounters = HIST) %>%
  mutate(Disease = "Histoplasmosis")

totenc_weighted_total_hist <- cbind(totenc_weighted_total_hist, hospdiv_denom_total)

nis_encounters_total <- rbind(totenc_weighted_total_asp, totenc_weighted_total_hist)

# Upload OERWD data
ocrwd_cohort_total <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv') %>%
  group_by(Disease) %>%
  summarize(nEncounters = sum(nEncounters),
            nEncounters_w = sum(nEncounters_w),
            nRWDEncounter_y = sum(nRWDEncounter_y),
            nRWDEncounter_y_w = sum(nRWDEncounter_y_w))

#combine
all_data_total <- full_join(nis_encounters_total, ocrwd_cohort_total, by = c("Disease")) 

# Calculate prevalence, relative difference, t-value, p-value, relative difference
# in prevalence, t-value for prevalence, p-value for prevalence for both datasets
table2_total <- all_data_total %>%
  mutate(prev_NIS = (nNISEncounters / nNISEncounters_tot) * 10000,
         prev_NIS_se = sqrt(
           (seNISEncounters / nNISEncounters_tot)^2 +
             ((nNISEncounters * seNISEncounters_tot) / (nNISEncounters_tot^2))^2
         ) * 10000,
         prev_NIS_lower_ci = prev_NIS - 1.96 * prev_NIS_se,
         prev_NIS_upper_ci = prev_NIS + 1.96 * prev_NIS_se,
         prev_OCRWD = (nEncounters/nRWDEncounter_y)*10000,
         prev_OCRWD_se = sqrt((nEncounters / nRWDEncounter_y) * (1 - (nEncounters / nRWDEncounter_y)) / nRWDEncounter_y) * 10000,
         prev_OCRWD_lower_ci = prev_OCRWD - 1.96 * prev_OCRWD_se,
         prev_OCRWD_upper_ci = prev_OCRWD + 1.96 * prev_OCRWD_se,
         prev_OCRWD_w = (nEncounters_w/nRWDEncounter_y_w)*10000,
         prev_OCRWD_w_se = sqrt((nEncounters_w / nRWDEncounter_y_w) * (1 - (nEncounters_w / nRWDEncounter_y_w)) / nRWDEncounter_y_w) * 10000,
         prev_OCRWD_w_lower_ci = prev_OCRWD_w - 1.96 * prev_OCRWD_w_se,
         prev_OCRWD_w_upper_ci = prev_OCRWD_w + 1.96 * prev_OCRWD_w_se,
         reldiff = (nNISEncounters - nEncounters)/nNISEncounters,
         reldiff_w = (nNISEncounters - nEncounters_w)/nNISEncounters,
         tvalue = ((nNISEncounters - nEncounters)/seNISEncounters),
         tvalue_w = ((nNISEncounters - nEncounters_w)/seNISEncounters),
         p_value = 2 * (1 - pnorm(abs(tvalue))),
         p_value_w = 2 * (1 - pnorm(abs(tvalue_w))),
         reldiff_prev = (prev_NIS - prev_OCRWD)/prev_NIS,
         reldiff_prev_se = sqrt(((prev_OCRWD/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_se^2))),
         reldiff_prev_lower_ci = reldiff_prev - 1.96 * reldiff_prev_se,
         reldiff_prev_upper_ci = reldiff_prev + 1.96 * reldiff_prev_se,
         reldiff_prev_w = (prev_NIS - prev_OCRWD_w)/prev_NIS,
         reldiff_prev_w_se = sqrt(((prev_OCRWD_w/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_w_se^2))),
         reldiff_prev_w_lower_ci = reldiff_prev_w - 1.96 * reldiff_prev_w_se,
         reldiff_prev_w_upper_ci = reldiff_prev_w + 1.96 * reldiff_prev_w_se,
         t_value_prev = abs(prev_NIS/10000 - prev_OCRWD/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD / 10000) * (1 - (prev_OCRWD / 10000))) / nEncounters
         ),
         t_value_prev_w = abs(prev_NIS/10000 - prev_OCRWD_w/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD_w / 10000) * (1 - (prev_OCRWD_w / 10000))) / nEncounters_w
         ),
         df = nNISEncounters + nEncounters - 2,
         df_w = nNISEncounters + nEncounters_w - 2,
         p_value_prev = 2 * (1 - pnorm(abs(t_value_prev))),
         p_value_prev_w = 2 * (1 - pnorm(abs(t_value_prev_w)))
  ) 
View(
  table2_total %>%
    mutate(
      across(
        c(prev_NIS, prev_NIS_lower_ci, prev_NIS_upper_ci,
          prev_OCRWD, prev_OCRWD_lower_ci, prev_OCRWD_upper_ci,
          prev_OCRWD_w, prev_OCRWD_w_lower_ci, prev_OCRWD_w_upper_ci),
        ~ round(.x, 1)
      ),
      across(
        c(reldiff, reldiff_w, reldiff_prev, reldiff_prev_w, reldiff_prev_lower_ci, reldiff_prev_upper_ci,
          reldiff_prev_w_lower_ci, reldiff_prev_w_upper_ci),
        ~ round(.x * 100, 1)
      )
    )
)
# Export table2_total to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/figures/table2_total.csv"

write.csv(table2_total, file = file_path, row.names = FALSE)

######################## By Census Division ####################################

# Upload Aspergillosis Division Encounters NIS
totenc_weighted_div_asp <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_div_asp.csv") %>%
  rename(nNISEncounters = ASP, seNISEncounters = se) %>%
  mutate(Disease = "Aspergillosis") %>%
  dplyr::select(-ci_l, -ci_u) %>%
  rename(censusdivision = HOSP_DIVISION)

# Upload Histoplasmosis Division Encounters NIS
totenc_weighted_div_hist <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_div_hist.csv") %>%
  rename(nNISEncounters = HIST, seNISEncounters = se) %>%
  mutate(Disease = "Histoplasmosis") %>%
  dplyr::select(-ci_l, -ci_u) %>%
  rename(censusdivision = HOSP_DIVISION)

nis_encounters <- rbind(totenc_weighted_div_asp, totenc_weighted_div_hist)

# Upload Denominators NIS
hospdiv_denom_cd <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/hospdiv_denom_cd.csv") %>%
  rename(nNISEncounters_tot = ENCOUNTERS, seNISEncounters_tot = SE) %>%
  dplyr::select(-ci_l, -ci_u) %>%
  rename(censusdivision = HOSP_DIVISION)

nis_encounters <- full_join(nis_encounters, hospdiv_denom_cd, by = c("censusdivision"))

# Upload OERWD data
ocrwd_cohort_cdiv <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv') %>%
  group_by(Disease, censusdivision) %>%
  summarize(nEncounters = sum(nEncounters),
            nEncounters_w = sum(nEncounters_w),
            nRWDEncounter_y = sum(nRWDEncounter_y),
            nRWDEncounter_y_w = sum(nRWDEncounter_y_w))

# combine all
all_data_cdiv <- full_join(ocrwd_cohort_cdiv, nis_encounters, by = c("Disease", "censusdivision"))
View(all_data_cdiv)

options(digits = 10)
# Calculate prevalence, relative difference, t-value, p-value, relative difference
# in prevalence, t-value for prevalence, p-value for prevalence for both datasets
table2_cdiv <- all_data_cdiv %>%
  mutate(prev_NIS = (nNISEncounters / nNISEncounters_tot) * 10000,
         prev_NIS_se = sqrt(
           (seNISEncounters / nNISEncounters_tot)^2 +
             ((nNISEncounters * seNISEncounters_tot) / (nNISEncounters_tot^2))^2
         ) * 10000,
         prev_NIS_lower_ci = prev_NIS - 1.96 * prev_NIS_se,
         prev_NIS_upper_ci = prev_NIS + 1.96 * prev_NIS_se,
         prev_OCRWD = (nEncounters/nRWDEncounter_y)*10000,
         prev_OCRWD_se = sqrt((nEncounters / nRWDEncounter_y) * (1 - (nEncounters / nRWDEncounter_y)) / nRWDEncounter_y) * 10000,
         prev_OCRWD_lower_ci = prev_OCRWD - 1.96 * prev_OCRWD_se,
         prev_OCRWD_upper_ci = prev_OCRWD + 1.96 * prev_OCRWD_se,
         prev_OCRWD_w = (nEncounters_w/nRWDEncounter_y_w)*10000,
         prev_OCRWD_w_se = sqrt((nEncounters_w / nRWDEncounter_y_w) * (1 - (nEncounters_w / nRWDEncounter_y_w)) / nRWDEncounter_y_w) * 10000,
         prev_OCRWD_w_lower_ci = prev_OCRWD_w - 1.96 * prev_OCRWD_w_se,
         prev_OCRWD_w_upper_ci = prev_OCRWD_w + 1.96 * prev_OCRWD_w_se,
         reldiff = (nNISEncounters - nEncounters)/nNISEncounters,
         reldiff_w = (nNISEncounters - nEncounters_w)/nNISEncounters,
         tvalue = ((nNISEncounters - nEncounters)/seNISEncounters),
         tvalue_w = ((nNISEncounters - nEncounters_w)/seNISEncounters),
         p_value = 2 * (1 - pnorm(abs(tvalue))),
         p_value_w = 2 * (1 - pnorm(abs(tvalue_w))),
         reldiff_prev = (prev_NIS - prev_OCRWD)/prev_NIS,
         reldiff_prev_se = sqrt(((prev_OCRWD/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_se^2))),
         reldiff_prev_lower_ci = reldiff_prev - 1.96 * reldiff_prev_se,
         reldiff_prev_upper_ci = reldiff_prev + 1.96 * reldiff_prev_se,
         reldiff_prev_w = (prev_NIS - prev_OCRWD_w)/prev_NIS,
         reldiff_prev_w_se = sqrt(((prev_OCRWD_w/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_w_se^2))),
         reldiff_prev_w_lower_ci = reldiff_prev_w - 1.96 * reldiff_prev_w_se,
         reldiff_prev_w_upper_ci = reldiff_prev_w + 1.96 * reldiff_prev_w_se,
         t_value_prev = abs(prev_NIS/10000 - prev_OCRWD/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD / 10000) * (1 - (prev_OCRWD / 10000))) / nEncounters
         ),
         t_value_prev_w = abs(prev_NIS/10000 - prev_OCRWD_w/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD_w / 10000) * (1 - (prev_OCRWD_w / 10000))) / nEncounters_w
         ),
         df = nNISEncounters + nEncounters - 2,
         df_w = nNISEncounters + nEncounters_w - 2,
         p_value_prev = 2 * (1 - pnorm(abs(t_value_prev))),
         p_value_prev_w = 2 * (1 - pnorm(abs(t_value_prev_w)))
  ) 
View(
  table2_cdiv %>%
    mutate(
      across(
        c(prev_NIS, prev_NIS_lower_ci, prev_NIS_upper_ci,
          prev_OCRWD, prev_OCRWD_lower_ci, prev_OCRWD_upper_ci,
          prev_OCRWD_w, prev_OCRWD_w_lower_ci, prev_OCRWD_w_upper_ci),
        ~ round(.x, 1)
      ),
      across(
        c(reldiff, reldiff_w, reldiff_prev, reldiff_prev_w, reldiff_prev_lower_ci, reldiff_prev_upper_ci,
          reldiff_prev_w_lower_ci, reldiff_prev_w_upper_ci),
        ~ round(.x * 100, 1)
      )
    )
)
# Export table2_cdiv to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/figures/table2_cdiv.csv"
write.csv(table2_cdiv, file = file_path, row.names = FALSE)

############################## By Gender #######################################
# Upload Aspergillosis Encounters NIS
totenc_weighted_female_asp <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_female_asp.csv") %>%
  rename(nNISEncounters = ASP, seNISEncounters = se) %>%
  mutate(Disease = "Aspergillosis") %>%
  dplyr::select(-ci_l, -ci_u)

# Upload Histoplasmosis Encounters NIS
totenc_weighted_female_hist <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_female_hist.csv") %>%
  rename(nNISEncounters = HIST, seNISEncounters = se) %>%
  mutate(Disease = "Histoplasmosis") %>%
  dplyr::select(-ci_l, -ci_u)

nis_encounters_female <- rbind(totenc_weighted_female_asp, totenc_weighted_female_hist)

# Upload Denominators NIS
female_denom_sex <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/female_denom_sex.csv") %>%
  rename(nNISEncounters_tot = ENCOUNTERS, seNISEncounters_tot = SE) %>%
  dplyr::select(-ci_l, -ci_u) %>%
  filter(FEMALE != "Unknown") %>%
  mutate(FEMALE = as.integer(FEMALE))

nis_encounters_female <- full_join(nis_encounters_female, female_denom_sex, by = c("FEMALE"))

# Upload OERWD data
ocrwd_cohort_female <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv') %>%
  group_by(Disease, FEMALE) %>%
  summarize(nEncounters = sum(nEncounters),
            nEncounters_w = sum(nEncounters_w),
            nRWDEncounter_y = sum(nRWDEncounter_y),
            nRWDEncounter_y_w = sum(nRWDEncounter_y_w))

all_data_female <- full_join(ocrwd_cohort_female, nis_encounters_female, by = c("Disease", "FEMALE")) %>%
  filter(FEMALE != 2)

# Calculate prevalence, relative difference, t-value, p-value, relative difference
# in prevalence, t-value for prevalence, p-value for prevalence for both datasets
table2_female <- all_data_female %>%
  mutate(prev_NIS = (nNISEncounters / nNISEncounters_tot) * 10000,
         prev_NIS_se = sqrt(
           (seNISEncounters / nNISEncounters_tot)^2 +
             ((nNISEncounters * seNISEncounters_tot) / (nNISEncounters_tot^2))^2
         ) * 10000,
         prev_NIS_lower_ci = prev_NIS - 1.96 * prev_NIS_se,
         prev_NIS_upper_ci = prev_NIS + 1.96 * prev_NIS_se,
         prev_OCRWD = (nEncounters/nRWDEncounter_y)*10000,
         prev_OCRWD_se = sqrt((nEncounters / nRWDEncounter_y) * (1 - (nEncounters / nRWDEncounter_y)) / nRWDEncounter_y) * 10000,
         prev_OCRWD_lower_ci = prev_OCRWD - 1.96 * prev_OCRWD_se,
         prev_OCRWD_upper_ci = prev_OCRWD + 1.96 * prev_OCRWD_se,
         prev_OCRWD_w = (nEncounters_w/nRWDEncounter_y_w)*10000,
         prev_OCRWD_w_se = sqrt((nEncounters_w / nRWDEncounter_y_w) * (1 - (nEncounters_w / nRWDEncounter_y_w)) / nRWDEncounter_y_w) * 10000,
         prev_OCRWD_w_lower_ci = prev_OCRWD_w - 1.96 * prev_OCRWD_w_se,
         prev_OCRWD_w_upper_ci = prev_OCRWD_w + 1.96 * prev_OCRWD_w_se,
         reldiff = (nNISEncounters - nEncounters)/nNISEncounters,
         reldiff_w = (nNISEncounters - nEncounters_w)/nNISEncounters,
         tvalue = ((nNISEncounters - nEncounters)/seNISEncounters),
         tvalue_w = ((nNISEncounters - nEncounters_w)/seNISEncounters),
         p_value = 2 * (1 - pnorm(abs(tvalue))),
         p_value_w = 2 * (1 - pnorm(abs(tvalue_w))),
         reldiff_prev = (prev_NIS - prev_OCRWD)/prev_NIS,
         reldiff_prev_se = sqrt(((prev_OCRWD/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_se^2))),
         reldiff_prev_lower_ci = reldiff_prev - 1.96 * reldiff_prev_se,
         reldiff_prev_upper_ci = reldiff_prev + 1.96 * reldiff_prev_se,
         reldiff_prev_w = (prev_NIS - prev_OCRWD_w)/prev_NIS,
         reldiff_prev_w_se = sqrt(((prev_OCRWD_w/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_w_se^2))),
         reldiff_prev_w_lower_ci = reldiff_prev_w - 1.96 * reldiff_prev_w_se,
         reldiff_prev_w_upper_ci = reldiff_prev_w + 1.96 * reldiff_prev_w_se,
         t_value_prev = abs(prev_NIS/10000 - prev_OCRWD/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD / 10000) * (1 - (prev_OCRWD / 10000))) / nEncounters
         ),
         t_value_prev_w = abs(prev_NIS/10000 - prev_OCRWD_w/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD_w / 10000) * (1 - (prev_OCRWD_w / 10000))) / nEncounters_w
         ),
         df = nNISEncounters + nEncounters - 2,
         df_w = nNISEncounters + nEncounters_w - 2,
         p_value_prev = 2 * (1 - pnorm(abs(t_value_prev))),
         p_value_prev_w = 2 * (1 - pnorm(abs(t_value_prev_w)))
  ) 
View(
  table2_female %>%
    mutate(
      across(
        c(prev_NIS, prev_NIS_lower_ci, prev_NIS_upper_ci,
          prev_OCRWD, prev_OCRWD_lower_ci, prev_OCRWD_upper_ci,
          prev_OCRWD_w, prev_OCRWD_w_lower_ci, prev_OCRWD_w_upper_ci),
        ~ round(.x, 1)
      ),
      across(
        c(reldiff, reldiff_w, reldiff_prev, reldiff_prev_w, reldiff_prev_lower_ci, reldiff_prev_upper_ci,
          reldiff_prev_w_lower_ci, reldiff_prev_w_upper_ci),
        ~ round(.x * 100, 1)
      )
    )
)
# Export table2_female to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/figures/table2_female.csv"
write.csv(table2_female, file = file_path, row.names = FALSE)

############################## By RACE #######################################
# Upload Aspergillosis Division Encounters NIS
totenc_weighted_race_asp <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_race_asp.csv") %>%
  rename(nNISEncounters = ASP, seNISEncounters = se) %>%
  mutate(Disease = "Aspergillosis") %>%
  dplyr::select(-ci_l, -ci_u)

# Upload Histoplasmosis Division Encounters NIS
totenc_weighted_race_hist <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_race_hist.csv") %>%
  rename(nNISEncounters = HIST, seNISEncounters = se) %>%
  mutate(Disease = "Histoplasmosis") %>%
  dplyr::select(-ci_l, -ci_u)

nis_encounters_race <- rbind(totenc_weighted_race_asp, totenc_weighted_race_hist)

# Upload Denominators NIS
race_denom_race <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/race_denom_race.csv") %>%
  rename(nNISEncounters_tot = ENCOUNTERS, seNISEncounters_tot = SE) %>%
  dplyr::select(-ci_l, -ci_u) %>%
  mutate(RACE = case_when(
    RACE == "Unknown" ~ "7",
    TRUE ~ RACE),
    RACE = as.integer(RACE))

nis_encounters_race <- full_join(nis_encounters_race, race_denom_race, by = c("RACE"))

# Upload OERWD data
ocrwd_cohort_race <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv') %>%
  group_by(Disease, RACE) %>%
  summarize(nEncounters = sum(nEncounters),
            nEncounters_w = sum(nEncounters_w),
            nRWDEncounter_y = sum(nRWDEncounter_y),
            nRWDEncounter_y_w = sum(nRWDEncounter_y_w))

# combine
all_data_race <- full_join(ocrwd_cohort_race, nis_encounters_race, by = c("Disease", "RACE")) %>%
  filter(RACE != 7)

# Calculate prevalence, relative difference, t-value, p-value, relative difference
# in prevalence, t-value for prevalence, p-value for prevalence for both datasets

table2_race <- all_data_race %>%
  mutate(prev_NIS = (nNISEncounters / nNISEncounters_tot) * 10000,
         prev_NIS_se = sqrt(
           (seNISEncounters / nNISEncounters_tot)^2 +
             ((nNISEncounters * seNISEncounters_tot) / (nNISEncounters_tot^2))^2
         ) * 10000,
         prev_NIS_lower_ci = prev_NIS - 1.96 * prev_NIS_se,
         prev_NIS_upper_ci = prev_NIS + 1.96 * prev_NIS_se,
         prev_OCRWD = (nEncounters/nRWDEncounter_y)*10000,
         prev_OCRWD_se = sqrt((nEncounters / nRWDEncounter_y) * (1 - (nEncounters / nRWDEncounter_y)) / nRWDEncounter_y) * 10000,
         prev_OCRWD_lower_ci = prev_OCRWD - 1.96 * prev_OCRWD_se,
         prev_OCRWD_upper_ci = prev_OCRWD + 1.96 * prev_OCRWD_se,
         prev_OCRWD_w = (nEncounters_w/nRWDEncounter_y_w)*10000,
         prev_OCRWD_w_se = sqrt((nEncounters_w / nRWDEncounter_y_w) * (1 - (nEncounters_w / nRWDEncounter_y_w)) / nRWDEncounter_y_w) * 10000,
         prev_OCRWD_w_lower_ci = prev_OCRWD_w - 1.96 * prev_OCRWD_w_se,
         prev_OCRWD_w_upper_ci = prev_OCRWD_w + 1.96 * prev_OCRWD_w_se,
         reldiff = (nNISEncounters - nEncounters)/nNISEncounters,
         reldiff_w = (nNISEncounters - nEncounters_w)/nNISEncounters,
         tvalue = ((nNISEncounters - nEncounters)/seNISEncounters),
         tvalue_w = ((nNISEncounters - nEncounters_w)/seNISEncounters),
         p_value = 2 * (1 - pnorm(abs(tvalue))),
         p_value_w = 2 * (1 - pnorm(abs(tvalue_w))),
         reldiff_prev = (prev_NIS - prev_OCRWD)/prev_NIS,
         reldiff_prev_se = sqrt(((prev_OCRWD/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_se^2))),
         reldiff_prev_lower_ci = reldiff_prev - 1.96 * reldiff_prev_se,
         reldiff_prev_upper_ci = reldiff_prev + 1.96 * reldiff_prev_se,
         reldiff_prev_w = (prev_NIS - prev_OCRWD_w)/prev_NIS,
         reldiff_prev_w_se = sqrt(((prev_OCRWD_w/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_w_se^2))),
         reldiff_prev_w_lower_ci = reldiff_prev_w - 1.96 * reldiff_prev_w_se,
         reldiff_prev_w_upper_ci = reldiff_prev_w + 1.96 * reldiff_prev_w_se,
         t_value_prev = abs(prev_NIS/10000 - prev_OCRWD/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD / 10000) * (1 - (prev_OCRWD / 10000))) / nEncounters
         ),
         t_value_prev_w = abs(prev_NIS/10000 - prev_OCRWD_w/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD_w / 10000) * (1 - (prev_OCRWD_w / 10000))) / nEncounters_w
         ),
         df = nNISEncounters + nEncounters - 2,
         df_w = nNISEncounters + nEncounters_w - 2,
         p_value_prev = 2 * (1 - pnorm(abs(t_value_prev))),
         p_value_prev_w = 2 * (1 - pnorm(abs(t_value_prev_w)))
  ) 
View(
  table2_race %>%
    mutate(
      across(
        c(prev_NIS, prev_NIS_lower_ci, prev_NIS_upper_ci,
          prev_OCRWD, prev_OCRWD_lower_ci, prev_OCRWD_upper_ci,
          prev_OCRWD_w, prev_OCRWD_w_lower_ci, prev_OCRWD_w_upper_ci),
        ~ round(.x, 1)
      ),
      across(
        c(reldiff, reldiff_w, reldiff_prev, reldiff_prev_w, reldiff_prev_lower_ci, reldiff_prev_upper_ci,
          reldiff_prev_w_lower_ci, reldiff_prev_w_upper_ci),
        ~ round(.x * 100, 1)
      )
    )
)
# Export table2_race to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/figures/table2_race.csv"
write.csv(table2_race, file = file_path, row.names = FALSE)

############################## By AGE #######################################
# Upload Aspergillosis Encounters NIS
totenc_weighted_age_asp <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_age_asp.csv") %>%
  rename(nNISEncounters = ASP, seNISEncounters = se) %>%
  mutate(Disease = "Aspergillosis") %>%
  dplyr::select(-ci_l, -ci_u)

# Upload Histoplasmosis Encounters NIS
totenc_weighted_age_hist <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/totenc_weighted_age_hist.csv") %>%
  rename(nNISEncounters = HIST, seNISEncounters = se) %>%
  mutate(Disease = "Histoplasmosis") %>%
  dplyr::select(-ci_l, -ci_u)

nis_encounters_age <- rbind(totenc_weighted_age_asp, totenc_weighted_age_hist)

# Upload Denominators NIS
age_denom_age_group <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/age_denom_age_group.csv") %>%
  rename(nNISEncounters_tot = ENCOUNTERS, seNISEncounters_tot = SE, age_group = AGE_GROUP) %>%
  dplyr::select(-ci_l, -ci_u) %>%
  mutate(age_group = case_when(
    age_group == "18-34" ~ "18 to 34",
    age_group == "35-64" ~ "35 to 64",
    age_group == "65+" ~ "65 and Over",
    TRUE ~ age_group
  )) %>%
  filter(
    age_group != ""
  )

nis_encounters_age <- full_join(nis_encounters_age, age_denom_age_group, by = c("age_group"))

# Upload OERWD data
ocrwd_cohort_age <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv') %>%
  group_by(Disease, age_group) %>%
  summarize(nEncounters = sum(nEncounters),
            nEncounters_w = sum(nEncounters_w),
            nRWDEncounter_y = sum(nRWDEncounter_y),
            nRWDEncounter_y_w = sum(nRWDEncounter_y_w))

all_data_age <- full_join(ocrwd_cohort_age, nis_encounters_age, by = c("Disease", "age_group")) %>%
  filter(age_group != "Unknown")

ocrwd_cohort_age <- fread('~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/outputs/ocrwd_cohort_year_full_w.csv') %>%
  filter(age_group != "Unknown") %>%
  group_by(Disease, age_group) %>%
  summarize(nEncounters = sum(nEncounters),
            nEncounters_w = sum(nEncounters_w),
            nRWDEncounter_y = sum(nRWDEncounter_y),
            nRWDEncounter_y_w = sum(nRWDEncounter_y_w))

# combine
all_data_age <- full_join(ocrwd_cohort_age, nis_encounters_age, by = c("Disease", "age_group"))

# Calculate prevalence, relative difference, t-value, p-value, relative difference
# in prevalence, t-value for prevalence, p-value for prevalence for both datasets
table2_age <- all_data_age %>%
  mutate(prev_NIS = (nNISEncounters / nNISEncounters_tot) * 10000,
         prev_NIS_se = sqrt(
           (seNISEncounters / nNISEncounters_tot)^2 +
             ((nNISEncounters * seNISEncounters_tot) / (nNISEncounters_tot^2))^2
         ) * 10000,
         prev_NIS_lower_ci = prev_NIS - 1.96 * prev_NIS_se,
         prev_NIS_upper_ci = prev_NIS + 1.96 * prev_NIS_se,
         prev_OCRWD = (nEncounters/nRWDEncounter_y)*10000,
         prev_OCRWD_se = sqrt((nEncounters / nRWDEncounter_y) * (1 - (nEncounters / nRWDEncounter_y)) / nRWDEncounter_y) * 10000,
         prev_OCRWD_lower_ci = prev_OCRWD - 1.96 * prev_OCRWD_se,
         prev_OCRWD_upper_ci = prev_OCRWD + 1.96 * prev_OCRWD_se,
         prev_OCRWD_w = (nEncounters_w/nRWDEncounter_y_w)*10000,
         prev_OCRWD_w_se = sqrt((nEncounters_w / nRWDEncounter_y_w) * (1 - (nEncounters_w / nRWDEncounter_y_w)) / nRWDEncounter_y_w) * 10000,
         prev_OCRWD_w_lower_ci = prev_OCRWD_w - 1.96 * prev_OCRWD_w_se,
         prev_OCRWD_w_upper_ci = prev_OCRWD_w + 1.96 * prev_OCRWD_w_se,
         reldiff = (nNISEncounters - nEncounters)/nNISEncounters,
         reldiff_w = (nNISEncounters - nEncounters_w)/nNISEncounters,
         tvalue = ((nNISEncounters - nEncounters)/seNISEncounters),
         tvalue_w = ((nNISEncounters - nEncounters_w)/seNISEncounters),
         p_value = 2 * (1 - pnorm(abs(tvalue))),
         p_value_w = 2 * (1 - pnorm(abs(tvalue_w))),
         reldiff_prev = (prev_NIS - prev_OCRWD)/prev_NIS,
         reldiff_prev_se = sqrt(((prev_OCRWD/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_se^2))),
         reldiff_prev_lower_ci = reldiff_prev - 1.96 * reldiff_prev_se,
         reldiff_prev_upper_ci = reldiff_prev + 1.96 * reldiff_prev_se,
         reldiff_prev_w = (prev_NIS - prev_OCRWD_w)/prev_NIS,
         reldiff_prev_w_se = sqrt(((prev_OCRWD_w/(prev_NIS^2)^2)*(prev_NIS_se^2)) + (((1/prev_NIS)^2)*(prev_OCRWD_w_se^2))),
         reldiff_prev_w_lower_ci = reldiff_prev_w - 1.96 * reldiff_prev_w_se,
         reldiff_prev_w_upper_ci = reldiff_prev_w + 1.96 * reldiff_prev_w_se,
         t_value_prev = abs(prev_NIS/10000 - prev_OCRWD/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD / 10000) * (1 - (prev_OCRWD / 10000))) / nEncounters
         ),
         t_value_prev_w = abs(prev_NIS/10000 - prev_OCRWD_w/10000) / sqrt(
           ((prev_NIS / 10000) * (1 - (prev_NIS / 10000))) / nNISEncounters +
             ((prev_OCRWD_w / 10000) * (1 - (prev_OCRWD_w / 10000))) / nEncounters_w
         ),
         df = nNISEncounters + nEncounters - 2,
         df_w = nNISEncounters + nEncounters_w - 2,
         p_value_prev = 2 * (1 - pnorm(abs(t_value_prev))),
         p_value_prev_w = 2 * (1 - pnorm(abs(t_value_prev_w)))
  ) 
View(
  table2_age %>%
    mutate(
      across(
        c(prev_NIS, prev_NIS_lower_ci, prev_NIS_upper_ci,
          prev_OCRWD, prev_OCRWD_lower_ci, prev_OCRWD_upper_ci,
          prev_OCRWD_w, prev_OCRWD_w_lower_ci, prev_OCRWD_w_upper_ci),
        ~ round(.x, 1)
      ),
      across(
        c(reldiff, reldiff_w, reldiff_prev, reldiff_prev_w, reldiff_prev_lower_ci, reldiff_prev_upper_ci,
          reldiff_prev_w_lower_ci, reldiff_prev_w_upper_ci),
        ~ round(.x * 100, 1)
      )
    )
)
# Export table2_age to csv
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/figures/table2_age.csv"
write.csv(table2_age, file = file_path, row.names = FALSE)

