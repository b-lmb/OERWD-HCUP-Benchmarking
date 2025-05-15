###############################################################################
# Purpose: Figures for comparison of prevalence of encounters in HCUP NIS and OERWD
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
library(RColorBrewer)
library(dplyr)
library(sf)
library(ggpattern)
library(readxl)
library(usmap)
library(cowplot)
library(ggrepel)
###############################################################################
# Figure 1. Prevalence of aspergillosis and histoplasmosis encounters in Healthcare 
# Cost and Utilization Project National Inpatient Sample (HCUP NIS) and Oracle EHR 
# Real World Data (OERWD) – United States, 2014-2020.
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

# Plot prevalences
all_data %>%
  ggplot(aes(x = YEAR, group = Disease, color = Disease)) +
  geom_point(aes(y = prev), size = 2) +
  geom_point(aes(y = prevalence), size = 2) +
  geom_line(aes(y = prev, linetype = "HCUP NIS")) + 
  geom_line(aes(y = prevalence, linetype = "OERWD")) +  
  geom_ribbon(aes(ymin = prev_ci_l, ymax = prev_ci_u), color = "NA", fill = "gray3", alpha = 0.1, show.legend = FALSE) + 
  geom_ribbon(aes(ymin = lower_ci_prevalence, ymax = upper_ci_prevalence), color = "NA", fill = "gray3", alpha = 0.1, show.legend = FALSE) + 
  labs(x = "Year", y = "Prevalence per 10,000 encounters") +
  scale_color_manual(values = c("#9A99F2", "#014F86"), name = "Fungal Disease", 
                     labels = c("Aspergillosis", "Histoplasmosis")) +
  scale_linetype_manual(name = "Dataset", 
                        values = c("HCUP NIS" = "dashed", "OERWD" = "solid"),
                        labels = c("HCUP NIS", "OERWD")) +
  scale_x_continuous(breaks = seq(2014,2020, by = 1)) +
  coord_cartesian(ylim = c(0, 6)) +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.51),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11))

file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/figures"
ggsave(
  "prevbyyear.jpg",
  path = file_path,
  width = 8, height = 5
)


###############################################################################
# Figure 2. Prevalence of aspergillosis and histoplasmosis diagnoses (per 10,000
# encounters) between 2014 to 2020 by census division estimated using Healthcare Cost 
# and Utilization Project National Inpatient Sample (HCUP NIS) and Oracle EHR Real 
# World Data (OERWD) 
###############################################################################
# Create color palettes
# census divisions palette 
cdiv_palette <- c("#310906", "#633533", "#90605e", "#e9b6b4", "#f3efe0", "#c1d3c8",
                  "#8fb7b0", "#597380", "#222e50")

################################################################################
# Load census divisions
censusdivisions <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/data/censusdivisions.csv")

# Join to table2 census division data
table2_cdiv <- fread("~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP paper/Repos/figures/table2_cdiv.csv")
table2_cdiv_formap <- right_join(censusdivisions, table2_cdiv, by = "censusdivision")
table2_cdiv_formap$censusdivision <- as.factor(table2_cdiv_formap$censusdivision)

# Make map of census divisions for reference
table2_cdiv_formap %>%
  mutate(censusdivision = case_when(
    censusdivision == 1 ~ "New England",
    censusdivision == 2 ~ "Middle Atlantic",
    censusdivision == 3 ~ "East North Central",
    censusdivision == 4 ~ "West North Central",
    censusdivision == 5 ~ "South Atlantic",
    censusdivision == 6 ~ "East South Central",
    censusdivision == 7 ~ "West South Central",
    censusdivision == 8 ~ "Mountain",
    censusdivision == 9 ~ "Pacific"
  )) %>%
  mutate(censusdivision = factor(censusdivision, levels = c("New England", "Middle Atlantic", 
                                                            "East North Central", "West North Central",
                                                            "South Atlantic", "East South Central",
                                                            "West South Central", "Mountain", "Pacific"))) %>%
  
  filter(state != "NA") %>%
  plot_usmap(data = ., 
             values = "censusdivision") +
  scale_fill_manual(values = cdiv_palette) +
  labs(fill = "Census Division") + 
  theme(panel.background = element_blank(), legend.position = "right")

# create census division map without labels
p0 <- table2_cdiv_formap %>%
  filter(state != "NA") %>%
  plot_usmap(data = ., 
             values = "censusdivision") +
  scale_fill_manual(values = cdiv_palette) +
  theme(panel.background = element_blank()) +
  guides(fill = "none")

# create numbered labels for census division map
centroid_labels <- usmapdata::centroid_labels("states") %>%
  rename(state = full) 

data_labels <- merge(centroid_labels, table2_cdiv_formap, by = "state") %>%
  group_by(censusdivision) %>%
  summarise(geometry = st_union(geometry), .groups = "drop",
            censusdivision = unique(censusdivision)) 

data_labels <- data_labels %>%
  mutate(geometry = case_when(
    censusdivision == 7 ~ {
      # Extract the bounding box of the geometry
      bbox <- st_bbox(geometry)
      # Shift xmin and ymin
      bbox["xmin"] <- bbox["xmin"] - 1000 
      bbox["ymin"] <- bbox["ymin"] - 1000000  
      # Create a polygon using the new bounding box coordinates
      new_geom <- st_polygon(list(rbind(
        c(bbox["xmin"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymin"])
      )))
      # Return the new geometry as an sf object with the same CRS
      st_sfc(new_geom, crs = st_crs(geometry))
    },
    censusdivision == 4 ~ {
      # Extract the bounding box of the geometry
      bbox <- st_bbox(geometry)
      # Shift xmin and ymin
      bbox["xmin"] <- bbox["xmin"] + 1000 
      bbox["ymin"] <- bbox["ymin"] + 1000000
      
      # Create a polygon using the new bounding box coordinates
      new_geom <- st_polygon(list(rbind(
        c(bbox["xmin"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymin"])
      )))
      # Return the new geometry as an sf object with the same CRS
      st_sfc(new_geom, crs = st_crs(geometry))
    },
    TRUE ~ geometry
  ))

# Plot the map with the shifted geometry
p0 <- p0 + 
  geom_sf_label(data = data_labels, 
                aes(label = censusdivision), 
                color = "black", 
                size = 2.5)

# Export census division reference map
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP Paper/Repos/figures"
ggsave(
  "censusdivisions_map.jpg",
  path = file_path
)

################################################################################
# Make maps

# Make map of HCUP NIS prevalence 
boundary_data <- us_map() %>%
  dplyr::rename(state = full)

# Combine boundaries with state data
table2_cdiv_formap_sf <- merge(boundary_data, table2_cdiv_formap, by = "state")
table2_cdiv_formap_sf_cd <- table2_cdiv_formap_sf %>%
  group_by(censusdivision) %>%
  summarise(geometry = st_union(geometry), .groups = "drop"
  )

# Aspergillosis
asp_map_data <- table2_cdiv_formap %>%
  filter(Disease == "Aspergillosis") %>%
  left_join(table2_cdiv_formap_sf_cd, by = "censusdivision") %>%
  st_as_sf()

asp_map_data$prev_NIS_cut <- cut(asp_map_data$prev_NIS,
                                 breaks = c(0, 2, 4, 6, 8, 10),
                                 labels = c("(0, 2]", "(2, 4]", "(4, 6]","(6, 8]", "(8, 10]"),
                                 include.lowest = TRUE)

asp_map_data$prev_OCRWD_w_cut <- cut(asp_map_data$prev_OCRWD_w,
                                     breaks = c(0, 2, 4, 6, 8, 10),
                                     labels = c("(0, 2]", "(2, 4]", "(4, 6]","(6, 8]", "(8, 10]"),
                                     include.lowest = TRUE)

# Aspergillosis - NIS Prevalence
p1 <- ggplot() +
  geom_sf(data = asp_map_data, aes(fill = prev_NIS_cut), color = "black",
          show.legend = c(fill = TRUE)) +
  scale_fill_manual(values = c("(0, 2]"  = "#e0aaff",  
                               "(2, 4]"  = "#c77dff",  
                               "(4, 6]"  = "#9d4edd",  
                               "(6, 8]"  = "#7b2cbf",  
                               "(8, 10]" = "#3c096c"),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = asp_map_data[asp_map_data$t_value_prev_w > 1.96, ], 
          color = "red", 
          fill = NA, 
          size = 3)

# Aspergillosis - OCRWD Prevalence
p2 <- ggplot() +
  geom_sf(data = asp_map_data, aes(fill = prev_OCRWD_w_cut), color = "black",
          show.legend = c(fill = TRUE)) +
  scale_fill_manual(values = c("(0, 2]"  = "#e0aaff",  
                               "(2, 4]"  = "#c77dff",  
                               "(4, 6]"  = "#9d4edd",  
                               "(6, 8]"  = "#7b2cbf",  
                               "(8, 10]" = "#3c096c"),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = asp_map_data[asp_map_data$t_value_prev_w > 1.96, ], 
          color = "red", 
          fill = NA, 
          size = 3)

# Histoplasmosis
hist_map_data <- table2_cdiv_formap %>%
  filter(Disease == "Histoplasmosis") %>%
  left_join(table2_cdiv_formap_sf_cd, by = "censusdivision") %>%
  st_as_sf()

hist_map_data$prev_NIS_cut <- cut(hist_map_data$prev_NIS,
                                  breaks = c(0, 1, 2, 3, 4, 5),
                                  labels = c("(0, 1]","(1, 2]", "(2, 3]", "(3, 4]", "(4, 5]"),
                                  include.lowest = TRUE)

hist_map_data$prev_OCRWD_w_cut <- cut(hist_map_data$prev_OCRWD_w,
                                      breaks = c(0, 1, 2, 3, 4, 5),
                                      labels = c("(0, 1]","(1, 2]", "(2, 3]", "(3, 4]", "(4, 5]"),
                                      include.lowest = TRUE)

# Histoplasmosis - NIS Prevalence
p3 <- ggplot() +
  geom_sf(data = hist_map_data, aes(fill = prev_NIS_cut), color = "black",
          show.legend = c(fill = TRUE)) +
  scale_fill_manual(values = c("(0, 1]" = "#caf0f8", 
                               "(1, 2]" = "#90e0ef", 
                               "(2, 3]" = "#00b4d8", 
                               "(3, 4]" = "#0077b6",
                               "(4, 5]" = "#03045e" ),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = hist_map_data[hist_map_data$t_value_prev_w > 1.96, ], 
          color = "red", 
          fill = NA, 
          size = 3)

# Histoplasmosis - OCRWD Prevalence
p4 <- ggplot() +
  geom_sf(data = hist_map_data, aes(fill = prev_OCRWD_w_cut), color = "black",
          show.legend = c(fill = TRUE)) +
  scale_fill_manual(values = c("(0, 1]" = "#caf0f8", 
                               "(1, 2]" = "#90e0ef", 
                               "(2, 3]" = "#00b4d8", 
                               "(3, 4]" = "#0077b6",
                               "(4, 5]" = "#03045e" ),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = hist_map_data[hist_map_data$t_value_prev_w > 1.96, ], 
          color = "red", 
          fill = NA, 
          size = 3)

################################################################################
# Plot all maps together
plot_grid(
  p1 + guides(fill = "none") + 
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),
          plot.title = element_text(hjust = 0.5)) +
    geom_sf_label(data = data_labels, 
                  aes(label = censusdivision), 
                  color = "black",
                  fill = "#F5F5F5",
                  size = 2.3,
                  label.size  = NA, 
                  alpha = 0.7),
  NULL,
  p2 + theme(plot.margin = margin(0, 0, 0, 0, "pt"),
             plot.title = element_text(hjust = 0.5)),
  p3 + guides(fill = "none") + theme(plot.margin = margin(0, 0, 0, 0, "pt"),
                                     plot.title = element_text(hjust = 0.5)),
  NULL,
  p4 + theme(plot.margin = margin(0, 0, 0, 0, "pt"),
             plot.title = element_text(hjust = 0.5)),
  ncol = 3,
  nrow = 2,
  align = "hv",
  labels = c("A.", "B.",'', "C.", "D.",''),
  label_x = c(0, 0.9, NA, 0, 0.9, NA),  # Move B and D slightly to the left
  rel_widths = c(1, -0.2, 1,1,-0.2,1),
  rel_heights = c(rep(1,6))
)

# Export map
file_path <- "~/Library/CloudStorage/Box-Box/GSR - EHR Epi/HCUP Paper/Repos/figures"
ggsave(
  "prevmaps.jpg",
  path = file_path
)


