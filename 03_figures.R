########################## Load Required Libraries ##########################

library(dplyr)
library(data.table)
library(ggplot2)
library(sf)
library(usmap)
library(cowplot)

########################## Load and Prepare Census Division Data ##########################

# Load census division lookup
censusdivisions <- fread("~/Desktop/HCUP Paper Repository/censusdivisions.csv")

# Load prevalence table and join census division info
cdiv_table_formap <- fread("~/Desktop/HCUP Paper Repository/cdiv_table_formap.csv")
cdiv_table_formap <- right_join(censusdivisions, cdiv_table_formap, by = "censusdivision")
cdiv_table_formap$censusdivision <- as.factor(cdiv_table_formap$censusdivision)

########################## Create Census Division Reference Map ##########################

# Create custom palette for census divisions
cdiv_palette <- c("#310906", "#633533", "#90605e", "#e9b6b4", "#f3efe0", "#c1d3c8",
                  "#8fb7b0", "#597380", "#222e50")

# Plot labeled census division map
cdiv_table_formap %>%
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
  plot_usmap(data = ., values = "censusdivision") +
  scale_fill_manual(values = cdiv_palette) +
  labs(fill = "Census Division") + 
  theme(panel.background = element_blank(), legend.position = "right")

########################## Create Unlabeled Census Division Base Map ##########################

# Base map
p0 <- cdiv_table_formap %>%
  filter(state != "NA") %>%
  plot_usmap(data = ., values = "censusdivision") +
  scale_fill_manual(values = cdiv_palette) +
  theme(panel.background = element_blank()) +
  guides(fill = "none")

########################## Generate Census Division Label Coordinates ##########################

# Merge coordinates
centroid_labels <- usmapdata::centroid_labels("states") %>%
  rename(state = full)

data_labels <- merge(centroid_labels, cdiv_table_formap, by = "state") %>%
  group_by(censusdivision) %>%
  summarise(geometry = st_union(geom), .groups = "drop",
            censusdivision = unique(censusdivision))

# Adjust label geometry for overlapping divisions
data_labels <- data_labels %>%
  mutate(geometry = case_when(
    censusdivision == 7 ~ {
      bbox <- st_bbox(geometry)
      bbox["xmin"] <- bbox["xmin"] - 1000 
      bbox["ymin"] <- bbox["ymin"] - 1000000  
      new_geom <- st_polygon(list(rbind(
        c(bbox["xmin"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymin"])
      )))
      st_sfc(new_geom, crs = st_crs(geometry))
    },
    censusdivision == 4 ~ {
      bbox <- st_bbox(geometry)
      bbox["xmin"] <- bbox["xmin"] + 1000 
      bbox["ymin"] <- bbox["ymin"] + 1000000
      new_geom <- st_polygon(list(rbind(
        c(bbox["xmin"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymax"]),
        c(bbox["xmax"], bbox["ymin"]),
        c(bbox["xmin"], bbox["ymin"])
      )))
      st_sfc(new_geom, crs = st_crs(geometry))
    },
    TRUE ~ geometry
  ))

# Add labels to census division map
p0 <- p0 + 
  geom_sf_label(data = data_labels, 
                aes(label = censusdivision), 
                color = "black", 
                size = 2.5)

# Export division reference map
file_path <- "~/Desktop"
ggsave("censusdivisions_map.jpg", path = file_path)

########################## Prepare Geographic Boundaries ##########################

# Load US map and rename state column
boundary_data <- us_map() %>%
  dplyr::rename(state = full)

# Merge boundary with prevalence data
cdiv_table_formap_sf <- merge(boundary_data, cdiv_table_formap, by = "state")

# Aggregate boundaries by census division
cdiv_table_formap_sf_cd <- cdiv_table_formap_sf %>%
  group_by(censusdivision) %>%
  summarise(geometry = st_union(geom), .groups = "drop")

########################## Create Map – Aspergillosis (NIS Prevalence) ##########################

asp_map_data <- cdiv_table_formap %>%
  filter(Disease == "Aspergillosis") %>%
  left_join(cdiv_table_formap_sf_cd, by = "censusdivision") %>%
  st_as_sf()

asp_map_data$prev_nis_cut <- cut(asp_map_data$prev_nis,
                                 breaks = c(0, 2, 4, 6, 8, 100),
                                 labels = c("(0, 2]", "(2, 4]", "(4, 6]","(6, 8]", "(8, 10]"),
                                 include.lowest = TRUE)

p1 <- ggplot() +
  geom_sf(data = asp_map_data, aes(fill = prev_nis_cut), color = "black") +
  scale_fill_manual(values = c("(0, 2]"  = "#e0aaff",  
                               "(2, 4]"  = "#c77dff",  
                               "(4, 6]"  = "#9d4edd",  
                               "(6, 8]"  = "#7b2cbf",  
                               "(8, 10]" = "#3c096c"),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = asp_map_data[asp_map_data$stat_sig == 1, ], 
          color = "red", fill = NA, size = 3)

########################## Create Map – Aspergillosis (OERWD Prevalence) ##########################

asp_map_data$prev_oerwd_w_cut <- cut(asp_map_data$prev_oerwd_w,
                                     breaks = c(0, 2, 4, 6, 8, 100),
                                     labels = c("(0, 2]", "(2, 4]", "(4, 6]","(6, 8]", "(8, 10]"),
                                     include.lowest = TRUE)

p2 <- ggplot() +
  geom_sf(data = asp_map_data, aes(fill = prev_oerwd_w_cut), color = "black") +
  scale_fill_manual(values = c("(0, 2]"  = "#e0aaff",  
                               "(2, 4]"  = "#c77dff",  
                               "(4, 6]"  = "#9d4edd",  
                               "(6, 8]"  = "#7b2cbf",  
                               "(8, 10]" = "#3c096c"),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = asp_map_data[asp_map_data$stat_sig == 1, ], 
          color = "red", fill = NA, size = 3)

########################## Create Map – Histoplasmosis (NIS Prevalence) ##########################

hist_map_data <- cdiv_table_formap %>%
  filter(Disease == "Histoplasmosis") %>%
  left_join(cdiv_table_formap_sf_cd, by = "censusdivision") %>%
  st_as_sf()

hist_map_data$prev_nis_cut <- cut(hist_map_data$prev_nis,
                                  breaks = c(0, 1, 2, 3, 4, 5),
                                  labels = c("(0, 1]","(1, 2]", "(2, 3]", "(3, 4]", "(4, 5]"),
                                  include.lowest = TRUE)

p3 <- ggplot() +
  geom_sf(data = hist_map_data, aes(fill = prev_nis_cut), color = "black") +
  scale_fill_manual(values = c("(0, 1]" = "#caf0f8", 
                               "(1, 2]" = "#90e0ef", 
                               "(2, 3]" = "#00b4d8", 
                               "(3, 4]" = "#0077b6",
                               "(4, 5]" = "#03045e"),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = hist_map_data[hist_map_data$stat_sig == 1, ], 
          color = "red", fill = NA, size = 3)

########################## Create Map – Histoplasmosis (OERWD Prevalence) ##########################

hist_map_data$prev_oerwd_w_cut <- cut(hist_map_data$prev_oerwd_w,
                                      breaks = c(0, 1, 2, 3, 4, 5),
                                      labels = c("(0, 1]","(1, 2]", "(2, 3]", "(3, 4]", "(4, 5]"),
                                      include.lowest = TRUE)

p4 <- ggplot() +
  geom_sf(data = hist_map_data, aes(fill = prev_oerwd_w_cut), color = "black") +
  scale_fill_manual(values = c("(0, 1]" = "#caf0f8", 
                               "(1, 2]" = "#90e0ef", 
                               "(2, 3]" = "#00b4d8", 
                               "(3, 4]" = "#0077b6",
                               "(4, 5]" = "#03045e"),
                    name = "Prevalence\nper 10,000",
                    drop = FALSE) +
  theme_void() +
  geom_sf(data = hist_map_data[hist_map_data$stat_sig == 1, ], 
          color = "red", fill = NA, size = 3)

########################## Combine All Maps – Figure 2 ##########################

# Figure 2. Prevalence by Census Division (NIS vs. OERWD)
plot_grid(
  p1 + guides(fill = "none") + 
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),
          plot.title = element_text(hjust = 0.5)) +
    geom_sf_label(data = data_labels, 
                  aes(label = censusdivision), 
                  color = "black",
                  fill = "#F5F5F5",
                  size = 2.3,
                  label.size = NA,
                  alpha = 0.7),
  NULL,
  p2 + theme(plot.margin = margin(0, 0, 0, 0, "pt"),
             plot.title = element_text(hjust = 0.5)),
  p3 + guides(fill = "none") + 
    theme(plot.margin = margin(0, 0, 0, 0, "pt"),
          plot.title = element_text(hjust = 0.5)),
  NULL,
  p4 + theme(plot.margin = margin(0, 0, 0, 0, "pt"),
             plot.title = element_text(hjust = 0.5)),
  ncol = 3,
  nrow = 2,
  align = "hv",
  labels = c("A.", "B.", "", "C.", "D.", ""),
  label_x = c(0, 0.9, NA, 0, 0.9, NA),
  rel_widths = c(1, -0.2, 1, 1, -0.2, 1),
  rel_heights = rep(1, 6)
)

# Save Figure 2
file_path <- "~/Desktop"
ggsave("prevmaps.jpg", path = file_path, width = 8, height = 5)
