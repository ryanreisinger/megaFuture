#-------------------------------------------------------------------------------
# Centroid shifts in core habitat by population
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(tidyterra)
library(geosphere)
library(circular)

# Get consistent theme
source("code/scripts/99_theme_and_fig_size.R")


#-------------------------------------------------------------------------------
# 1. Data Prep
#-------------------------------------------------------------------------------

# load monthly predictions
pred <- rast("output/predictions/ensemble_prediction.tif")

# load SSP126 projections
files_126 <- list.files("output/projections/ssp126/", pattern = ".tif$", 
                        recursive = TRUE, full.names = TRUE)
proj_126 <- lapply(files_126, rast)
proj_126 <- rast(proj_126)
proj_126

# load SSP585 projections
files_585 <- list.files("output/projections/ssp585/", pattern = ".tif$", 
                        recursive = TRUE, full.names = TRUE)
proj_585 <- lapply(files_585, rast)
proj_585 <- rast(proj_585)
proj_585

# read in habitat suitability threshold
threshold <- readRDS("output/predictions/suitability_threshold.rds")

#-------------------------------------------------------------------------------
# 2. Compute Core Habitat Area per Month
#-------------------------------------------------------------------------------

# binarise predictions based on threshold
m1 <- matrix(c(-Inf, threshold, NA,
               threshold, Inf, 1), 
             ncol=3, byrow=TRUE)
pred <- classify(pred, m1)
proj_126 <- classify(proj_126, m1)
proj_585 <- classify(proj_585, m1)

# read in longitudinal limit dataframe
lims <- readRDS("output/populations/longitudinal_limits.RDS")

# list populations
pops <- unique(lims$region)

# loop over each population
for(this_pop in pops){
  print(this_pop)
  
  # get longitudinal limits for this population
  min_x <- lims %>%
    filter(region == this_pop) %>%
    pull(min_x)
  max_x <- lims %>%
    filter(region == this_pop) %>%
    pull(max_x)
  
  # if Pacific population, need to account for dateline
  if(this_pop == "Pacific"){
    min_x <- -180
  }
  
  # crop rasters to this population's feeding grounds
  present_pop <- crop(pred, ext(c(min_x, max_x, -80, -40)))
  proj_126_pop <- crop(proj_126, ext(c(min_x, max_x, -80, -40)))
  proj_585_pop <- crop(proj_585, ext(c(min_x, max_x, -80, -40)))
  
  # centroids for present day
  for(i in 1:nlyr(pred)){
    
    # get polygons for this layer
    polys <- as.polygons(present_pop[[i]])
    
    # compute centroid
    cents <- centroids(polys)
    
    # append month
    cents <- cents %>%
      mutate(month = month(time(present_pop[[i]])))
    
    # combine with other months
    if(i == 1){
      present_centroids <- cents
    } else {
      present_centroids <- bind_spat_rows(present_centroids, cents)
    }
  }
  
  # centroids for ssp126
  for(i in 1:nlyr(proj_126_pop)){
    
    # get polygons for this layer
    polys <- as.polygons(proj_126_pop[[i]])
    
    # compute centroid
    cents <- centroids(polys)
    
    # append month
    cents <- cents %>%
      mutate(month = month(time(proj_126_pop[[i]])))
    
    # combine with other months
    if(i == 1){
      proj_126_centroids <- cents
    } else {
      proj_126_centroids <- bind_spat_rows(proj_126_centroids, cents)
    }
  }
  
  # centroids for ssp585
  for(i in 1:nlyr(proj_585_pop)){
    
    # get polygons for this layer
    polys <- as.polygons(proj_585_pop[[i]])
    
    # compute centroid
    cents <- centroids(polys)
    
    # append month
    cents <- cents %>%
      mutate(month = month(time(proj_585_pop[[i]])))
    
    # combine with other months
    if(i == 1){
      proj_585_centroids <- cents
    } else {
      proj_585_centroids <- bind_spat_rows(proj_585_centroids, cents)
    }
  }
  
  # for each month, calculate distance and bearing to future centroids
  for(this_month in c(10:12, 1:7)){
    print(this_month)
    
    # isolate present centroid
    present_cent <- present_centroids %>%
      filter(month == this_month)
    
    # isolate ssp126 centroids
    cent_126 <- proj_126_centroids %>%
      filter(month == this_month)
    
    # isolate ssp585 centroids
    cent_585 <- proj_585_centroids %>%
      filter(month == this_month)
    
    coast <- rnaturalearth::ne_countries(scale = 10, returnclass = "sv")
    p1 <- ggplot() +
      geom_spatvector(data = coast %>% crop(ext(min_x, max_x, -80, -45)), fill="lightgrey") +
      geom_spatvector(data = present_cent, col = "black") +
      geom_spatvector(data = cent_126, col = "blue") +
      geom_spatvector(data = cent_585, col = "red") 
    print(p1)
    
    # calculate distances of centroids
    dists_126 <- distance(present_cent, cent_126, unit = "km")
    dists_585 <- distance(present_cent, cent_585, unit = "km")
    
    # calculate bearings of centroids
    bearing_126 <- bearing(crds(present_cent), crds(cent_126))
    bearing_585 <- bearing(crds(present_cent), crds(cent_585))
    
    # adjust bearings to 0-360
    bearing_126 <- ifelse(bearing_126 < 0, bearing_126 + 360, bearing_126)
    bearing_585 <- ifelse(bearing_585 < 0, bearing_585 + 360, bearing_585)
    
    # combine into dataframe
    shifts <- data.frame(
      population = this_pop,
      month = this_month,
      ssp126_distance_km = mean(dists_126),
      ssp126_bearing_deg = mean(circular(bearing_126, units = "degrees", type = "angles")),
      ssp585_distance_km = mean(dists_585),
      ssp585_bearing_deg = mean(circular(bearing_585, units = "degrees", type = "angles"))
    )
    
    # re-adjust bearings to 0-360
    shifts <- shifts %>%
      mutate(
        ssp126_bearing_deg = ifelse(ssp126_bearing_deg < 0, ssp126_bearing_deg + 360, ssp126_bearing_deg),
        ssp585_bearing_deg = ifelse(ssp585_bearing_deg < 0, ssp585_bearing_deg + 360, ssp585_bearing_deg)
      )
    
    # append to other months
    if(this_month == 10){
      all_shifts <- shifts
    } else {
      all_shifts <- rbind(all_shifts, shifts)
    }
  }
  
  # combine to all populations
  if(this_pop == pops[1]){
    centroid_shifts <- all_shifts
  } else {
    centroid_shifts <- rbind(centroid_shifts, all_shifts)
  }
}

# export data for future use
saveRDS(centroid_shifts, "output/imagery/centroids/centroid_shifts_by_population.RDS")
centroid_shifts <- readRDS("output/imagery/centroids/centroid_shifts_by_population.RDS")


#-------------------------------------------------------------------------------
# 3. Plot Centroid Shifts
#-------------------------------------------------------------------------------

# set up population structure
centroid_shifts <- centroid_shifts %>%
  mutate(population = factor(population, levels = c("WestAtlantic", "EastAtlantic",
                                                    "WestIndian", "EastIndian",
                                                    "WestPacific", "Pacific", "EastPacific"))) %>%
  mutate(population = recode(population,
                             "WestAtlantic" = "West Atlantic",
                             "EastAtlantic" = "East Atlantic",
                             "WestIndian" = "West Indian",
                             "EastIndian" = "East Indian",
                             "WestPacific" = "West Pacific",
                             "Pacific" = "Central Pacific",
                             "EastPacific" = "East Pacific"))

# coord_polar bar plots per month
rbreaks <- seq(100, 2000, 100)
ggplot(centroid_shifts %>% filter(month == 4), 
       aes(x = ssp585_bearing_deg, y = ssp585_distance_km)) +
  geom_bar(stat = "identity", width = 8, aes(fill = population), 
           alpha = 1, position = position_identity()) +
  coord_polar(theta = "x", direction = -1) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 45)) +
  #facet_wrap(~month) +
  theme_custom() +
  labs(x = "Bearing of Shift (°)", y = "Distance of Shift (km)") +
  scale_y_continuous(breaks = rbreaks, expand = c(0,0)) +
  scale_fill_manual(values = c("#F2CF91", "#DB941A",
                               "#00B4CC", "#007E8F",
                               "#E27865", "#DB5943", "#892B1A"),
                    name = "") +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "grey30")) +
  annotate("text", x = 0, y = 100, label = "100", color = "grey30", size = 3) +
  annotate("text", x = 0, y = 200, label = "200", color = "grey30", size = 3) +
  annotate("text", x = 0, y = 300, label = "300", color = "grey30", size = 3) +
  annotate("text", x = 0, y = 400, label = "400", color = "grey30", size = 3) +
  annotate("text", x = 0, y = 500, label = "500", color = "grey30", size = 3) 
  

# lollipop distance plots
centroid_shifts2 <- centroid_shifts %>%
  mutate(month = case_when(
    month == 10 ~ 1,
    month == 11 ~ 2,
    month == 12 ~ 3,
    TRUE ~ month + 3
  )) 

ggplot(centroid_shifts2, aes(x = factor(month), y = ssp585_distance_km, 
                             color = population, fill = population)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7),
           width = 0.25) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  theme_custom() +
  labs(x = "Month", y = "Distance of Shift (km)") +
  scale_y_continuous(breaks = seq(0, 1600, 200), limits = c(0, 1600),
                     expand = c(0,0)) +
  scale_x_discrete(labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", 
                              "May", "Jun", "Jul")) +
  theme(legend.position = "right") +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
  scale_fill_manual(values = c("#F2CF91", "#DB941A",
                               "#00B4CC", "#007E8F",
                               "#E27865", "#DB5943", "#892B1A"),
                    name = "") +
  scale_color_manual(values = c("#F2CF91", "#DB941A",
                                "#00B4CC", "#007E8F",
                                "#E27865", "#DB5943", "#892B1A"),
                     name = "") 

# average bearings and distances per population
avg_shifts <- centroid_shifts %>%
  group_by(population) %>%
  filter(month %in% c(12, 1:7)) %>%
  summarise(
    mean_ssp126_distance_km = mean(ssp126_distance_km),
    mean_ssp126_bearing_deg = mean(circular(ssp126_bearing_deg, units = "degrees", type = "angles")),
    mean_ssp585_distance_km = mean(ssp585_distance_km),
    mean_ssp585_bearing_deg = mean(circular(ssp585_bearing_deg, units = "degrees", type = "angles"))
  ) %>%
  ungroup() %>%
  mutate(
    mean_ssp126_bearing_deg = ifelse(mean_ssp126_bearing_deg < 0, mean_ssp126_bearing_deg + 360, mean_ssp126_bearing_deg),
    mean_ssp585_bearing_deg = ifelse(mean_ssp585_bearing_deg < 0, mean_ssp585_bearing_deg + 360, mean_ssp585_bearing_deg)
  )

# plot as coord_polar bar plot (reorder factor so largest shifts are below)
avg_shifts <- avg_shifts %>%
  arrange(-mean_ssp585_distance_km) %>%
  mutate(population = factor(population, levels = population))

p1 <- ggplot(avg_shifts, 
       aes(x = mean_ssp585_bearing_deg, y = mean_ssp585_distance_km)) +
  geom_bar(stat = "identity", width = 8, aes(fill = population), 
           alpha = 1, position = position_identity()) +
  coord_polar(theta = "x", direction = 1) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 315, by = 45),
                     labels = rep("", 8)) +
  #facet_wrap(~month) +
  theme_custom() +
  labs(x = "Bearing of Shift (°)", y = "Distance of Shift (km)") +
  scale_y_continuous(breaks = rbreaks, expand = c(0,0),  limits = c(0, 515)) +
  scale_fill_manual(values = c("#DB5943", "#F2CF91",
                               "#00B4CC", "#DB941A",
                               "#007E8F", "#892B1A", "#E27865"),
                    name = "", guide = "none") +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "grey30", size = 12),
        panel.grid.major = element_line(color = "grey70")) 
p1 + ggview::canvas(width = 10, height = 10)

# repeat for SSP126
avg_shifts <- avg_shifts %>% 
  mutate(mean_ssp126_bearing_deg = ifelse(population == "West Pacific", 
                                          134, mean_ssp126_bearing_deg))
p2 <- ggplot(avg_shifts, 
             aes(x = mean_ssp126_bearing_deg, y = mean_ssp126_distance_km)) +
  geom_bar(stat = "identity", width = 8, aes(fill = population), alpha = 1, 
           position = position_identity()) +
  coord_polar(theta = "x", direction = 1) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 315, by = 45),
                     labels = rep("", 8)) +
  #facet_wrap(~month) +
  theme_custom() +
  labs(x = "Bearing of Shift (°)", y = "Distance of Shift (km)") +
  scale_y_continuous(breaks = rbreaks, expand = c(0,0), limits = c(0, 320)) +
  scale_fill_manual(values = c("#DB5943", "#F2CF91",
                               "#00B4CC", "#DB941A",
                               "#007E8F", "#892B1A", "#E27865"),
                    name = "", guide = "none") +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "grey30", size = 12),
         panel.grid.major = element_line(color = "grey70"))  
p2 + ggview::canvas(width = 10, height = 10)

# export
ggsave("output/imagery/centroids/centroid_shift_polar_ssp585.png", 
       plot = p1,
       width = 10, height = 10, units = "in", dpi = 300)
ggsave("output/imagery/centroids/centroid_shift_polar_ssp126.png", 
       plot = p2,
       width = 10, height = 10, units = "in", dpi = 300)

# average but over seasons instead this time (Oct - Dec, Jan - Apr, May - Jul)
seasonal_shifts <- centroid_shifts %>%
  mutate(season = case_when(
    month %in% c(10, 11, 12) ~ "Oct-Dec",
    month %in% c(1, 2, 3, 4) ~ "Jan-Apr",
    month %in% c(5, 6, 7) ~ "May-Jul"
  )) %>%
  group_by(population, season) %>%
  summarise(
    mean_ssp126_distance_km = mean(ssp126_distance_km),
    mean_ssp126_bearing_deg = mean(circular(ssp126_bearing_deg, units = "degrees", type = "angles")),
    mean_ssp585_distance_km = mean(ssp585_distance_km),
    mean_ssp585_bearing_deg = mean(circular(ssp585_bearing_deg, units = "degrees", type = "angles"))
  ) %>%
  ungroup() %>%
  mutate(
    mean_ssp126_bearing_deg = ifelse(mean_ssp126_bearing_deg < 0, mean_ssp126_bearing_deg + 360, mean_ssp126_bearing_deg),
    mean_ssp585_bearing_deg = ifelse(mean_ssp585_bearing_deg < 0, mean_ssp585_bearing_deg + 360, mean_ssp585_bearing_deg)
  )

# # plot as coord_polar bar plot 
# p2 <- ggplot(seasonal_shifts %>% filter(season == "Oct-Dec"), 
#              aes(x = mean_ssp585_bearing_deg, y = mean_ssp585_distance_km)) +
#   geom_bar(stat = "identity", width = 8, aes(fill = population), 
#            alpha = 1, position = position_identity()) +
#   coord_polar(theta = "x", direction = 1) +
#   scale_x_continuous(limits = c(0, 360), breaks = seq(45, 315, by = 45)) +
#   theme_custom() +
#   labs(x = "Bearing of Shift (°)", y = "Distance of Shift (km)") +
#   scale_y_continuous(breaks = rbreaks, expand = c(0,0)) +
#   scale_fill_manual(values = c("#F2CF91", "#DB941A",
#                                "#00B4CC", "#007E8F",
#                                "#E27865", "#DB5943", "#892B1A"),
#                     name = "", guide = "none") +
#   theme(axis.ticks = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(color = "grey30", size = 12)) +
#   annotate("text", x = 0, y = 600, label = "600", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 200, label = "200", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 800, label = "800", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 400, label = "400", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 1000, label = "1000", color = "grey30", size = 5) 
# p2 + ggview::canvas(width = 10, height = 10)

# 
# p3 <- ggplot(seasonal_shifts %>% filter(season == "Jan-Apr"), 
#        aes(x = mean_ssp585_bearing_deg, y = mean_ssp585_distance_km)) +
#   geom_bar(stat = "identity", width = 8, aes(fill = population), 
#            alpha = 1, position = position_identity()) +
#   coord_polar(theta = "x", direction = 1) +
#   scale_x_continuous(limits = c(0, 360), breaks = seq(45, 315, by = 45)) +
#   theme_custom() +
#   labs(x = "Bearing of Shift (°)", y = "Distance of Shift (km)") +
#   scale_y_continuous(breaks = rbreaks, expand = c(0,0)) +
#   scale_fill_manual(values = c("#F2CF91", "#DB941A",
#                                "#00B4CC", "#007E8F",
#                                "#E27865", "#DB5943", "#892B1A"),
#                     name = "", guide = "none") +
#   theme(axis.ticks = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(color = "grey30", size = 12)) +
#   annotate("text", x = 0, y = 100, label = "100", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 200, label = "200", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 300, label = "300", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 400, label = "400", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 500, label = "500", color = "grey30", size = 5) 
# p3 + ggview::canvas(width = 10, height = 10)
# 
# 
# p4 <- ggplot(seasonal_shifts %>% filter(season == "May-Jul") %>%
#                arrange(-mean_ssp585_distance_km) %>%
#                mutate(population = factor(population, levels = population)),
#              aes(x = mean_ssp585_bearing_deg, y = mean_ssp585_distance_km)) +
#   geom_bar(stat = "identity", width = 8, aes(fill = population), 
#            alpha = 1, position = position_identity()) +
#   coord_polar(theta = "x", direction = 1) +
#   scale_x_continuous(limits = c(0, 360), breaks = seq(45, 315, by = 45)) +
#   theme_custom() +
#   labs(x = "Bearing of Shift (°)", y = "Distance of Shift (km)") +
#   scale_y_continuous(breaks = rbreaks, expand = c(0,0)) +
#   scale_fill_manual(values = c("#F2CF91", "#DB941A",
#                                "#00B4CC", "#007E8F",
#                                "#E27865", "#DB5943", "#892B1A"),
#                     name = "", guide = "none") +
#   theme(axis.ticks = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(color = "grey30", size = 12)) +
#   annotate("text", x = 0, y = 100, label = "100", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 200, label = "200", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 300, label = "300", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 400, label = "400", color = "grey30", size = 5) +
#   annotate("text", x = 0, y = 500, label = "500", color = "grey30", size = 5) 
# p4 + ggview::canvas(width = 10, height = 10)
