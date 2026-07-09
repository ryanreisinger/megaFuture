#-------------------------------------------------------------------------------
# Habitat degredation within current corridors of each population
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
# 1. Calculate mean habitat suitability within current corridors
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

# read in longitudinal limits of each population's corridor
long_lims <- readRDS("output/populations/longitudinal_limits.RDS")

# for each populations
populations <- unique(long_lims$region)
for(this_pop in populations){
  print(this_pop)
  
  # get longitudinal limits for this population
  min_x <- long_lims %>%
    filter(region == this_pop) %>%
    pull(min_x)
  max_x <- long_lims %>%
    filter(region == this_pop) %>%
    pull(max_x)
  
  # create spatvector of corridor
  if(this_pop != "Pacific"){
    e <- ext(min_x, max_x, -80, -40)
    pop_corridor <- as.polygons(e)
    crs(pop_corridor) <- "epsg:4326"
  } else {
    # Pacific population crosses the dateline
    e1 <- ext(min_x, 180, -80, -40)
    e2 <- ext(-180, max_x, -80, -40)
    pop_corridor1 <- as.polygons(e1)
    pop_corridor2 <- as.polygons(e2)
    crs(pop_corridor1) <- "epsg:4326"
    crs(pop_corridor2) <- "epsg:4326"
    pop_corridor <- combineGeoms(pop_corridor1, pop_corridor2)
  }
  
  # for each month
  for(this_month in c(10:12, 1:7)){
    
    # extract mean suitability within polygon 
    present <- extract(pred[[month(time(pred)) == this_month]], pop_corridor, 
                       fun = mean, ID = F, na.rm = TRUE) %>%
      as.numeric()
    
    # extract mean suitability for SSP126
    future_126 <- extract(proj_126[[month(time(proj_126)) == this_month]], 
                          pop_corridor, fun = mean, ID = F, na.rm = TRUE) %>%
      as.numeric()
    
    # extract mean suitability for SSP585
    future_585 <- extract(proj_585[[month(time(proj_585)) == this_month]], 
                          pop_corridor, fun = mean, ID = F, na.rm = TRUE) %>%
      as.numeric()
    
    # combine into dataframe
    df <- data.frame(
      pop = this_pop,
      month = this_month,
      present = present,
      mean_126 = mean(future_126, na.rm = TRUE),
      mean_585 = mean(future_585, na.rm = TRUE),
      sd_126 = sd(future_126, na.rm = TRUE),
      sd_585 = sd(future_585, na.rm = TRUE)
    )
    
    # join to all other months
    if(this_month == 10){
      data <- df
    } else {
      data <- rbind(data, df)
    }
  }
  
  # combine with all populations
  if(this_pop == populations[1]){
    all_data <- data
  } else {
    all_data <- rbind(all_data, data)
  }
}

# calculate differences between present and future
all_data <- all_data %>%
  mutate(
    diff_126 = mean_126 - present,
    diff_585 = mean_585 - present
  )

# save data for reuse
saveRDS(all_data, "output/imagery/heatmap/habitat_degredation_long_data.RDS")
all_data <- readRDS("output/imagery/heatmap/habitat_degredation_long_data.RDS")


#-------------------------------------------------------------------------------
# 2. Plot
#-------------------------------------------------------------------------------

# change all_data so October is first
all_data <- all_data %>%
  mutate(month = case_when(
    month %in% 10:12 ~ month - 9,
    TRUE ~ month + 3
  ))

# organise populations into factor levels
all_data <- all_data %>%
  mutate(pop = factor(pop, levels = c("WestAtlantic", "EastAtlantic",
                                      "WestIndian", "EastIndian",
                                      "WestPacific", "Pacific", "EastPacific"))) %>%
  mutate(pop = recode(pop,
                      "WestAtlantic" = "A - West Atlantic",
                      "EastAtlantic" = "B - East Atlantic",
                      "WestIndian" = "C - West Indian",
                      "EastIndian" = "D - East Indian",
                      "WestPacific" = "E - West Pacific",
                      "Pacific" = "F - Central Pacific",
                      "EastPacific" = "G - East Pacific"))

# get min and max fill values from diff_585 and diff_126
min_fill <- min(c(all_data$diff_126, all_data$diff_585), na.rm = TRUE)
max_fill <- max(c(all_data$diff_126, all_data$diff_585), na.rm = TRUE)
abs_max <- max(abs(min_fill), abs(max_fill))
abs_max <- round(abs_max, 1)

p1 <- ggplot(all_data, aes(x = month, y = pop)) + 
  geom_tile(aes(fill = diff_585), color = NA) +
  scale_fill_gradient2(
    low = "darkred", mid = "white", high = "steelblue4",
    midpoint = 0, limits = c(-abs_max, abs_max),
    name = "Change in\nHabitat Suitability"
  ) + 
  scale_y_discrete(expand = c(0,0), limits = rev) +
  scale_x_continuous(breaks = 1:10,
                     expand = c(0,0),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb",
                                "Mar", "Apr", "May", "Jun", "Jul")) +
  labs(x = "", y = "") +
  theme_custom() +
  theme(axis.ticks = element_blank(),
        text = element_text(face = "bold"))
  # geom_vline(xintercept = 3.5, linetype = "dashed", color = "black") +
  # geom_vline(xintercept = 8.5, linetype = "dashed", color = "black")
p1 + ggview::canvas(width = 10, height = 5)

p2 <- ggplot(all_data, aes(x = month, y = pop)) + 
  geom_tile(aes(fill = diff_126), color = NA) +
  scale_fill_gradient2(
    low = "darkred", mid = "white", high = "steelblue4",
    midpoint = 0, limits = c(-abs_max, abs_max),
    name = "Change in\nHabitat Suitability"
  ) + 
  scale_y_discrete(expand = c(0,0), limits = rev) +
  scale_x_continuous(breaks = 1:10,
                     expand = c(0,0),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb",
                                "Mar", "Apr", "May", "Jun", "Jul")) +
  labs(x = "", y = "") +
  theme_custom() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(face = "bold"))
  # geom_vline(xintercept = 3.5, linetype = "dashed", color = "black") +
  # geom_vline(xintercept = 8.5, linetype = "dashed", color = "black")
p2 + ggview::canvas(width = 10, height = 5)

# plot together
library(cowplot)
grid <- plot_grid(p2, p1, ncol = 1, align = "v", scale = 0.925)
grid + ggview::canvas(width = 10, height = 10)

# export
ggsave("output/imagery/heatmap/habitat_degredation_long.png", grid,
       width = 10, height = 10, units = "in", dpi = 300)


# plot standard deviation
# get max sd
max_sd <- max(c(all_data$sd_126, all_data$sd_585), na.rm = TRUE)
ggplot(all_data, aes(x = month, y = pop)) + 
  geom_tile(aes(fill = sd_585), color = NA) +
  scale_fill_viridis_c(
    name = "Standard Deviation\nin Future Projections",
    limits = c(0, max_sd)
  ) + 
  scale_y_discrete(expand = c(0,0), limits = rev) +
  scale_x_continuous(breaks = 1:10,
                     expand = c(0,0),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb",
                                "Mar", "Apr", "May", "Jun", "Jul")) +
  labs(x = "", y = "") +
  theme_custom() 
  # geom_vline(xintercept = 3.5, linetype = "dashed", color = "black") +
  # geom_vline(xintercept = 8.5, linetype = "dashed", color = "black")

ggplot(all_data, aes(x = month, y = pop)) + 
  geom_tile(aes(fill = sd_126), color = NA) +
  scale_fill_viridis_c(
    name = "Standard Deviation\nin Future Projections",
    limits = c(0, max_sd)
  ) + 
  scale_y_discrete(expand = c(0,0), limits = rev) +
  scale_x_continuous(breaks = 1:10,
                     expand = c(0,0),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb",
                                "Mar", "Apr", "May", "Jun", "Jul")) +
  labs(x = "", y = "") +
  theme_custom() 
  # geom_vline(xintercept = 3.5, linetype = "dashed", color = "black") +
  # geom_vline(xintercept = 8.5, linetype = "dashed", color = "black")
