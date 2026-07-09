#-------------------------------------------------------------------------------
# Changes in core habitat area by each population's feeding grounds
#-------------------------------------------------------------------------------

# SET EAST PACIFIC TO NOT INCLUDE WEDDELL SEA

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(tidyterra)

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
m1 <- matrix(c(-Inf, threshold, 0,
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
  
  # crop rasters to this population's feeding grounds
  if(this_pop != "Pacific"){
    
    present_pop <- crop(pred, ext(c(min_x, max_x, -80, -40)))
    proj_126_pop <- crop(proj_126, ext(c(min_x, max_x, -80, -40)))
    proj_585_pop <- crop(proj_585, ext(c(min_x, max_x, -80, -40)))
    
  } else {
    
    # Pacific population crosses the dateline, so need to crop in two parts and merge
    present_pop_1 <- crop(pred, ext(c(min_x, 180, -80, -40)))
    present_pop_2 <- crop(pred, ext(c(-180, max_x, -80, -40)))
    present_pop <- merge(present_pop_1, present_pop_2)
    
    proj_126_pop_1 <- crop(proj_126, ext(c(min_x, 180, -80, -40)))
    proj_126_pop_2 <- crop(proj_126, ext(c(-180, max_x, -80, -40)))
    proj_126_pop <- merge(proj_126_pop_1, proj_126_pop_2)
    
    proj_585_pop_1 <- crop(proj_585, ext(c(min_x, 180, -80, -40)))
    proj_585_pop_2 <- crop(proj_585, ext(c(-180, max_x, -80, -40)))
    proj_585_pop <- merge(proj_585_pop_1, proj_585_pop_2)
  }
  
  # calculate area of core habitat per month (in km^2)
  pred_area <- cellSize(present_pop, unit = "km") * present_pop
  pred_area_sum <- global(pred_area, sum, na.rm=TRUE)
  pred_area_sum$date <- time(present_pop)
  
  proj_126_area <- cellSize(proj_126_pop, unit = "km") * proj_126_pop
  proj_126_area_sum <- global(proj_126_area, sum, na.rm=TRUE)
  proj_126_area_sum$date <- time(proj_126_pop)
  
  proj_585_area <- cellSize(proj_585_pop, unit = "km") * proj_585_pop
  proj_585_area_sum <- global(proj_585_area, sum, na.rm=TRUE)
  proj_585_area_sum$date <- time(proj_585_pop)
  
  # calculate mean and standard deviation of future scenarios
  area_126 <- proj_126_area_sum %>%
    group_by(month = month(date)) %>%
    summarise(mean_126 = mean(sum, na.rm=TRUE),
              sd_126 = sd(sum, na.rm=TRUE)) %>%
    mutate(upper_126 = mean_126 + sd_126,
           lower_126 = mean_126 - sd_126)
  
  area_585 <- proj_585_area_sum %>%
    group_by(month = month(date)) %>%
    summarise(mean_585 = mean(sum, na.rm=TRUE),
              sd_585 = sd(sum, na.rm=TRUE)) %>%
    mutate(upper_585 = mean_585 + sd_585,
           lower_585 = mean_585 - sd_585)
  
  # convert present data to similar format
  area_present <- pred_area_sum %>%
    group_by(month = month(date)) %>%
    summarise(present = mean(sum, na.rm=TRUE))
  
  # join all together
  df <- area_present %>%
    left_join(area_126, by="month") %>%
    left_join(area_585, by="month") %>%
    mutate(region = this_pop)
  
  # join to all populations
  if(this_pop == pops[1]){
    data <- df
  } else {
    data <- bind_rows(data, df)
  }
}

# export data frame for plotting
saveRDS(data, "output/imagery/area_change/area_change_by_population.RDS")
data <- readRDS("output/imagery/area_change/area_change_by_population.RDS")


#-------------------------------------------------------------------------------
# 3. Plot Changes in Core Habitat Area by Population
#-------------------------------------------------------------------------------

# format month so that 10 becomes 1, 11 becomes 2, 12 becomes 3, 1 becomes 4, etc.
data <- data %>%
  mutate(month = case_when(
    month == 10 ~ 1,
    month == 11 ~ 2,
    month == 12 ~ 3,
    TRUE ~ month + 3
  ))

ggplot(data, aes(x=month)) +
  geom_ribbon(aes(ymin=lower_126, ymax=upper_126), fill="#56B4E9", alpha=0.3) +
  geom_line(aes(y=mean_126, color="SSP126"), linewidth = .75) +
  geom_ribbon(aes(ymin=lower_585, ymax=upper_585), fill="#E69F00", alpha=0.3) +
  geom_line(aes(y=mean_585, color="SSP585"), linewidth = .75) +
  geom_line(aes(y=present, color="Present"), linewidth = .75) +
  scale_color_manual(name="Scenario",
                     values=c("Present"="black", "SSP126"="#56B4E9", "SSP585"="#E69F00")) +
  labs(x="Month", y="Core Habitat Area (km²)") +
  facet_wrap(~region, scales = "free_y") +
  theme_custom() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 1:10, 
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
