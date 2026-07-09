#-------------------------------------------------------------------------------
# Plot monthly latitudinal shifts and GCM variability
#-------------------------------------------------------------------------------

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

# create a latitude raster using the y-coordinates
lat_raster <- init(pred, fun = "y")

#-------------------------------------------------------------------------------
# 2. Compute differences by latitude
#-------------------------------------------------------------------------------

# loop over each month from October to July
for(this_month in c(10:12, 1:7)) {
  
  print(paste("Processing month:", this_month))
  
  # limit present and future predictions to this month
  present <- pred[[month(time(pred)) == this_month]]
  future_126 <- proj_126[[month(time(proj_126)) == this_month]]
  future_585 <- proj_585[[month(time(proj_585)) == this_month]]
  
  # stack rasters together
  suit_stack <- c(present, future_126, future_585, lat_raster)
  
  # assign names
  names(suit_stack) <- c("Present",
                         "126_1", "126_2", "126_3", "126_4", "126_5", "126_6", "126_7", "126_8", 
                         "585_1", "585_2", "585_3", "585_4", "585_5", "585_6", "585_7", "585_8",
                         "Latitude")
  
  # convert to a dataframe with all values
  df <- as.data.frame(suit_stack, xy = TRUE, na.rm = F)
  
  # define latitude bands (every 2 degrees)
  df <- df %>%
    mutate(lat_band = cut(Latitude, breaks = seq(min(df$y), max(df$y), by = 2), include.lowest = TRUE))
  
  # summarise by latitude band
  summary <- df %>%
    group_by(lat_band) %>%
    summarise(
      lat_mid = mean(Latitude, na.rm = TRUE),
      mean_present = mean(Present, na.rm = TRUE),
      mean_126_1 = mean(`126_1`, na.rm = TRUE),
      mean_126_2 = mean(`126_2`, na.rm = TRUE),
      mean_126_3 = mean(`126_3`, na.rm = TRUE),
      mean_126_4 = mean(`126_4`, na.rm = TRUE),
      mean_126_5 = mean(`126_5`, na.rm = TRUE),
      mean_126_6 = mean(`126_6`, na.rm = TRUE),
      mean_126_7 = mean(`126_7`, na.rm = TRUE),
      mean_126_8 = mean(`126_8`, na.rm = TRUE),
      mean_585_1 = mean(`585_1`, na.rm = TRUE),
      mean_585_2 = mean(`585_2`, na.rm = TRUE),
      mean_585_3 = mean(`585_3`, na.rm = TRUE),
      mean_585_4 = mean(`585_4`, na.rm = TRUE),
      mean_585_5 = mean(`585_5`, na.rm = TRUE),
      mean_585_6 = mean(`585_6`, na.rm = TRUE),
      mean_585_7 = mean(`585_7`, na.rm = TRUE),
      mean_585_8 = mean(`585_8`, na.rm = TRUE)
    )
  
  # calculate mean and sd across GCMs
  summary <- summary %>%
    mutate(
      mean_ssp126 = rowMeans(select(., starts_with("mean_126_")), na.rm = TRUE),
      sd_ssp126 = apply(select(., starts_with("mean_126_")), 1, sd, na.rm = TRUE),
      mean_ssp585 = rowMeans(select(., starts_with("mean_585_")), na.rm = TRUE),
      sd_ssp585 = apply(select(., starts_with("mean_585_")), 1, sd, na.rm = TRUE),
      diff_126 = mean_ssp126 - mean_present,
      diff_585 = mean_ssp585 - mean_present
    ) 
  
  # append month
  summary <- summary %>%
    mutate(month = this_month)
  
  # join to all other summaries
  if(this_month == 10) {
    all_summaries <- summary
  } else {
    all_summaries <- bind_rows(all_summaries, summary)
  }
  
}

# create ribbon values for plotting (mean + sd)
all_summaries <- all_summaries %>%
  mutate(
    ssp126_upper = mean_ssp126 + sd_ssp126,
    ssp126_lower = mean_ssp126 - sd_ssp126,
    ssp585_upper = mean_ssp585 + sd_ssp585,
    ssp585_lower = mean_ssp585 - sd_ssp585
  )

# remove NAs
all_summaries <- all_summaries %>%
  drop_na()

# change month numbers to month names
all_summaries <- all_summaries %>%
  mutate(month = factor(month.abb[month], levels = month.abb[c(10:12, 1:7)]))

# export data frame for plotting
#saveRDS(all_summaries, "output/imagery/lat_shifts/figdata.RDS")
all_summaries <- readRDS("output/imagery/lat_shifts/figdata.RDS")

p1 <- ggplot(all_summaries, aes(x = lat_mid)) +
  facet_wrap(~month, ncol = 3) +
  geom_ribbon(aes(ymin = ssp126_lower, ymax = ssp126_upper), fill = "#56B4E9", alpha = 0.3) +
  geom_line(aes(y = mean_ssp126, color = "SSP126"), linewidth = .75) +
  geom_ribbon(aes(ymin = ssp585_lower, ymax = ssp585_upper), fill = "#E69F00", alpha = 0.3) +
  geom_line(aes(y = mean_ssp585, color = "SSP585"), linewidth = .75) +
  geom_line(aes(y = mean_present, color = "Present"), linewidth = .75) +
  scale_color_manual(values = c("Present" = "black", "SSP126" = "#56B4E9", "SSP585" = "#E69F00")) +
  labs(x = "Latitude (°)", y = "Mean Habitat Suitability", color = "Scenario") +
  theme_custom() +
  theme(
    strip.text = element_text(size = 12),
    panel.background  = element_rect(fill = "transparent", colour = NA),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    axis.title.y = element_text(angle = 0)
  ) +
  scale_x_continuous(expand = c(0,0), breaks = seq(-75, -40, by = 5))
p1 + ggview::canvas(width = 10, height = 12)

ggsave("output/imagery/lat_shifts/latitudinal_shifts_lines.png",
       width = 10, height = 12, units = "in", bg = "transparent", dpi = 300)

# presentation version to replace any black elements with light grey
p2 <- p1 +
  scale_color_manual(values = c("Present" = "#ddeaf2", "SSP126" = "#56B4E9", "SSP585" = "#E69F00")) +
  theme(
    axis.text = element_text(color = "grey90"),
    axis.title.x = element_text(color = "grey90"),
    axis.title.y = element_text(angle = 0, color = "grey90"),
    strip.text = element_text(color = "grey90"),
    legend.text = element_text(color = "grey90"),
    legend.title = element_text(color = "grey90"),
    panel.grid.major = element_line(colour = "grey35", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey90", fill = NA, linewidth = 0.5)
  )
p2 + ggview::canvas(width = 12, height = 8, bg = "#152A3B")

# export
ggsave("~/OneDrive - University of Southampton/Documents/Conferences/UKIRSC 2026/lat_shift.png",
       p2, width = 12, height = 8, dpi = 300)


# latitudinal centroid shifts for results reporting
with(all_summaries, 
     sum(lat_mid * mean_present, na.rm = TRUE) / sum(mean_present, na.rm = TRUE))
centroid_mean_shifts <- all_summaries %>%
  group_by(month) %>%
  summarise(centroid_present = sum(lat_mid * mean_present, na.rm = TRUE) / sum(mean_present, na.rm = TRUE),
            centroid_126 = sum(lat_mid * mean_ssp126, na.rm = TRUE) / sum(mean_ssp126, na.rm = TRUE),
            centroid_585 = sum(lat_mid * mean_ssp585, na.rm = TRUE) / sum(mean_ssp585, na.rm = TRUE)) %>%
  mutate(diff_126 = centroid_126 - centroid_present,
         diff_585 = centroid_585 - centroid_present)

# compute centroid shifts for each GCM
centroid_sd <- all_summaries %>%
  group_by(month) %>%
  summarise(across(starts_with("mean_126_"), ~ sum(lat_mid * .x, na.rm = TRUE) / sum(.x, na.rm = TRUE), .names = "centroid_{.col}"),
            across(starts_with("mean_585_"), ~ sum(lat_mid * .x, na.rm = TRUE) / sum(.x, na.rm = TRUE), .names = "centroid_{.col}")) %>%
  pivot_longer(cols = starts_with("centroid_"), names_to = c(".value", "gcm"), names_pattern = "centroid_mean_(\\d+)_(\\d+)") %>%
  group_by(month) %>%
  summarise(sd_126 = sd(`126`, na.rm = TRUE),
            sd_585 = sd(`585`, na.rm = TRUE))

# join together
centroid_shifts <- centroid_mean_shifts %>%
  left_join(centroid_sd, by = "month") 

# mean from October to December
centroid_shifts %>%
  filter(month %in% c("Oct", "Nov", "Dec")) %>%
  summarise(present = mean(centroid_present, na.rm = TRUE),
            mean_diff_126 = mean(diff_126, na.rm = TRUE),
            mean_diff_585 = mean(diff_585, na.rm = TRUE),
            sd_diff_126 = mean(sd_126, na.rm = TRUE),
            sd_diff_585 = mean(sd_585, na.rm = TRUE))

# mean from January to April
centroid_shifts %>%
  filter(month %in% c("Jan", "Feb", "Mar", "Apr")) %>%
  summarise(present = mean(centroid_present, na.rm = TRUE),
            mean_diff_126 = mean(diff_126, na.rm = TRUE),
            mean_diff_585 = mean(diff_585, na.rm = TRUE),
            sd_diff_126 = mean(sd_126, na.rm = TRUE),
            sd_diff_585 = mean(sd_585, na.rm = TRUE))

# mean from May to July
centroid_shifts %>%
  filter(month %in% c("May", "Jun", "Jul")) %>%
  summarise(present = mean(centroid_present, na.rm = TRUE),
            mean_diff_126 = mean(diff_126, na.rm = TRUE),
            mean_diff_585 = mean(diff_585, na.rm = TRUE),
            sd_diff_126 = mean(sd_126, na.rm = TRUE),
            sd_diff_585 = mean(sd_585, na.rm = TRUE))


#-------------------------------------------------------------------------------
# 3. Compute Population-Level Differences by Latitude within Longitudinal Bins
#-------------------------------------------------------------------------------

# read in longitudinal limit dataframe
lims <- readRDS("output/populations/longitudinal_limits.RDS")
  
# list populations
pops <- unique(lims$region)

# loop over each population
for(this_pop in pops){
  print(this_pop)
  
  # identify longitudinal limits for this population
  min_x <- lims %>%
    filter(region == this_pop) %>%
    pull(min_x)
  max_x <- lims %>%
    filter(region == this_pop) %>%
    pull(max_x)
  
  # crop rasters to these limits
  if(this_pop != "Pacific"){
    
    present_pop <- crop(pred, ext(c(min_x, max_x, -80, -40)))
    proj_126_pop <- crop(proj_126, ext(c(min_x, max_x, -80, -40)))
    proj_585_pop <- crop(proj_585, ext(c(min_x, max_x, -80, -40)))
    lat_pop <- crop(lat_raster, ext(c(min_x, max_x, -80, -40)))
    
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
    
    lat_pop_1 <- crop(lat_raster, ext(c(min_x, 180, -80, -40)))
    lat_pop_2 <- crop(lat_raster, ext(c(-180, max_x, -80, -40)))
    lat_pop <- merge(lat_pop_1, lat_pop_2)
  }
  
  # loop over each month from October to July
  for(this_month in c(10:12, 1:7)) {
    
    print(paste("Processing month:", this_month))
    
    # limit present and future predictions to this month
    present <- present_pop[[month(time(present_pop)) == this_month]]
    future_126 <- proj_126_pop[[month(time(proj_126_pop)) == this_month]]
    future_585 <- proj_585_pop[[month(time(proj_585_pop)) == this_month]]
    
    # stack rasters together
    suit_stack <- c(present, future_126, future_585, lat_pop)
    
    # assign names
    names(suit_stack) <- c("Present",
                           "126_1", "126_2", "126_3", "126_4", "126_5", "126_6", "126_7", "126_8", 
                           "585_1", "585_2", "585_3", "585_4", "585_5", "585_6", "585_7", "585_8",
                           "Latitude")
    
    # convert to a dataframe with all values
    df <- as.data.frame(suit_stack, xy = TRUE, na.rm = F)
    
    # define latitude bands (every 2 degrees)
    df <- df %>%
      mutate(lat_band = cut(Latitude, breaks = seq(min(df$y), max(df$y), by = 2), include.lowest = TRUE))
    
    # summarise by latitude band
    summary <- df %>%
      group_by(lat_band) %>%
      summarise(
        lat_mid = mean(Latitude, na.rm = TRUE),
        mean_present = mean(Present, na.rm = TRUE),
        mean_126_1 = mean(`126_1`, na.rm = TRUE),
        mean_126_2 = mean(`126_2`, na.rm = TRUE),
        mean_126_3 = mean(`126_3`, na.rm = TRUE),
        mean_126_4 = mean(`126_4`, na.rm = TRUE),
        mean_126_5 = mean(`126_5`, na.rm = TRUE),
        mean_126_6 = mean(`126_6`, na.rm = TRUE),
        mean_126_7 = mean(`126_7`, na.rm = TRUE),
        mean_126_8 = mean(`126_8`, na.rm = TRUE),
        mean_585_1 = mean(`585_1`, na.rm = TRUE),
        mean_585_2 = mean(`585_2`, na.rm = TRUE),
        mean_585_3 = mean(`585_3`, na.rm = TRUE),
        mean_585_4 = mean(`585_4`, na.rm = TRUE),
        mean_585_5 = mean(`585_5`, na.rm = TRUE),
        mean_585_6 = mean(`585_6`, na.rm = TRUE),
        mean_585_7 = mean(`585_7`, na.rm = TRUE),
        mean_585_8 = mean(`585_8`, na.rm = TRUE)
      )
    
    # calculate mean and sd across GCMs
    summary <- summary %>%
      mutate(
        mean_ssp126 = rowMeans(select(., starts_with("mean_126_")), na.rm = TRUE),
        sd_ssp126 = apply(select(., starts_with("mean_126_")), 1, sd, na.rm = TRUE),
        mean_ssp585 = rowMeans(select(., starts_with("mean_585_")), na.rm = TRUE),
        sd_ssp585 = apply(select(., starts_with("mean_585_")), 1, sd, na.rm = TRUE),
        diff_126 = mean_ssp126 - mean_present,
        diff_585 = mean_ssp585 - mean_present
      ) 
    
    # append month
    summary <- summary %>%
      mutate(month = this_month)
    
    # join to all other summaries
    if(this_month == 10) {
      all_summaries <- summary
    } else {
      all_summaries <- bind_rows(all_summaries, summary)
    }
    
  }
  
  # create ribbon values for plotting (mean + sd)
  all_summaries <- all_summaries %>%
    mutate(
      ssp126_upper = mean_ssp126 + sd_ssp126,
      ssp126_lower = mean_ssp126 - sd_ssp126,
      ssp585_upper = mean_ssp585 + sd_ssp585,
      ssp585_lower = mean_ssp585 - sd_ssp585
    )
  
  # remove NAs
  all_summaries <- all_summaries %>%
    drop_na()
  
  # change month numbers to month names
  all_summaries <- all_summaries %>%
    mutate(month = factor(month.abb[month], levels = month.abb[c(10:12, 1:7)]))
  
  # export data frame for plotting
  saveRDS(all_summaries, paste0("output/imagery/lat_shifts/", this_pop, "_figdata.RDS"))
  all_summaries <- readRDS(paste0("output/imagery/lat_shifts/", this_pop, "_figdata.RDS"))
  
  p1 <- ggplot(all_summaries, aes(x = lat_mid)) +
    facet_wrap(~month, ncol = 5) +
    geom_ribbon(aes(ymin = ssp126_lower, ymax = ssp126_upper), fill = "#56B4E9", alpha = 0.3) +
    geom_line(aes(y = mean_ssp126, color = "SSP126"), linewidth = .75) +
    geom_ribbon(aes(ymin = ssp585_lower, ymax = ssp585_upper), fill = "#E69F00", alpha = 0.3) +
    geom_line(aes(y = mean_ssp585, color = "SSP585"), linewidth = .75) +
    geom_line(aes(y = mean_present, color = "Present"), linewidth = .75) +
    scale_color_manual(values = c("Present" = "black", "SSP126" = "#56B4E9", "SSP585" = "#E69F00")) +
    labs(x = "Latitude (°)", y = "Mean Habitat Suitability", color = "Scenario") +
    theme_custom() +
    theme(
      panel.background  = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    ) +
    scale_x_continuous(expand = c(0,0), breaks = seq(-75, -40, by = 5)) +
    ggtitle(this_pop)
  p1 + ggview::canvas(width = 12, height = 8)
  
  # export
  ggsave(paste0("output/imagery/lat_shifts/latitudinal_shifts_lines_", this_pop, ".png"),
         p1, width = 12, height = 8, dpi = 300)
}
