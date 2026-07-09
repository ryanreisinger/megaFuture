#-------------------------------------------------------------------------------
# Plot Humpback Habitat Suitability Changes Between Models
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)

# read in present day ensemble
present <- rast("output/predictions/ensemble_prediction.tif")
plot(present)

# read in all SSP126 gcm predictions
access126 <- rast("output/projections/ssp126/ACCESS-ESM1-5/ensemble_prediction.tif")
can126 <- rast("output/projections/ssp126/CanESM5/ensemble_prediction.tif")
cesm126 <- rast("output/projections/ssp126/CESM2-WACCM/ensemble_prediction.tif")
hadgem126 <- rast("output/projections/ssp126/HadGEM3-GC31-LL/ensemble_prediction.tif")
ipsl126 <- rast("output/projections/ssp126/IPSL-CM6A-LR/ensemble_prediction.tif")
mri126 <- rast("output/projections/ssp126/MRI-ESM2-0/ensemble_prediction.tif")
nor126 <- rast("output/projections/ssp126/NorESM2-MM/ensemble_prediction.tif")
ukesm126 <- rast("output/projections/ssp126/UKESM1-0-LL/ensemble_prediction.tif")

# stack all SSP126 models
ssp126_stack <- c(access126, can126, cesm126, hadgem126, ipsl126, mri126, nor126, ukesm126)

# read in all SSP585 gcm predictions
access585 <- rast("output/projections/ssp585/ACCESS-ESM1-5/ensemble_prediction.tif")
can585 <- rast("output/projections/ssp585/CanESM5/ensemble_prediction.tif")
cesm585 <- rast("output/projections/ssp585/CESM2-WACCM/ensemble_prediction.tif")
hadgem585 <- rast("output/projections/ssp585/HadGEM3-GC31-LL/ensemble_prediction.tif")
ipsl585 <- rast("output/projections/ssp585/IPSL-CM6A-LR/ensemble_prediction.tif")
mri585 <- rast("output/projections/ssp585/MRI-ESM2-0/ensemble_prediction.tif")
nor585 <- rast("output/projections/ssp585/NorESM2-MM/ensemble_prediction.tif")
ukesm585 <- rast("output/projections/ssp585/UKESM1-0-LL/ensemble_prediction.tif")

# stack all SSP585 models
ssp585_stack <- c(access585, can585, cesm585, hadgem585, ipsl585, mri585, nor585, ukesm585)

# desired months
months <- c(10:12, 1:7)

# for each month, calculate the mean projected suitability
for(this_month in months){
  print(this_month)
  
  # limit present day to this month
  present_month <- present[[month(time(present)) == this_month]]
  
  # limit projections to this month
  month126 <- ssp126_stack[[month(time(ssp126_stack)) == this_month]]
  month585 <- ssp585_stack[[month(time(ssp585_stack)) == this_month]]
  
  # average projections for this month
  mean126 <- app(month126, mean, na.rm = T)
  mean585 <- app(month585, mean, na.rm = T)
  
  # calculate difference from present day
  diff126 <- mean126 - present_month
  diff585 <- mean585 - present_month
  
  # project 
  present_month <- project(present_month, "epsg:6932")
  mean126 <- project(mean126, "epsg:6932")
  mean585 <- project(mean585, "epsg:6932")
  diff126 <- project(diff126, "epsg:6932")
  diff585 <- project(diff585, "epsg:6932")
  
  # get max values for plotting
  max_val <- max(minmax(present_month)[2,], 
                 minmax(mean126)[2,],
                 minmax(mean585)[2,])
  diff_max <- max(abs(minmax(diff126)[1,]), abs(minmax(diff126)[2,]),
                  abs(minmax(diff585)[1,]), abs(minmax(diff585)[2,])) %>%
    abs()
  
  # load in coastline
  coast <- readRDS("data/coast_ice_vect.RDS")
  
  # create plots
  p1 <- ggplot() +
    geom_spatraster(data = present_month) +
    scale_fill_gradient(low = "#152A3B", high = "#ddeaf2", na.value = "white",
                        guide = "none", limits = c(0, max_val)) +
    geom_spatvector(data = coast, fill = "white", col = "white") +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    ggtitle("Present (2000-2020)") 
  
  p2 <- ggplot() +
    geom_spatraster(data = mean126) +
    scale_fill_gradient(low = "#152A3B", high = "#ddeaf2", na.value = "white",
                        guide = "none", limits = c(0, max_val)) +
    geom_spatvector(data = coast, fill = "white", col = "white") +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    ggtitle("SSP126 (2080-2100)")
  
  p3 <- ggplot() +
    geom_spatraster(data = mean585) +
    scale_fill_gradient(low = "#152A3B", high = "#ddeaf2", na.value = "white",
                        guide = "none", limits = c(0, max_val)) +
    geom_spatvector(data = coast, fill = "white", col = "white") +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    ggtitle("SSP585 (2080-2100)")
  
  p4 <- ggplot() +
    geom_spatraster(data = diff126) +
    scale_fill_gradient2(low = "#67001F", mid = "grey90", high = "#053061", na.value = "white",
                         guide = "none", limits = c(-diff_max, diff_max)) +
    geom_spatvector(data = coast, fill = "grey40", col = "grey40") +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    ggtitle("Difference (SSP126 - Present)")
  
  p5 <- ggplot() +
    geom_spatraster(data = diff585) +
    scale_fill_gradient2(low = "#67001F", mid = "grey90", high = "#053061", na.value = "white",
                         guide = "none", limits = c(-diff_max, diff_max)) +
    geom_spatvector(data = coast, fill = "grey40", col = "grey40") +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA),
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    ggtitle("Difference (SSP585 - Present)")
  
  # make legend plots for the different scales
  legend_suitability <- ggplot() +
    geom_spatraster(data = present_month) +
    scale_fill_gradient(low = "#152A3B", high = "#ddeaf2", na.value = "white",
                        name = "Habitat Suitability",
                        limits = c(0, max_val)) +
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)) +
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
  legend_suitability <- get_legend(legend_suitability)
  
  legend_difference <- ggplot() +
    geom_spatraster(data = diff126) +
    scale_fill_gradient2(low = "#67001F", mid = "grey90", high = "#053061", na.value = "white",
                         name = "Change in Habitat Suitability",
                         limits = c(-diff_max, diff_max)) +
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)) +
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
  legend_difference <- get_legend(legend_difference)
  
  # create grid
  row1 <- plot_grid(p1, legend_suitability, legend_difference, 
                    ncol = 3, rel_widths = c(1, 0.5, 0.5))
  row2 <- plot_grid(p2, p4, ncol = 2)
  row3 <- plot_grid(p3, p5, ncol = 2)
  grid <- plot_grid(row1, row2, row3, ncol = 1, align = "v")
  grid + ggview::canvas(width = 10, height = 15)
  
  # export
  ggsave(paste0("output/imagery/supplementary/monthly_plots/grid_", this_month, ".png"),
         grid, width = 10, height = 15, dpi = 300)
  
}
