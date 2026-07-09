#-------------------------------------------------------------------------------
# Fig. 3 - Present Day Suitability
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(tidyterra)

# read in prediction
pred <- rast("output/predictions/ensemble_prediction.tif")

# project to antarctic stereographic view
pred <- project(pred, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# max value for plotting
max_val <- minmax(pred)[2,] %>%
  max()

# plot function
pred_plot <- function(month){
  
  # create plot
  p1 <- ggplot() +
    geom_spatraster(data = pred[[month(time(pred)) == month]]) +
    scale_fill_gradient(low = "#152A3B", high = "#ddeaf2", na.value = "#152A3B",
                        guide = "none", limits = c(0, max_val)) +
    geom_spatvector(data = coast, fill = "white", col = "white") +
    theme_void() +
    theme(plot.background = element_rect(fill = NA, color = NA))
  
  # export plot
  ggsave(paste0("output/imagery/predictions/fin/habitat_suitability_", month, ".png"),
         p1, width = 10, height = 10, dpi = 300)
  
  # return month to signify completion
  return(month)
  
}

# create plot for each month
for(i in c(10:12, 1:7)){
  pred_plot(i)
}
