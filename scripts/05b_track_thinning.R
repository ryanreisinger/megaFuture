# thin tracks of humpback whales
rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(GeoThinneR)

# read in tracks
tracks <- readRDS("data/all_tracks.RDS")

# plot tracks
tracks %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  project("epsg:6932") %>%
  plot(pch = ".")

# limit to one point per individual per day
thinned <- tracks %>%
  group_by(individual_id, as_date(date)) %>%
  slice_sample(n = 1) %>%
  ungroup()

# read in depth raster to spatially thin data to the same grid
depth <- rast("~/OneDrive - University of Southampton/Documents/Predictor Data/processing/dShelf/depth.nc")

# thin spatially to one point per grid cell
quick_thin <- thin_points(
  data = thinned,
  lon_col = "lon",
  lat_col = "lat",
  method = "grid",
  raster_obj = depth
)

# get thinned data
thinned <- largest(quick_thin)

# plot
thinned %>%
  vect(geom = c("lon", "lat"), crs = "epsg:4326") %>%
  project("epsg:6932") %>%
  plot(pch = ".")

# export
saveRDS(thinned, "output/thinned_tracks/thinned_tracks.RDS")
