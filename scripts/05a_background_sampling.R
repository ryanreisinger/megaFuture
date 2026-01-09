#create background samples for humpback whales
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(sf)
  library(terra)
  library(dplyr)
  library(lubridate)
}


rm(list=ls())

#read in oceans file and metadata
oceans <- readRDS("data/oceans_vect.RDS")
meta <- read.csv("data/metadata.csv")

#create bounding box from -40 to -80 S
bbox <- ext(-180, 180, -80, -40) %>%
  vect()
crs(bbox) <- "epsg:4326"

# create mask from intersect of oceans and the bounding box
mask <- terra::intersect(oceans, bbox)

#visualise
plot(mask)

# read in tracks
tracks <- readRDS("data/all_tracks.RDS")

# plot to check
plot(tracks %>% vect(geom = c("x", "y"), crs = "epsg:4326"), pch = ".", add = T)

#create background points
back <- spatSample(mask, size = nrow(tracks))
plot(back, pch = ".", add = T, col = "red3")

#convert to dataframe
back <- as.data.frame(back, geom = "XY")
back <- back %>% select(x, y)

#link with a tracking ID and date
back <- back %>% 
  mutate(individual_id = tracks$individual_id,
         date = tracks$date,
         region = tracks$region)

#export 
saveRDS(back, file = "output/background/background_samples.RDS")
