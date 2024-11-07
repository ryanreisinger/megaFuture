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

#get a list of all regions
regions <- meta %>%
  mutate(region = as.factor(region)) %>%
  pull(region)
levels(regions)

#define region
this.region <- "EastAtlantic"

#read in tracks
tracks <- readRDS(paste0("data/tracks_by_region/", this.region, "_mpm_6.RDS"))

#limit to below 40 degrees South
tracks <- tracks %>%
  filter(y < -40)

#convert to terra
tracks_terra <- tracks %>%
  vect(geom = c("x", "y"),
       crs = "epsg:4326")

#visualise
plot(tracks_terra, pch =".")

#create minimum convex hull
mch <- convHull(tracks_terra)
plot(mch, add=T)

#buffer to prevent self intersection error
mch <- buffer(mch, 0)

#intersect mch with oceans to avoid sampling on land
mask <- terra::intersect(mch, oceans)
plot(mask)

#create background points
back <- spatSample(mask, size = nrow(tracks))
plot(back, pch = ".", add = T)

#convert to dataframe
back <- as.data.frame(back, geom = "XY")
back <- back %>% select(x, y)

#link with a tracking ID and date
back <- back %>% 
  mutate(individual_id = tracks$individual_id,
         date = tracks$date,
         region = this.region)

#export 
saveRDS(back, file = paste0("output/background/", this.region, "_background.RDS"))
