#RESTART WEST ATLANTIC FROM BACKGROUND SALINITY

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}

#read in metadata
meta <- read_csv("data/metadata.csv")

#define list of regions
regions <- meta %>% 
  mutate(region = as.factor(region)) %>%
  pull(region)
regions <- levels(regions)

regions <- regions[5] 

#loop over regions
for(i in regions){
  
  #define region
  this.region <- i
  
  #read in tracks and back samples
  tracks <- readRDS(paste0("data/tracks_by_region/", this.region, "_mpm_6.RDS"))
  back <- readRDS(paste0("output/background/", this.region, "_background.RDS"))
  
  #filter tracks to below 40 degrees south
  tracks <- tracks %>% 
    filter(y < -40)
  
  #load in extraction functions
  source("code/extraction_functions.R")
  
  #---------------
  #Static Variables
  
  ###Depth###
  depth <- rast("D:/Satellite_Data/static/depth/depth.nc")
  
  #create SpatVector for tracks and back
  tracks <- vect(tracks,
                 geom=c("x", "y"),
                 crs=crs(depth)) #this ensures crs are the same as rasters
  back <- vect(back,
               geom=c("x", "y"),
               crs=crs(depth))
  
  #extract
  tracks$depth <- extract(depth, tracks, ID=F)
  back$depth <- extract(depth, back, ID=F)
  
  #remove rows where depth is NA - will be NA for every GLORYS variable
  tracks <- tracks %>% drop_na(depth)
  back <- back %>% drop_na(depth)
  
  
  ###Slope###
  slope <- rast("D:/Satellite_Data/static/slope/slope.nc")
  tracks$slope <- extract(slope, tracks, ID=F)
  back$slope <- extract(slope, back, ID=F)
  
  ###dShelf### - does this work beyond 40 south?
  dshelf <- rast("D:/Satellite_Data/static/dshelf/dshelf_resampled.nc")
  tracks$dshelf <- extract(dshelf, tracks, ID=F)
  back$dshelf <- extract(dshelf, back, ID=F)
  
  #cleanup static
  rm(depth, slope, dshelf)
  
  
  #---------------
  #Dynamic Variables
  
  ###SST###
  tracks <- dynamic_extract(predictor = "sst", tracks)
  back <- dynamic_extract(predictor = "sst", back)
  
  ###MLD###
  tracks <- dynamic_extract(predictor = "mld", tracks)
  back <- dynamic_extract(predictor = "mld", back)
  
  ###SAL###
  tracks <- dynamic_extract(predictor = "sal", tracks)
  back <- dynamic_extract(predictor = "sal", back)
  
  ###SSH###
  tracks <- dynamic_extract(predictor = "ssh", tracks)
  back <- dynamic_extract(predictor = "ssh", back)
  
  ###SIC###
  tracks <- dynamic_extract(predictor = "sic", tracks)
  tracks$sic[is.na(tracks$sic)] <- 0 #SIC values of 0 print as NA in GLORYS
  back <- dynamic_extract(predictor = "sic", back)
  back$sic[is.na(back$sic)] <- 0
  
  ###CURR###
  tracks <- dynamic_extract(predictor = "uo", tracks) #uo is eastward velocity
  back <- dynamic_extract(predictor = "uo", back) 
  
  tracks <- dynamic_extract(predictor = "vo", tracks) #vo is northwards velocity
  back <- dynamic_extract(predictor = "vo", back)
  
  tracks$curr <- sqrt((tracks$uo^2) + (tracks$vo^2)) #current speed
  back$curr <- sqrt((back$uo^2) + (back$vo^2))
  
  ###CHL###
  #uses different function for resampled files
  tracks <- dynamic_chlorophyll(predictor = "chl", tracks)
  back <- dynamic_chlorophyll(predictor = "chl", back)
  
  ###WIND###
  #uses different function for monthly file structure
  tracks <- dynamic_wind(predictor = "wind", tracks = tracks, direction = "east")
  tracks <- dynamic_wind(predictor = "wind", tracks = tracks, direction = "north")
  tracks$wind <- sqrt(tracks$wind_east^2 + tracks$wind_north^2)
  
  back <- dynamic_wind(predictor = "wind", back, direction = "east")
  back <- dynamic_wind(predictor = "wind", back, direction = "north")
  back$wind <- sqrt(back$wind_east^2 + back$wind_north^2)
  
  #---------------
  #Export
  plot(tracks, pch=".")
  tracks <- as.data.frame(tracks, geom="XY")
  
  plot(back, pch=".")
  back <- as.data.frame(back, geom="XY")
  
  saveRDS(tracks, 
          file=paste0("output/extraction/", this.region, "_presences_extracted.RDS"))
  
  saveRDS(back, 
          file=paste0("output/extraction/", this.region, "_background_extracted.RDS"))
  
}
