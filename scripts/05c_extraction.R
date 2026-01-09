# extract environmental variables
rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}

#read in metadata
meta <- read_csv("data/metadata.csv")

#read in thinned tracks and background samples
tracks <- readRDS(paste0("output/thinned_tracks/thinned_tracks.RDS"))
back <- readRDS(paste0("output/background/background_samples.RDS"))

#filter tracks to below 40 degrees south
tracks <- tracks %>% 
  filter(y < -40)

# add presence-background column
tracks <- tracks %>%
  mutate(pb = "presence")
back <- back %>%
  mutate(pb = "background")

# select the same columns in both dataframes
cols <- c("x", "y", "individual_id", "date", "region", "pb")
tracks <- tracks %>%
  select(all_of(cols))
back <- back %>%
  select(all_of(cols))

# combine the dataframes
data <- bind_rows(tracks, back)

#load in extraction functions
source("code/functions/dynamic_extract.R")

#---------------
#Static Variables

###Depth###
depth <- rast("E:/Satellite_Data/static/depth/depth.nc")

#create SpatVector for data
data <- vect(data,
               geom=c("x", "y"),
               crs=crs(depth)) #this ensures crs are the same as rasters

#extract
data$depth <- extract(depth, data, ID=F)

#remove rows where depth is NA - will be NA for every GLORYS variable
data <- data %>% drop_na(depth)

###Slope###
slope <- rast("E:/Satellite_Data/static/slope/slope.nc")
data$slope <- extract(slope, data, ID=F)

###dShelf### 
dshelf <- rast("E:/Satellite_Data/static/dshelf/dshelf_resampled.nc")
data$dshelf <- extract(dshelf, data, ID=F)

#cleanup static vars
rm(depth, slope, dshelf)


#---------------
#Dynamic Variables

###SST###
data <- dynamic_extract(predictor = "sst", data, crop = F)
print("sst")

###MLD###
data <- dynamic_extract(predictor = "mld", data, crop = F)
print("mld")

###SAL###
data <- dynamic_extract(predictor = "sal", data, crop = F)
print("sal")

###SIC###
data <- dynamic_extract(predictor = "sic", data, crop = F)
data$sic[is.na(data$sic)] <- 0 #SIC values of 0 print as NA in GLORYS
print("sic")

###CURR###
data <- dynamic_extract(predictor = "uo", data, crop = F) #uo is eastward velocity
data <- dynamic_extract(predictor = "vo", data, crop = F) #vo is northwards velocity
data$curr <- sqrt((data$uo^2) + (data$vo^2)) #current speed calculation
print("curr")

#---------------
#Export
plot(data, pch=".")
data <- as.data.frame(data, geom="XY")

saveRDS(data, 
        file="output/extraction/extracted.RDS")
