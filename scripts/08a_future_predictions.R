#-----------------------------
# Predict future circumpolar habitat by month
#-----------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(caret)
  library(ranger)
  library(terra)
  library(tidyterra)
}

#define scenario
this.scenario <- "ssp585"

# 1. Read in monthly variables and predict each regional model

#read in monthly dynamic variables
sst <- rast(paste0("E:/cmip6_data/CMIP6/deltas/tos/satellite_data/monthly_transformed_", this.scenario, ".nc"))
ssh <- rast(paste0("E:/cmip6_data/CMIP6/deltas/zos/satellite_data/monthly_transformed_", this.scenario, ".nc"))
sal <- rast(paste0("E:/cmip6_data/CMIP6/deltas/sos/satellite_data/monthly_transformed_", this.scenario, ".nc"))
mld <- rast(paste0("E:/cmip6_data/CMIP6/deltas/mlotst/satellite_data/monthly_transformed_", this.scenario, ".nc"))
sic <- rast(paste0("E:/cmip6_data/CMIP6/deltas/siconc/satellite_data/monthly_transformed_", this.scenario, ".nc"))

#create stack
dynamic <- c(sst, ssh, sal, mld, sic)

#limit to target years and months
min_year <- 2001
max_year <- 2020
months <- c(10, 11, 12, 1, 2, 3, 4, 5)
dynamic <- dynamic[[month(time(dynamic)) %in% months & 
                      year(time(dynamic)) >= min_year & 
                      year(time(dynamic)) <= max_year]]

#cleanup
rm(sst, ssh, sal, mld, sic)

#read in static variables
depth <- rast("E:/Satellite_Data/static/depth/depth.nc")
dshelf <- rast("E:/Satellite_Data/static/dshelf/dshelf_resampled.nc")
slope <- rast("E:/Satellite_Data/static/slope/slope.nc")

#create stack
static <- c(depth, dshelf, slope)
names(static) <- c("depth", "dshelf", "slope")

#cleanup
rm(depth, dshelf, slope)

#resample static variables to dynamic resolution, crs, and extent
static <- resample(static, dynamic, method = "bilinear", threads = T)

#crop to target area
e <- ext(-180, 180, -80, -40)
static <- crop(static, e)
dynamic <- crop(dynamic, e)

#cleanup
rm(min_year, max_year, months)


# 2. Make circumpolar prediction map for each month

#read in random forests for each population and the circumpolar model
wa_rf <- readRDS("output/random_forests/WestAtlantic_rf.RDS")
wi_rf <- readRDS("output/random_forests/WestIndian_rf.RDS")
wp_rf <- readRDS("output/random_forests/WestPacific_rf.RDS")
p_rf <- readRDS("output/random_forests/Pacific_rf.RDS")
ea_rf <- readRDS("output/random_forests/EastAtlantic_rf.RDS")
ei_rf <- readRDS("output/random_forests/EastIndian_rf.RDS")
ep_rf <- readRDS("output/random_forests/EastPacific_rf.RDS")
circumpolar_rf <- readRDS("output/random_forests/circumpolar_rf.RDS")

#extract list of every foraging month from 2001-2020
nmonths <- n_distinct(time(dynamic))
months <- time(dynamic)[1:nmonths] 
months <- as_date(months)

#loop over each month
for(i in months){
  this.month <- as_date(i)
  
  #extract dynamic variables for this month
  stack <- dynamic[[time(dynamic) == this.month]]
  names(stack) <- c("sst", "ssh", "sal", "mld", "sic")
  
  #change sic NAs to 0s (to allow prediction outside sea ice area)
  stack$sic[is.na(stack$sic)] <- 0
  
  #crop to target area
  stack <- crop(stack, e)
  
  #create month raster layer
  month <- rast(ext = ext(stack), crs = crs(stack), res = res(stack))
  values(month) <- month(this.month)
  names(month) <- "month"
  
  #integrate static variables and month with raster stack
  stack <- c(stack, static, month)
  
  #predict each model to raster stack
  ea <- predict(stack, ea_rf, type="prob", na.rm = T)$presence
  names(ea) <- "EastAtlantic"
  
  ei <- predict(stack, ei_rf, type="prob", na.rm = T)$presence
  names(ei) <- "EastIndian"
  
  ep <- predict(stack, ep_rf, type="prob", na.rm = T)$presence
  names(ep) <- "EastPacific"
  
  p <- predict(stack, p_rf, type="prob", na.rm = T)$presence
  names(p) <- "Pacific"
  
  wa <- predict(stack, wa_rf, type="prob", na.rm = T)$presence
  names(wa) <- "WestAtlantic"
  
  wi <- predict(stack, wi_rf, type="prob", na.rm = T)$presence
  names(wi) <- "WestIndian"
  
  wp <- predict(stack, wp_rf, type="prob", na.rm = T)$presence
  names(wp) <- "WestPacific"
  
  #combine predictions with raster stack
  stack <- c(stack, ea, ei, ep, p, wa, wi, wp)
  rm(ea, ei, ep, p, wa, wi, wp)
  
  #predict circumpolar habitat
  circumpolar <- predict(stack, circumpolar_rf, type="prob", na.rm = T)$presence
  
  #assign time
  time(circumpolar) <- this.month
  
  #join circumpolar prediction to all other predictions
  if(i == months[1]){
    predictions <- circumpolar
  } else {
    predictions <- c(predictions, circumpolar)
  }
  
  #project to visualise
  ant_view <- project(circumpolar, "epsg:6932")
  g1 <- ggplot() + geom_spatraster(data=ant_view) + theme_void() + 
    scale_fill_viridis_c(na.value="white", name = "Habitat Suitability") + 
    ggtitle(paste0(month(this.month, label=T), " ", year(this.month))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  #save plot
  ggsave(plot = g1, width = 10, height = 8,
         filename = paste0("output/projections/", this.scenario, "/", month(this.month, label=T), "_", year(this.month), ".jpeg"))
  
  #print completion
  print(paste0(month(this.month, label=T), " ", year(this.month), " completed"))
}

#export
writeCDF(predictions, paste0("output/projections/", this.scenario, "/projections_", this.scenario, ".nc"))
