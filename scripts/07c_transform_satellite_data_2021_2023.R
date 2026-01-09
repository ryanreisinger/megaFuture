#-----------------------------
# Add deltas to satellite layers
#-----------------------------

rm(list=ls())
setwd("E://")

{
  library(terra)
  library(lubridate)
}

#define cmip variable name
cmip_var <- "sos"

#define satellite variable name
satellite_var <- "sal"


# 1. Setup

#read in mean ssp126 and ssp585 delta for this variable
delta_126 <- rast(paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/ssp126/mean_ssp126_", cmip_var, "_delta.nc"))
delta_585 <- rast(paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/ssp585/mean_ssp585_", cmip_var, "_delta.nc"))

#list monthly satellite rasters
dir <- paste0("E:/Satellite_Data/monthly/", satellite_var)
satellite <- list.files(path=dir, pattern = "2023.nc")

#read in and combine all satellite rasters
for(i in satellite){
  
  #read in this file
  this_satellite <- rast(paste0(dir, "/", i))
  
  #join with other files
  if(i == satellite[1]){
    all_satellite <- this_satellite
  } else {
    all_satellite <- c(all_satellite, this_satellite)
  }
  
  #cleanup
  rm(this_satellite)
}
plot(all_satellite[[1]])

#project satellite data to cmip projection
all_satellite <- project(all_satellite, crs(delta_126))
plot(all_satellite[[1]])

#crop cmip extent to that of satellite data
delta_126 <- crop(delta_126, ext(all_satellite))
delta_585 <- crop(delta_585, ext(all_satellite))

#resample satellite data to cmip grid
all_satellite <- resample(all_satellite, delta_126, method="bilinear")

#sanity checks for units
plot(all_satellite[[1]])
plot(delta_585[[1]])

#for chlorophyll, transform units to match globcolour
if(cmip_var == "chlos"){
  delta_126 <- delta_126 * 1000
  delta_585 <- delta_585 * 1000
}

#for sea ice, convert cmip from percentage to fraction
if(cmip_var == "siconc"){
  delta_126 <- delta_126 / 100
  delta_585 <- delta_585 / 100
}

# 2. Transformations

#transform each month one by one
for(i in 1:12){
  
  #extract all satellite layers for that month
  monthly_satellite <- all_satellite[[month(time(all_satellite)) == i]]
  
  #extract the deltas for that month
  monthly_126 <- delta_126[[month(time(delta_126)) == i]]
  monthly_585 <- delta_585[[month(time(delta_585)) == i]]
  
  #add the deltas to the monthly satellite data
  transformed_126 <- monthly_satellite + monthly_126
  transformed_585 <- monthly_satellite + monthly_585
  
  #combine transformed datasets to all months
  if(i == 1){
    all_transformed_126 <- transformed_126
    all_transformed_585 <- transformed_585
  } else {
    all_transformed_126 <- c(all_transformed_126, transformed_126)
    all_transformed_585 <- c(all_transformed_585, transformed_585)
  }
}

#sanity check
plot(all_transformed_585[[1]])
plot(all_transformed_126[[1]])
plot(all_satellite[[1]])

# 3. Export

#export rasampled satellite data
writeCDF(all_satellite, paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/satellite_data/monthly_original_2123.nc"), overwrite = T)

#export transformed satellite data
writeCDF(all_transformed_126, paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/satellite_data/monthly_transformed_ssp126_2123.nc"), overwrite = T)
writeCDF(all_transformed_585, paste0("E:/cmip6_data/CMIP6/deltas/", cmip_var, "/satellite_data/monthly_transformed_ssp585_2123.nc"), overwrite = T)
