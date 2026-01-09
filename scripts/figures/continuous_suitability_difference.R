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
months <- c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)

# for each month, calculate the mean projected suitability
for(this_month in months){
  print(this_month)
  
  # limit projections to this month
  month126 <- ssp126_stack[[month(time(ssp126_stack)) == this_month]]
  month585 <- ssp585_stack[[month(time(ssp585_stack)) == this_month]]
  
  # average projections for this month
  mean126 <- app(month126, mean, na.rm = T)
  mean585 <- app(month585, mean, na.rm = T)
  
  # calculate difference from present day
  diff126 <- mean126 - present[[month(time(present)) == this_month]]
  diff585 <- mean585 - present[[month(time(present)) == this_month]]
  
  # join mean and difference rasters to all months
  if(this_month == months[1]){
    all_mean126 <- mean126
    all_mean585 <- mean585
    all_diff126 <- diff126
    all_diff585 <- diff585
  } else {
    all_mean126 <- c(all_mean126, mean126)
    all_mean585 <- c(all_mean585, mean585)
    all_diff126 <- c(all_diff126, diff126)
    all_diff585 <- c(all_diff585, diff585)
  }
  
}

# plot example months of mean and difference
# December - month 3
pres_dec <- present[[3]]
mean126_dec <- all_mean126[[3]]
mean585_dec <- all_mean585[[3]]
diff126_dec <- all_diff126[[3]]
diff585_dec <- all_diff585[[3]]

# March - month 6
pres_mar <- present[[6]]
mean126_mar <- all_mean126[[6]]
mean585_mar <- all_mean585[[6]]
diff126_mar <- all_diff126[[6]]
diff585_mar <- all_diff585[[6]]

# June - month 9
pres_jun <- present[[9]]
mean126_jun <- all_mean126[[9]]
mean585_jun <- all_mean585[[9]]
diff126_jun <- all_diff126[[9]]
diff585_jun <- all_diff585[[9]]

# plot differences in projected form
diff585_dec <- project(diff585_dec, "epsg:6932")
ggplot() +
  geom_spatraster(data = diff585_dec) +
  scale_fill_gradient2()

diff585_mar <- project(diff585_mar, "epsg:6932")
ggplot() +
  geom_spatraster(data = diff585_mar) +
  scale_fill_gradient2()

diff585_jun <- project(diff585_jun, "epsg:6932")
ggplot() +
  geom_spatraster(data = diff585_jun) +
  scale_fill_gradient2()
