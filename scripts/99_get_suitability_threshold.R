#-------------------------------------------------------------------------------
# Calculate ensemble prediction for present day
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(tidyterra)
library(tidysdm)

# read in present day predictions
bart <- rast("output/predictions/bart_prediction.tif")
rf <- rast("output/predictions/rf_prediction.tif")
brt <- rast("output/predictions/brt_prediction.tif")

# stack
present <- c(bart, rf, brt)

# calculate monthly averages
for(this_month in c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)){
  print(this_month)
  month_layers <- present[[month(time(present)) == this_month]]
  month_mean <- app(month_layers, mean, na.rm = T)
  time(month_mean) <- as_date(paste0("2000-", sprintf("%02d", this_month), "-15"))
  if(this_month == 10){
    present_monthly <- month_mean
  } else {
    present_monthly <- c(present_monthly, month_mean)
  }
}

# plot present suitability
plot(present_monthly)

# export suitability
writeRaster(present_monthly, "output/predictions/ensemble_prediction.tif", overwrite = T)

#-------------------------------------------------------------------------------
# Get habitat suitability threshold
#-------------------------------------------------------------------------------

# read in presences and background samples
tracks <- readRDS("output/thinned_tracks/thinned_tracks.RDS")
bg <- readRDS("output/background/background_samples.RDS")

# combine
data <- rbind(
  tracks %>% select(x, y, date) %>% mutate(presence = 1),
  bg %>% select(x, y, date) %>% mutate(presence = 0)
)

# convert to terra
data <- vect(data, geom = c("x", "y"), crs = "epsg:4326")

# crop to prediction extent
data <- crop(data, ext(present_monthly))

# extract values for each month
for(this_month in c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)){
  print(this_month)
  month_data <- data %>% filter(month(date) == this_month)
  month_pred <- present_monthly[[month(time(present_monthly)) == this_month]]
  month_data$suitability <- terra::extract(month_pred, month_data, ID = F)
  if(this_month == 10){
    all_data <- month_data
  } else {
    all_data <- rbind(all_data, month_data)
  }
}

# find the max TSS value 
all_data <- all_data %>%
  as.data.frame() %>%
  mutate(presence = as.factor(presence)) %>%
  drop_na()
threshold <- optim_thresh(all_data$presence, all_data$suitability, 
                          metric = "tss_max", event_level = "second")

# export threshold
saveRDS(threshold, "output/predictions/suitability_threshold.RDS")