#-------------------------------------------------------------------------------
# Visualise Habitat Persistence and Arrival/Departure Months
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(tidyterra)

# read in present day predictions
present <- rast("output/predictions/ensemble_prediction.tif")
plot(present)

# read in habitat suitability threshold
threshold <- readRDS("output/predictions/suitability_threshold.rds")

# classify present day predictions as suitable/unsuitable
m1 <- matrix(c(-Inf, threshold, 0,
               threshold, Inf, 1), 
             ncol=3, byrow=TRUE)
present_bin <- classify(present, m1)

# get number of months that habitat is suitability (sum)
persistence <- app(present_bin, sum, na.rm = T)
plot(persistence)

# get months of arrival (first month habitat is suitable)
for(i in c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)){
  print(i)
  
  # isolate layer
  month_layer <- present_bin[[month(time(present_bin)) == i]]
  
  # revalue 0s to NA and 1s to month number
  month_layer[month_layer == 0] <- NA
  values(month_layer)[!is.na(values(month_layer))] <- i
  
  # bank the layer to mask other layers
  if(i == 10){
    arrival <- month_layer
  } else {
    arrival <- cover(arrival, month_layer)
  }
}

# change numbering so that October is 1, November is 2, ..., September is 12
values(arrival) <- ifelse(values(arrival) >= 10, 
                          values(arrival) - 9,
                          values(arrival) + 3)
plot(arrival)

# rename levels to month names
levels(arrival) <- data.frame(ID = 1:12,
                               month = c("October", "November", "December",
                                         "January", "February", "March",
                                         "April", "May", "June",
                                         "July", "August", "September"))
plot(arrival)

# get months of departure (last month habitat is suitable)
for(i in c(7, 6, 5, 4, 3, 2, 1, 12, 11, 10)){
  print(i)
  
  # isolate layer
  month_layer <- present_bin[[month(time(present_bin)) == i]]
  
  # revalue 0s to NA and 1s to month number
  month_layer[month_layer == 0] <- NA
  values(month_layer)[!is.na(values(month_layer))] <- i
  
  # bank the layer to mask other layers
  if(i == 7){
    departure <- month_layer
  } else {
    departure <- cover(departure, month_layer)
  }
}

# change numbering so that October is 1, November is 2, ..., September is 12
values(departure) <- ifelse(values(departure) >= 10, 
                          values(departure) - 9,
                          values(departure) + 3)
plot(departure)

# rename levels to month names
levels(departure) <- data.frame(ID = 1:12,
                             month = c("October", "November", "December",
                                       "January", "February", "March",
                                       "April", "May", "June",
                                       "July", "August", "September"))
plot(departure)

# save outputs
writeRaster(persistence, "output/imagery/persistence/habitat_persistence.tif", overwrite=TRUE)
writeRaster(arrival, "output/imagery/persistence/arrival_month.tif", overwrite=TRUE)
writeRaster(departure, "output/imagery/persistence/departure_month.tif", overwrite=TRUE)


#-------------------------------------------------------------------------------
# Repeat for SSP585
#-------------------------------------------------------------------------------

# clean up
rm(list=ls())

# list GCMs
gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL",
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")

# for each gcm
for(gcm in gcms[2:8]){
  print(gcm)
  
  # read in ssp585 predictions
  ssp585 <- rast(paste0("output/projections/ssp585/", gcm, "/ensemble_prediction.tif"))
  
  # classify ssp585 predictions as suitable/unsuitable
  threshold <- readRDS("output/predictions/suitability_threshold.rds")
  m1 <- matrix(c(-Inf, threshold, 0,
                 threshold, Inf, 1), 
               ncol=3, byrow=TRUE)
  ssp585_bin <- classify(ssp585, m1)
  
  # get number of months that habitat is suitability (sum)
  persistence <- app(ssp585_bin, sum, na.rm = T)
  plot(persistence)
  
  # get months of arrival (first month habitat is suitable)
  for(i in c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)){
    print(i)
    
    # isolate layer
    month_layer <- ssp585_bin[[month(time(ssp585_bin)) == i]]
    
    # revalue 0s to NA and 1s to month number
    month_layer[month_layer == 0] <- NA
    values(month_layer)[!is.na(values(month_layer))] <- i
    
    # bank the layer to mask other layers
    if(i == 10){
      arrival <- month_layer
    } else {
      arrival <- cover(arrival, month_layer)
    }
  }
  
  # change numbering so that October is 1, November is 2, ..., September is 12
  values(arrival) <- ifelse(values(arrival) >= 10, 
                            values(arrival) - 9,
                            values(arrival) + 3)
  
  # rename levels to month names
  levels(arrival) <- data.frame(ID = 1:12,
                                month = c("October", "November", "December",
                                          "January", "February", "March",
                                          "April", "May", "June",
                                          "July", "August", "September"))
  plot(arrival)
  
  # get months of departure (last month habitat is suitable)
  for(i in c(7, 6, 5, 4, 3, 2, 1, 12, 11, 10)){
    print(i)
    
    # isolate layer
    month_layer <- ssp585_bin[[month(time(ssp585_bin)) == i]]
    
    # revalue 0s to NA and 1s to month number
    month_layer[month_layer == 0] <- NA
    values(month_layer)[!is.na(values(month_layer))] <- i
    
    # bank the layer to mask other layers
    if(i == 7){
      departure <- month_layer
    } else {
      departure <- cover(departure, month_layer)
    }
  }
  
  # change numbering so that October is 1, November is 2, ..., September is 12
  values(departure) <- ifelse(values(departure) >= 10, 
                              values(departure) - 9,
                              values(departure) + 3)
  
  # rename levels to month names
  levels(departure) <- data.frame(ID = 1:12,
                                  month = c("October", "November", "December",
                                            "January", "February", "March",
                                            "April", "May", "June",
                                            "July", "August", "September"))
  plot(departure)
  
  # export
  writeRaster(persistence, paste0("output/imagery/persistence/ssp585_", gcm, "_habitat_persistence.tif"), overwrite=TRUE)
  writeRaster(arrival, paste0("output/imagery/persistence/ssp585_", gcm, "_arrival_month.tif"), overwrite=TRUE)
  writeRaster(departure, paste0("output/imagery/persistence/ssp585_", gcm, "_departure_month.tif"), overwrite=TRUE)
  
}


#-------------------------------------------------------------------------------
# Repeat for SSP126
#-------------------------------------------------------------------------------

# clean up
rm(list=ls())

# list GCMs
gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL",
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")

# for each gcm
for(gcm in gcms){
  print(gcm)
  
  # read in ssp126 predictions
  ssp126 <- rast(paste0("output/projections/ssp126/", gcm, "/ensemble_prediction.tif"))
  
  # classify ssp126 predictions as suitable/unsuitable
  threshold <- readRDS("output/predictions/suitability_threshold.rds")
  m1 <- matrix(c(-Inf, threshold, 0,
                 threshold, Inf, 1), 
               ncol=3, byrow=TRUE)
  ssp126_bin <- classify(ssp126, m1)
  
  # get number of months that habitat is suitability (sum)
  persistence <- app(ssp126_bin, sum, na.rm = T)
  plot(persistence)
  
  # get months of arrival (first month habitat is suitable)
  for(i in c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)){
    print(i)
    
    # isolate layer
    month_layer <- ssp126_bin[[month(time(ssp126_bin)) == i]]
    
    # revalue 0s to NA and 1s to month number
    month_layer[month_layer == 0] <- NA
    values(month_layer)[!is.na(values(month_layer))] <- i
    
    # bank the layer to mask other layers
    if(i == 10){
      arrival <- month_layer
    } else {
      arrival <- cover(arrival, month_layer)
    }
  }
  
  # change numbering so that October is 1, November is 2, ..., September is 12
  values(arrival) <- ifelse(values(arrival) >= 10, 
                            values(arrival) - 9,
                            values(arrival) + 3)
  
  # rename levels to month names
  levels(arrival) <- data.frame(ID = 1:12,
                                month = c("October", "November", "December",
                                          "January", "February", "March",
                                          "April", "May", "June",
                                          "July", "August", "September"))
  plot(arrival)
  
  # get months of departure (last month habitat is suitable)
  for(i in c(7, 6, 5, 4, 3, 2, 1, 12, 11, 10)){
    print(i)
    
    # isolate layer
    month_layer <- ssp126_bin[[month(time(ssp126_bin)) == i]]
    
    # revalue 0s to NA and 1s to month number
    month_layer[month_layer == 0] <- NA
    values(month_layer)[!is.na(values(month_layer))] <- i
    
    # bank the layer to mask other layers
    if(i == 7){
      departure <- month_layer
    } else {
      departure <- cover(departure, month_layer)
    }
  }
  
  # change numbering so that October is 1, November is 2, ..., September is 12
  values(departure) <- ifelse(values(departure) >= 10, 
                              values(departure) - 9,
                              values(departure) + 3)
  
  # rename levels to month names
  levels(departure) <- data.frame(ID = 1:12,
                                  month = c("October", "November", "December",
                                            "January", "February", "March",
                                            "April", "May", "June",
                                            "July", "August", "September"))
  plot(departure)
  
  # export
  writeRaster(persistence, paste0("output/imagery/persistence/ssp126_", gcm, "_habitat_persistence.tif"), overwrite=TRUE)
  writeRaster(arrival, paste0("output/imagery/persistence/ssp126_", gcm, "_arrival_month.tif"), overwrite=TRUE)
  writeRaster(departure, paste0("output/imagery/persistence/ssp126_", gcm, "_departure_month.tif"), overwrite=TRUE)
  
}


#-------------------------------------------------------------------------------
# Plot changes in persistence
#-------------------------------------------------------------------------------

# cleanup
rm(list=ls())

# read in present day persistence
present <- rast("output/imagery/persistence/habitat_persistence.tif")

# read in ssp585 persistence for each gcm
ssp585_files <- list.files("output/imagery/persistence/", pattern = "ssp585_.*_habitat_persistence.tif", full.names = TRUE)
ssp585_list <- lapply(ssp585_files, rast)
ssp585_stack <- rast(ssp585_list)
ssp585_mean <- app(ssp585_stack, mean, na.rm = TRUE)

# read in ssp126 persistence for each gcm
ssp126_files <- list.files("output/imagery/persistence/", pattern = "ssp126_.*_habitat_persistence.tif", full.names = TRUE)
ssp126_list <- lapply(ssp126_files, rast)
ssp126_stack <- rast(ssp126_list)
ssp126_mean <- app(ssp126_stack, mean, na.rm = TRUE)

# calculate differences
diff126 <- ssp126_mean - present
diff585 <- ssp585_mean - present

# project
present <- project(present, "EPSG:6932")
diff126 <- project(diff126, "EPSG:6932")
diff585 <- project(diff585, "EPSG:6932")

# read in coastline for plotting
coast <- readRDS("data/coast_ice_vect.RDS")

# get min and max difference values
min <- min(c(minmax(diff126)[1,], minmax(diff585)[1,]))
max <- max(c(minmax(diff126)[2,], minmax(diff585)[2,]))
abs_max <- max(abs(min), abs(max))

# plots
p1 <- ggplot() +
  geom_spatraster(data = present) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  scale_fill_viridis_c(na.value = "transparent", name = "Habitat Persistence") + 
  theme_void()
p1 + ggview::canvas(width = 10, height = 10)

p2 <- ggplot() +
  geom_spatraster(data = diff126) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  scale_fill_gradient2(high = "steelblue4", low = "darkred", mid = "grey90", 
                       limits = c(-abs_max, abs_max), na.value = "transparent",
                       name = "SSP126 Difference") +
  theme_void()
p2 + ggview::canvas(width = 10, height = 10)

p3 <- ggplot() +
  geom_spatraster(data = diff585) +
  geom_spatvector(data = coast, col = NA, fill = "white") +
  scale_fill_gradient2(high = "steelblue4", low = "darkred", mid = "grey90",
                       limits = c(-abs_max, abs_max), na.value = "transparent",
                       name = "SSP585 Difference") +
  theme_void()
p3 + ggview::canvas(width = 10, height = 10)

# plot together
grid <- cowplot::plot_grid(p1, p2, p3, ncol = 1)
grid + ggview::canvas(width = 8, height = 14)

# export
ggsave("output/imagery/persistence/1. Persistence.png", grid,
       height = 14, width = 8)


#-------------------------------------------------------------------------------
# Plot changes in arrival month
#-------------------------------------------------------------------------------

# cleanup
rm(list=ls())

# read in present day arrival
arrival <- rast("output/imagery/persistence/arrival_month.tif")

# recode values to numbers 
arrival <- as.numeric(arrival)

# read in SSP126 arrival
ssp126_files <- list.files("output/imagery/persistence/", pattern = "ssp126_.*_arrival_month.tif", full.names = TRUE)
ssp126_files <- ssp126_files[!grepl("aux", ssp126_files)]
ssp126_list <- lapply(ssp126_files, rast)
ssp126_stack <- rast(ssp126_list)
ssp126_stack <- as.numeric(ssp126_stack)
ssp126_mean <- app(ssp126_stack, mean, na.rm = TRUE)

# read in SSP585 arrival
ssp585_files <- list.files("output/imagery/persistence/", pattern = "ssp585_.*_arrival_month.tif", full.names = TRUE)
ssp585_files <- ssp585_files[!grepl("aux", ssp585_files)]
ssp585_list <- lapply(ssp585_files, rast)
ssp585_stack <- rast(ssp585_list)
ssp585_stack <- as.numeric(ssp585_stack)
ssp585_mean <- app(ssp585_stack, mean, na.rm = TRUE)

# revalue NAs to 0 for difference calculation
values(ssp126_mean)[is.na(values(ssp126_mean))] <- 0
values(ssp585_mean)[is.na(values(ssp585_mean))] <- 0

# calculate difference
diff126 <- ssp126_mean - arrival
diff585 <- ssp585_mean - arrival

# truncate values to between -5 and 5
diff126 <- clamp(diff126, -5, 5)
diff585 <- clamp(diff585, -5, 5)

# revalue arrival dates to month names
levels(arrival) <- data.frame(ID = 1:12,
                             month = c("October", "November", "December",
                                       "January", "February", "March",
                                       "April", "May", "June",
                                       "July", "August", "September"))

# project
arrival <- project(arrival, "EPSG:6932")
diff126 <- project(diff126, "EPSG:6932")
diff585 <- project(diff585, "EPSG:6932")

# read in coastline for plotting
coast <- readRDS("data/coast_ice_vect.RDS")

# plots
p1 <- ggplot() +
  geom_spatraster(data = arrival) +
  geom_spatvector(data = coast, col = NA, fill = "grey30") +
  scale_fill_viridis_d(na.value = "transparent", name = "Arrival Month") + 
  theme_void()
p1 + ggview::canvas(width = 10, height = 10)

p2 <- ggplot() +
  geom_spatraster(data = diff126) +
  geom_spatvector(data = coast, col = NA, fill = "grey30") +
  scale_fill_gradient2(high = "steelblue4", low = "darkred", mid = "grey90", 
                       na.value = "transparent",
                       name = "SSP126 Difference") +
  theme_void()
p2 + ggview::canvas(width = 10, height = 10)

p3 <- ggplot() +
  geom_spatraster(data = diff585) +
  geom_spatvector(data = coast, col = NA, fill = "grey30") +
  scale_fill_gradient2(high = "steelblue4", low = "darkred", mid = "grey90",
                       na.value = "transparent",
                       name = "SSP585 Difference") +
  theme_void()
p3 + ggview::canvas(width = 10, height = 10)

# plot together
grid <- cowplot::plot_grid(p1, p2, p3, ncol = 1)
grid + ggview::canvas(width = 8, height = 14)

# export
ggsave("output/imagery/persistence/2. Arrival Month.png", grid,
       height = 14, width = 8)

#-------------------------------------------------------------------------------
# Plot changes in departure month
#-------------------------------------------------------------------------------

# cleanup
rm(list=ls())

# read in present day departure
departure <- rast("output/imagery/persistence/departure_month.tif")

# recode values to numbers 
departure <- as.numeric(departure)

# read in SSP126 departure
ssp126_files <- list.files("output/imagery/persistence/", pattern = "ssp126_.*_departure_month.tif", full.names = TRUE)
ssp126_files <- ssp126_files[!grepl("aux", ssp126_files)]
ssp126_list <- lapply(ssp126_files, rast)
ssp126_stack <- rast(ssp126_list)
ssp126_stack <- as.numeric(ssp126_stack)
ssp126_mean <- app(ssp126_stack, mean, na.rm = TRUE)

# read in SSP585 departure
ssp585_files <- list.files("output/imagery/persistence/", pattern = "ssp585_.*_departure_month.tif", full.names = TRUE)
ssp585_files <- ssp585_files[!grepl("aux", ssp585_files)]
ssp585_list <- lapply(ssp585_files, rast)
ssp585_stack <- rast(ssp585_list)
ssp585_stack <- as.numeric(ssp585_stack)
ssp585_mean <- app(ssp585_stack, mean, na.rm = TRUE)

# revalue NAs to 0 for difference calculation
values(ssp126_mean)[is.na(values(ssp126_mean))] <- 0
values(ssp585_mean)[is.na(values(ssp585_mean))] <- 0

# calculate difference
diff126 <- ssp126_mean - departure
diff585 <- ssp585_mean - departure

# truncate values to between -5 and 5
diff126 <- clamp(diff126, -5, 5)
diff585 <- clamp(diff585, -5, 5)

# revalue departure dates to month names
levels(departure) <- data.frame(ID = 1:12,
                              month = c("October", "November", "December",
                                        "January", "February", "March",
                                        "April", "May", "June",
                                        "July", "August", "September"))

# project
departure <- project(departure, "EPSG:6932")
diff126 <- project(diff126, "EPSG:6932")
diff585 <- project(diff585, "EPSG:6932")

# read in coastline for plotting
coast <- readRDS("data/coast_ice_vect.RDS")

# plots
p1 <- ggplot() +
  geom_spatraster(data = departure) +
  geom_spatvector(data = coast, col = NA, fill = "grey30") +
  scale_fill_viridis_d(na.value = "transparent", name = "Departure Month") + 
  theme_void()
p1 + ggview::canvas(width = 10, height = 10)

p2 <- ggplot() +
  geom_spatraster(data = diff126) +
  geom_spatvector(data = coast, col = NA, fill = "grey30") +
  scale_fill_gradient2(high = "steelblue4", low = "darkred", mid = "grey90", 
                       na.value = "transparent",
                       name = "SSP126 Difference") +
  theme_void()
p2 + ggview::canvas(width = 10, height = 10)

p3 <- ggplot() +
  geom_spatraster(data = diff585) +
  geom_spatvector(data = coast, col = NA, fill = "grey30") +
  scale_fill_gradient2(high = "steelblue4", low = "darkred", mid = "grey90",
                       na.value = "transparent",
                       name = "SSP585 Difference") +
  theme_void()
p3 + ggview::canvas(width = 10, height = 10)

# plot together
grid <- cowplot::plot_grid(p1, p2, p3, ncol = 1)
grid + ggview::canvas(width = 8, height = 14)

# export
ggsave("output/imagery/persistence/3. Departure Month.png", grid,
       height = 14, width = 8)
