# make contemporaneous predictions of habitat suitability

rm(list=ls())
setwd("/iridisfs/scratch/jcw2g17/")

library(dplyr)
library(lubridate)
library(terra)
library(tidyterra)
library(tidymodels)
library(tidysdm)
library(bonsai)


#------------------------------------------------------------
# Compile Raster Stack
#------------------------------------------------------------

# list of months to predict to
months <- c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)

# load covariate names
all_vars <- read.csv("humpbacks/output/varselection/key_vars.csv") %>%
  pull(key_vars)

# separate out static and dynamic covariates
static_vars <- all_vars[all_vars %in% c("depth", "slope", "dshelf")]
dynamic_vars <- all_vars[!all_vars %in% static_vars]

# set out target extent
e <- ext(-180, 180, -90, -40)

# for each static covariate
for(var in static_vars){
  
  # read in raster
  var_rast <- rast(paste0("Satellite_Data/static/", var, "/", var, ".nc"))
  
  # assign crs (missing in netCDFs)
  crs(var_rast) <- "EPSG:4326"
  
  # crop to target extent
  var_rast <- crop(var_rast, e)
  
  # combine with other static rasters
  if(var == static_vars[1]) {
    static_stack <- var_rast
  } else {
    static_stack <- c(static_stack, var_rast)
  }
}

# apply names
names(static_stack) <- static_vars

# for each dynamic covariate
for(var in dynamic_vars){
  
  # read in monthly raster
  var_rast <- rast(paste0("Satellite_Data/monthly/", var, "/", var, ".nc"))
  
  # limit to target months
  var_rast <- var_rast[[month(time(var_rast)) %in% months]]
  
  # apply CRS
  crs(var_rast) <- "EPSG:4326"
  
  # crop to target extent
  var_rast <- crop(var_rast, e)
  
  # if SIC, revalue NAs to 0
  if(var == "sic"){
    values(var_rast) <- ifelse(is.na(values(var_rast)), 0, values(var_rast))
  }
  
  # combine with other dynamic rasters
  if(var == dynamic_vars[1]) {
    dynamic_stack <- var_rast
  } else {
    dynamic_stack <- c(dynamic_stack, var_rast)
  }
  
  # print completion
  print(var)
}

# create region raster to enable predictions
region_rast <- rast(ext = ext(static_stack), crs = "epsg:4326", res = res(static_stack))
values(region_rast) <- "test"
names(region_rast) <- "region"


#------------------------------------------------------------
# Boosted Regression Trees
#------------------------------------------------------------

# print initialisation
print("Boosted Regression Trees")

# load in boosted regression tree model
brt <- readRDS("humpbacks/output/boosted regression trees/brt_model.rds")

# list each month in the dynamic stack
timeslices <- time(dynamic_stack) %>% unique()

# for each slice
for(j in 1:length(timeslices)){
  slice <- timeslices[j]
  
  # limit dynamic stack to current slice
  dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
  
  # create a raster for the month to allow for interactive effects
  month_rast <- rast(ext = ext(static_stack), crs = "epsg:4326", res = res(static_stack))
  values(month_rast) <- month(slice)
  names(month_rast) <- "month"
  
  # combine static, dynamic, region, and month rasters
  stack <- c(static_stack, dynamic_slice, region_rast, month_rast)
  
  # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
  names(stack) <- gsub("zos_", "ssh_", names(stack))
  names(stack) <- gsub("so_", "sal_", names(stack))
  names(stack) <- gsub("thetao_", "sst_", names(stack))
  names(stack) <- gsub("siconc_", "sic_", names(stack))
  names(stack) <- gsub("mlotst_", "mld_", names(stack))
  
  # remove everything following the first underscore from names
  names(stack) <- gsub("_.*", "", names(stack))
  
  # predict raster
  pred_raster <- predict_raster(brt, stack, type = "prob")
  
  # limit to presences only
  pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
  
  # apply timestamp to raster
  time(pred_raster) <- slice
  
  # combine with other predictions
  if(slice == timeslices[1]) {
    preds <- pred_raster
  } else {
    preds <- c(preds, pred_raster)
  }
  
  # print completion
  print(slice)
}

# for each month, average predictions
for(this_month in months){
  
  # isolate predictions for this month
  month_preds <- preds[[month(time(preds)) == this_month]]
  
  # average predictions
  month_preds <- app(month_preds, mean, na.rm = TRUE)
  
  # assign time as 2010 for this month
  time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
  
  # join to all monthly predictions
  if(this_month == months[1]) {
    all_month_preds <- month_preds
  } else {
    all_month_preds <- c(all_month_preds, month_preds)
  }
}

# plot all month predictions
plot(all_month_preds)

# export the BRT prediction
writeRaster(all_month_preds, 
            filename = paste0("humpbacks/output/predictions/brt_prediction.tif"),
            overwrite = TRUE)

# cleanup
rm(brt, all_month_preds)


#------------------------------------------------------------
# Random Forests
#------------------------------------------------------------

# print initialisation
print("Random Forests")

# load in random forest model
rf <- readRDS("humpbacks/output/random forests/rf_model.rds")

# list each month in the dynamic stack
timeslices <- time(dynamic_stack) %>% unique()

# for each slice
for(j in 1:length(timeslices)){
  slice <- timeslices[j]
  
  # limit dynamic stack to current slice
  dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
  
  # create a raster for the month to allow for interactive effects
  month_rast <- rast(ext = ext(static_stack), crs = "epsg:4326", res = res(static_stack))
  values(month_rast) <- month(slice)
  names(month_rast) <- "month"
  
  # combine static, dynamic, region, and month rasters
  stack <- c(static_stack, dynamic_slice, region_rast, month_rast)
  
  # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
  names(stack) <- gsub("zos_", "ssh_", names(stack))
  names(stack) <- gsub("so_", "sal_", names(stack))
  names(stack) <- gsub("thetao_", "sst_", names(stack))
  names(stack) <- gsub("siconc_", "sic_", names(stack))
  names(stack) <- gsub("mlotst_", "mld_", names(stack))
  
  # remove everything following the first underscore from names
  names(stack) <- gsub("_.*", "", names(stack))
  
  # predict raster
  pred_raster <- predict_raster(rf, stack, type = "prob")
  
  # limit to presences only
  pred_raster <- pred_raster[[names(pred_raster) == ".pred_presence"]]
  
  # apply timestamp to raster
  time(pred_raster) <- slice
  
  # combine with other predictions
  if(slice == timeslices[1]) {
    preds <- pred_raster
  } else {
    preds <- c(preds, pred_raster)
  }
  
  # print completion
  print(slice)
}

# for each month, average predictions
for(this_month in months){
  
  # isolate predictions for this month
  month_preds <- preds[[month(time(preds)) == this_month]]
  
  # average predictions
  month_preds <- app(month_preds, mean, na.rm = TRUE)
  
  # assign time as 2010 for this month
  time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
  
  # join to all monthly predictions
  if(this_month == months[1]) {
    all_month_preds <- month_preds
  } else {
    all_month_preds <- c(all_month_preds, month_preds)
  }
}

# plot all month predictions
plot(all_month_preds)

# export the RF prediction
writeRaster(all_month_preds, 
            filename = paste0("humpbacks/output/predictions/rf_prediction.tif"),
            overwrite = TRUE)

# cleanup
rm(rf, all_month_preds)


#------------------------------------------------------------
# Bayesian Additive Regression Trees
#------------------------------------------------------------

# print initialisation
print("Bayesian Additive Regression Trees")

# load in BART model
bart <- readRDS("humpbacks/output/bayesian additive regression trees/bart_model.rds")
bart <- bundle::unbundle(bart)

# list each month in the dynamic stack
timeslices <- time(dynamic_stack) %>% unique()

# for each slice
for(j in 1:length(timeslices)){
  slice <- timeslices[j]
  
  # limit dynamic stack to current slice
  dynamic_slice <- dynamic_stack[[time(dynamic_stack) == slice]]
  
  # create a raster for the month to allow for interactive effects
  month_rast <- rast(ext = ext(static_stack), crs = "epsg:4326", res = res(static_stack))
  values(month_rast) <- month(slice)
  names(month_rast) <- "month"
  
  # combine static, dynamic, region, and month rasters
  stack <- c(static_stack, dynamic_slice, region_rast, month_rast)
  
  # replace dynamic name codes with full names, e.g. if name is "zos_1", change to "ssh_1"
  names(stack) <- gsub("zos_", "ssh_", names(stack))
  names(stack) <- gsub("so_", "sal_", names(stack))
  names(stack) <- gsub("thetao_", "sst_", names(stack))
  names(stack) <- gsub("siconc_", "sic_", names(stack))
  names(stack) <- gsub("mlotst_", "mld_", names(stack))
  
  # remove everything following the first underscore from names
  names(stack) <- gsub("_.*", "", names(stack))
  
  # get all values of raster
  vals <- terra::as.matrix(stack)
  
  # create empty output vector
  blank_output <- as.numeric(rep(NA, nrow(vals)))
  
  # Get indices of non-NA values in the input matrix
  which_vals <- which(complete.cases(vals))
  
  # Remove NA values from the input matrix
  input_matrix <- vals[complete.cases(vals), , drop = FALSE]
  
  # chunk over input matrix
  total_length <- nrow(input_matrix)
  
  # set chunk size 
  chunk_size <- 10000
  
  # define number of chunks
  num_chunks <- ceiling(total_length / chunk_size)
  
  # for each chunk
  i <- 1
  
  # subset matrix to this number of chunks
  while(i <= num_chunks){
    # get start and end of chunk
    start <- (i - 1) * chunk_size + 1
    end <- min(i * chunk_size, total_length)
    
    # if last chunk, change end to end value of matrix
    if(i == num_chunks){
      end <- total_length
    }
    
    # get chunk
    input_chunk <- input_matrix[start:end, ]
    
    # make predictions
    pred_chunk <- predict(bart, input_chunk, type = "prob") %>%
      pull(.pred_presence)
    
    # merge with other data
    if(i == 1){
      prediction_vals <- pred_chunk
    } else{
      prediction_vals <- c(prediction_vals, pred_chunk)
    }
    
    # print progress if i is divisible by 10
    if(i%%10 == 0){
      print(paste0(i, "/", num_chunks))
    }
    
    # increment i
    i <- i + 1
  }
  
  # join predictions to blank output
  blank_output[which_vals] <- prediction_vals
  
  # create empty raster
  pred_raster <- rast(ext = ext(stack), crs = crs(stack), res = res(stack))
  
  # assign values to raster
  values(pred_raster) <- blank_output
  plot(pred_raster)
  
  # apply timestamp to raster
  time(pred_raster) <- slice
  
  # combine with other predictions
  if(slice == timeslices[1]) {
    preds <- pred_raster
  } else {
    preds <- c(preds, pred_raster)
  }
  
  # print completion
  print(slice)
}

# for each month, average predictions
for(this_month in months){
  
  # isolate predictions for this month
  month_preds <- preds[[month(time(preds)) == this_month]]
  
  # average predictions
  month_preds <- app(month_preds, mean, na.rm = TRUE)
  
  # assign time as 2010 for this month
  time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
  
  # join to all monthly predictions
  if(this_month == months[1]) {
    all_month_preds <- month_preds
  } else {
    all_month_preds <- c(all_month_preds, month_preds)
  }
}

# plot all month predictions
plot(all_month_preds)

# export the BART prediction
writeRaster(all_month_preds, 
            filename = paste0("humpbacks/output/predictions/bart_prediction.tif"),
            overwrite = TRUE)


#------------------------------------------------------------
# Ensemble Prediction
#------------------------------------------------------------

# print initialisation
print("Ensemble Prediction")

# reset environment
rm(list=ls())

# read in all predicted rasters
rf <- rast("humpbacks/output/predictions/rf_prediction.tif")
brt <- rast("humpbacks/output/predictions/brt_prediction.tif")
bart <- rast("humpbacks/output/predictions/bart_prediction.tif")

# stack predictions
pred_stack <- c(rf, brt, bart)

# for each month, average predictions
for(this_month in c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7)){
  
  # isolate predictions for this month
  month_preds <- pred_stack[[month(time(pred_stack)) == this_month]]
  
  # average predictions
  month_preds <- app(month_preds, mean, na.rm = TRUE)
  
  # assign time as 2010 for this month
  time(month_preds) <- as_date(paste0("2010-", this_month, "-01"))
  
  # join to all monthly predictions
  if(this_month == 10) {
    all_month_preds <- month_preds
  } else {
    all_month_preds <- c(all_month_preds, month_preds)
  }
}

# plot
plot(all_month_preds)

# export
writeRaster(all_month_preds, 
            filename = paste0("humpbacks/output/predictions/ensemble_prediction.tif"),
            overwrite = TRUE)
