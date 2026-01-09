#-----------------------------
# Calculate average delta and project to GLORYS layers
#-----------------------------

rm(list=ls())

{
  library(terra)
  library(lubridate)
}


#define variable 
which_var <- "zos"

#set working directory to delta folder
setwd(paste0("E:/cmip6_data/CMIP6/deltas/", which_var))

#read in info on the models available for each variable
model_info <- read.csv("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Humpbacks/code/out/cmip6/cmip6_final_models.csv")

#isolate models where variable is available
var_models <- model_info %>% 
  select(Institute, Model, all_of(which_var)) %>%
  filter(across(3) != "")

#list all models (use spreadsheet to automate this)
models <- var_models$Model


# 1. SSP126

#list all SSP126 delta rasters
deltas <- list.files(path="ssp126/")

#filter to ensure that only models listed earlier are used (i.e. no duplicates)
deltas <- deltas[grep(paste(models, collapse="|"), deltas)]

#read in and combine all deltas
for(i in deltas){
  this_delta <- rast(paste0("ssp126/", i))
  
  if(i == deltas[1]){
    ssp126_delta <- this_delta
  } else {
    ssp126_delta <- c(ssp126_delta, this_delta)
  }
  
  rm(this_delta)
}

#calculate average delta per month
for(i in 1:12){
  
  #extract all deltas for this month
  this_month <- ssp126_delta[[month(time(ssp126_delta)) == i]]
  
  #average this months deltas
  this_month_mean <- mean(this_month, na.rm = T)
  
  #assign time
  time(this_month_mean) <- time(this_month)[1]
  
  #join to deltas for all months
  if(i == 1){
    ssp126_mean_delta <- this_month_mean
  } else {
    ssp126_mean_delta <- c(ssp126_mean_delta, this_month_mean)
  }
}

#visualise
plot(ssp126_mean_delta)


# 2. SSP585

#list all SSP585 delta rasters
deltas <- list.files(path="ssp585/")

#filter to only include models listed earlier (i.e. no duplicates)
deltas <- deltas[grep(paste(models, collapse="|"), deltas)]

#read in and combine all deltas
for(i in deltas){
  this_delta <- rast(paste0("ssp585/", i))
  
  if(i == deltas[1]){
    ssp585_delta <- this_delta
  } else {
    ssp585_delta <- c(ssp585_delta, this_delta)
  }
  
  rm(this_delta)
}

#calculate average delta per month
for(i in 1:12){
  
  #extract all deltas for this month
  this_month <- ssp585_delta[[month(time(ssp585_delta)) == i]]
  
  #average this months deltas
  this_month_mean <- mean(this_month, na.rm = T)
  
  #assign time
  time(this_month_mean) <- time(this_month)[1]
  
  #join to deltas for all months
  if(i == 1){
    ssp585_mean_delta <- this_month_mean
  } else {
    ssp585_mean_delta <- c(ssp585_mean_delta, this_month_mean)
  }
}

#visualise
plot(ssp585_mean_delta)


# 3. Export

#export ssp126
writeCDF(ssp126_mean_delta, filename = paste0("ssp126/mean_ssp126_", which_var, "_delta.nc"),
         overwrite = T)

#export ssp585
writeCDF(ssp585_mean_delta, filename = paste0("ssp585/mean_ssp585_", which_var, "_delta.nc"),
         overwrite = T)
