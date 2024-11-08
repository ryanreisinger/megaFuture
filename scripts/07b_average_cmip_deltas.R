#-----------------------------
# Calculate average delta and project to GLORYS layers
#-----------------------------

rm(list=ls())

{
  library(terra)
}

#set working directory to delta folder
setwd(paste0("E:/cmip6_data/CMIP6/deltas/", which_var))

#define variable 
which_var <- "uas"

#list all models (use spreadsheet to automate this)
#if any repeated modelling groups, eliminate one model
models <- c("AWI-CM-1-1-MR", "CAS-ESM2-0", "CMCC-ESM2",
            "HadGEM3-GC31-MM", "MPI-ESM1-2-HR", "MRI-ESM2-0")


# 1. SSP126

#list all SSP126 delta rasters
deltas <- list.files(path="ssp126/")

#filter to only include models listed earlier (i.e. no duplicates)
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
  
#calculate average delta
mean_ssp126_delta <- mean(ssp126_delta)

#visualise
plot(mean_ssp126_delta)


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

#calculate average delta
mean_ssp585_delta <- mean(ssp585_delta)

#visualise
plot(mean_ssp585_delta)


# 3. Export

#export ssp126
writeCDF(mean_ssp126_delta, filename = paste0("ssp126/mean_ssp126_", which_var, "_delta.nc"))

#export ssp585
writeCDF(mean_ssp585_delta, filename = paste0("ssp585/mean_ssp585_", which_var, "_delta.nc"))

#export final model list
saveRDS(models, file = paste0("models_", which_var, ".rds"))
