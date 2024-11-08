#-----------------------------
# Calculate deltas for each variable
#-----------------------------

rm(list=ls())
setwd("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Humpbacks/code")

{
  library(terra)
  library(lubridate)
}

#the 30-year mean calculation function
source("functions/cmip_mean_calc.R")

#define variable and CMIP frequency code
which_var <- "uas"
which_freq <- "Amon" #Amon, Omon, or SImon

#list all models (use spreadsheet to automate this)
models <- c("AWI-CM-1-1-MR", "CAS-ESM2-0", "CMCC-CM2-SR5", "CMCC-ESM2",
          "HadGEM3-GC31-MM", "MPI-ESM1-2-HR", "MRI-ESM2-0")

#list all associated institutes (use spreadsheet to automate)
institutes <- c("AWI", "CAS", "CMCC", "CMCC", "MOHC", "MPI-M", "MRI")

#set model
m <- 7

#loop through each model
which_model <- models[m]
which_institute <- institutes[m]


# 1. Create mean climatology rasters

#set working directory to historical folder
setwd(paste0("E:/cmip6_data/CMIP6/CMIP/", which_institute, "/", which_model, "/historical/"))
member_dir <- list.dirs()[2]
setwd(paste0(member_dir, "/", which_freq, "/", which_var, "/gn/"))

#mean calculation function
historical_mean <- cmip_mean_calc(scenario = "historical")

#plot historical mean to check
plot(historical_mean)

#set working directory to SSP126 folder 
setwd(paste0("E:/cmip6_data/CMIP6/ScenarioMIP/", which_institute, "/", which_model, "/ssp126/", member_dir, "/", which_freq, "/", which_var, "/gn/"))

#mean calculation function
ssp126_mean <- cmip_mean_calc(scenario = "ssp126")

#plot ssp126 mean to check
plot(ssp126_mean)

#set working directory to SSP585 folder
setwd(paste0("E:/cmip6_data/CMIP6/ScenarioMIP/", which_institute, "/", which_model, "/ssp585/", member_dir, "/", which_freq, "/", which_var, "/gn/"))

#mean calculation function
ssp585_mean <- cmip_mean_calc(scenario = "ssp585")

#plot ssp585 mean to check
plot(ssp585_mean)


# 2. Create delta layers

#calculate SSP126 delta
ssp126_delta <- ssp126_mean - historical_mean

#plot to check
plot(ssp126_delta)

#calculate SSP585 delta
ssp585_delta <- ssp585_mean - historical_mean

#plot to check
plot(ssp585_delta)


# 3. Export

#set working directory to delta folder
setwd(paste0("E:/cmip6_data/CMIP6/deltas/", which_var))

#export SSP126 delta
writeCDF(ssp126_delta, 
         filename = paste0("ssp126/", which_model, "_ssp126_", which_var, "_delta.nc"),
         varname = paste0("delta_", which_var),
         longname = paste0("Delta ", which_var, " (", which_model, " SSP126)"))

#export SSP585 delta
writeCDF(ssp585_delta, 
         filename = paste0("ssp585/", which_model, "_ssp585_", which_var, "_delta.nc"),
         varname = paste0("delta_", which_var),
         longname = paste0("Delta ", which_var, " (", which_model, " SSP585)"))

