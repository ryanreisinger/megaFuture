#-----------------------------
# Calculate monthly deltas for each variable
#-----------------------------
  
rm(list=ls())
setwd("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Humpbacks/code")

{
  library(terra)
  library(tidyverse)
  library(tidyterra)
}

#the 30-year mean calculation function
source("functions/cmip_mean_calc.R")

#read in info on the models available for each variable
model_info <- read.csv("out/cmip6/cmip6_final_models.csv")

#define variable and CMIP frequency code
which_var <- "siconc" #uas, vas, sos, tos, zos, mlotst, chlos, or siconc
which_freq <- "SImon" #Amon, Omon, or SImon

#isolate models where variable is available
var_models <- model_info %>% 
  select(Institute, Model, all_of(which_var)) %>%
  filter(across(3) != "")

#list all models (use spreadsheet to automate this)
models <- var_models$Model

#list all associated institutes (use spreadsheet to automate)
institutes <- var_models$Institute

#loop through each model
for(m in 1:length(models)){
  
  #set model and institute 
  which_model <- models[m]
  which_institute <- institutes[m]
  
  
  # 1. Create mean climatology rasters
  
  #set working directory to historical folder
  setwd(paste0("E:/cmip6_data/CMIP6/CMIP/", which_institute, "/", which_model, "/historical/"))
  member_dir <- list.dirs()[2]
  setwd(paste0(member_dir, "/", which_freq, "/", which_var, "/gn/"))
  
  #mean calculation function
  historical_mean <- cmip_mean_calc(scenario = "historical")
  
  #set working directory to SSP126 folder 
  setwd(paste0("E:/cmip6_data/CMIP6/ScenarioMIP/", which_institute, "/", which_model, "/ssp126/", member_dir, "/", which_freq, "/", which_var, "/gn/"))
  
  #mean calculation function
  ssp126_mean <- cmip_mean_calc(scenario = "ssp126")
  
  #set working directory to SSP585 folder
  setwd(paste0("E:/cmip6_data/CMIP6/ScenarioMIP/", which_institute, "/", which_model, "/ssp585/", member_dir, "/", which_freq, "/", which_var, "/gn/"))
  
  #mean calculation function
  ssp585_mean <- cmip_mean_calc(scenario = "ssp585")
  
  
  # 2. Create delta layers
  
  #calculate SSP126 delta
  ssp126_delta <- ssp126_mean - historical_mean
  
  #calculate SSP585 delta
  ssp585_delta <- ssp585_mean - historical_mean
  
  
  # 3. Export
  
  #check bilinear interpolation hasn't created issues
  p1 <- ggplot() + geom_spatraster(data = historical_mean[[1]]) +
    ggtitle(paste0(which_var, " ", which_model)) + 
    scale_fill_viridis_c()
  print(p1)
  
  #set working directory to delta folder
  setwd(paste0("E:/cmip6_data/CMIP6/deltas/", which_var))
  
  #export SSP126 delta
  writeCDF(ssp126_delta, 
           filename = paste0("ssp126/", which_model, "_ssp126_", which_var, "_delta.nc"),
           varname = paste0("delta_", which_var),
           longname = paste0("Delta ", which_var, " (", which_model, " SSP126)"),
           overwrite = T)
  
  #export SSP585 delta
  writeCDF(ssp585_delta, 
           filename = paste0("ssp585/", which_model, "_ssp585_", which_var, "_delta.nc"),
           varname = paste0("delta_", which_var),
           longname = paste0("Delta ", which_var, " (", which_model, " SSP585)"),
           overwrite = T)
  
  #print completion
  print(paste0(which_model, " complete"))
}

