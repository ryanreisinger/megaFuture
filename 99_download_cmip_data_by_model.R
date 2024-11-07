#-----------------------------
# Explore and download a specific model
#-----------------------------

library(rcmip6)
library(dplyr)
library(tidyr)
library(terra)
library(ncdf4)

# Get the results
results <- readRDS("out/cmip6/cmip6_results.rds")

# Get the summary table
summary_table <- read.csv("out/cmip6/cmip6_summary_table.csv")

# Filter to get a specific model
# which_model <- "CESM2"
which_model <- unique(summary_table$source_id)[1]

this_result <- filter(results, source_id == which_model)

# Filter results to include only member_id including 'r1'
# This is the first realization of the model
# TODO: if there are multiple forcings and physics, choose the first
# TODO: if there are native and regridded grids, choose regridded
this_result <- filter(this_result, grepl("r1i", member_id))
write.csv(filter(summary_table, source_id == which_model), paste0("out/cmip6/summaries/cmip6_summary_table_", which_model, ".csv"), row.names = FALSE)

# Check
cmip_info(this_result)
cmip_size(this_result)/1e+6 # size in terrabytes
cmip_size(this_result)/1000 # size in gigabytes

#-----------------------------
# Download the data
#-----------------------------

# Define the download folder
cmip_root_set("/Volumes/roamer/cmip6_data/")   # Set the root folder where to save files 
# dir.create(cmip_root_get()) # Create the folder if it doesn't exist
files <- cmip_download(this_result) # Download the data

#-----------------------------
# After download
#-----------------------------

# Read in a file to check
t <- terra::rast("/Volumes/roamer/cmip6_data/CMIP6/CMIP/NCAR/CESM2/historical/r1i1p1f1/Omon/chlos/gr/20190308/chlos_Omon_CESM2_historical_r1i1p1f1_gr_185001-201412.nc")

# Plot the last layer of the raster
terra::plot(t, nlyr(t), col = terrain.colors(100))

# Check characteristics of the netcdf
t_n <- ncdf4::nc_open("/Volumes/roamer/cmip6_data/CMIP6/CMIP/NCAR/CESM2/historical/r1i1p1f1/Omon/chl/gr/20190308/chl_Omon_CESM2_historical_r1i1p1f1_gr_185001-201412.nc")