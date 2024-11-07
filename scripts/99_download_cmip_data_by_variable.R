#UAS and VAS Failures: BCC-CSM2-MR_ssp126, BCC-CSM2-MR_historical, BCC-CSM2-MR_ssp585
#check VAS CAS_ESM2-0
#-----------------------------
# Explore and download by variable
#-----------------------------

setwd("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Humpbacks")

library(rcmip6)
library(dplyr)
library(tidyr)
library(terra)
library(ncdf4)

# Get the results
results <- readRDS("output/cmip6/cmip6_results.rds")

# Get the summary table
summary_table <- read.csv("output/cmip6/cmip6_summary_table.csv")

# List of all variables
vars <- names(summary_table)[6:13]

# Choose a variable
which_var <- "vas"

# Filter results to include only the chosen variable
this_result <- filter(results, variable_id == which_var)

# Filter to only native grids
this_result <- filter(this_result, grid_label == "gn")

# Remove members with data for fewer than three scenarios
incomplete <- this_result %>% 
  group_by(source_id, member_id, nominal_resolution) %>%
  summarise(scenarios = n_distinct(experiment_id)) %>%
  filter(scenarios < 3)
this_result <- this_result %>% 
  anti_join(incomplete, by = c("source_id", "member_id", "nominal_resolution"))

# Are there multiple members for some experiments?
members <- this_result %>% 
  group_by(source_id, experiment_id) %>% 
  arrange(member_id) %>%
  slice(1) %>% 
  ungroup()
nrow(anti_join(this_result, members, by = c("source_id", "experiment_id", "member_id")))

# Only keep resolutions of 100km
this_result <- filter(this_result, nominal_resolution == "100 km")

# Summary table for this result
df <- cmip_simplify(this_result) %>% data.table::data.table() %>% dplyr::select(source_id, experiment_id, member_id, variable_id, grid_label, nominal_resolution)
df <- df %>% distinct() %>% pivot_wider(names_from = variable_id, values_from = variable_id, values_fill = list(variable_id = NA))
df <- df %>% arrange(source_id, experiment_id)
df

# Export list of models/members for this variable
write.csv(df, paste0("output/cmip6/summaries/cmip6_summary_table_", which_var, ".csv"), row.names = FALSE)

# Check download size
cmip_size(this_result)/1e+6 # size in terrabytes
cmip_size(this_result)/1000 # size in gigabytes

#-----------------------------
# Download the data
#-----------------------------

# Set working directory to hard drive
setwd("E://")

cmip_root_set("cmip6_data")   # Set the root folder where to save files 
# dir.create(cmip_root_get()) # Create the folder if it doesn't exist
files <- cmip_download(this_result, year_range = c(1985, 2100)) # Download the data

#-----------------------------
# After download
#-----------------------------

# Read in a file to check
t <- terra::rast("/Volumes/roamer/cmip6_data/CMIP6/CMIP/NCAR/CESM2/historical/r1i1p1f1/Omon/chlos/gr/20190308/chlos_Omon_CESM2_historical_r1i1p1f1_gr_185001-201412.nc")

# Plot the last layer of the raster
terra::plot(t, nlyr(t), col = terrain.colors(100))

# Check characteristics of the netcdf
t_n <- ncdf4::nc_open("/Volumes/roamer/cmip6_data/CMIP6/CMIP/NCAR/CESM2/historical/r1i1p1f1/Omon/chl/gr/20190308/chl_Omon_CESM2_historical_r1i1p1f1_gr_185001-201412.nc")