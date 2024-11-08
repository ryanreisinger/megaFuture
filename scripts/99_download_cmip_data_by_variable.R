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

# List of all variables
vars <- c("uas", "vas", "chlos", "sos", "tos", "zos", "mlotst", "siconc")

# Choose a variable
which_var <- "siconc"

# Setup a search query
query <- list(
  type               = "Dataset",
  # institution_id     = "CCCma",
  replica            = "false",
  latest             = "true",
  variable_id        = which_var,
  project            = "CMIP6",
  frequency          = "mon",                          
  # table_id           = c("Omon", "SImon"),
  experiment_id      = c("historical", "ssp126", "ssp585")
)

# Get available models
this_result <- cmip_search(query)

# Filter to only native grids
this_result <- filter(this_result, grid_label == "gn")

# Remove models with data download issues
this_result <- filter(this_result, source_id != "BCC-CSM2-MR" & source_id != "TaiESM1")

# Keep only members from run 1
this_result <- filter(this_result, grepl("r1i", member_id))

# Remove members with data for fewer than three scenarios
incomplete <- this_result %>% 
  group_by(source_id, member_id, nominal_resolution) %>%
  summarise(scenarios = n_distinct(experiment_id)) %>%
  filter(scenarios < 3)
this_result <- this_result %>% 
  anti_join(incomplete, by = c("source_id", "member_id", "nominal_resolution"))

# Remove any excess members for some experiments
members <- this_result %>% 
  group_by(source_id, experiment_id) %>% 
  arrange(member_id) %>%
  slice(1) %>% 
  ungroup()
excess <- this_result %>% 
  anti_join(members, by = c("source_id", "experiment_id", "member_id"))
this_result <- this_result %>% 
  anti_join(excess)

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

# Set the root folder where to save files 
cmip_root_set("cmip6_data")   

# Configure download settings to only retry once if slow speeds
cmip_download_config(retry = 2)

# Download the files
files <- cmip_download(this_result, year_range = c(1985, 2100)) 

