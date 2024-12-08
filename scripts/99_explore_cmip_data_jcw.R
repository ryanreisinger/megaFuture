library(rcmip6)
library(dplyr)
library(tidyr)
library(terra)

# Create a vector of the variables we are interested in
which_variables <- c("tos", "siconc", "sfcWind", "mlotst", "sos", "zos", "chlos", "uas", "vas")
#uo and vo not available at surface - MIROC size with uo and vo = 60GB, without = 3.5GB

# Notes:
# - tos: sea surface temperature versus thetaao: potential temperature?
# - taouo: zonal wind stress and tauvo: meridional wind stress, uas and vas is wind speed
# - uo: zonal velocity and vo: meridional velocity

# Which scenario do we want?
which_scenarios <- c("historical", "ssp126", "ssp585")
# which_scenarios <- c("historical")

# Create a query list
# Can look manually at https://aims2.llnl.gov/search/cmip6/
query <- list(
  type               = "Dataset",
  # institution_id     = "CCCma",
  replica            = "false",
  latest             = "true",
  variable_id        = which_variables,
  project            = "CMIP6",
  frequency          = "mon",                          
  # table_id           = c("Omon", "SImon"),
  experiment_id      = which_scenarios
)

results <- cmip_search(query)
cmip_info(results)

# Simplify and show the results
results |> 
  cmip_simplify() |>   # To keep only the most informative columns
  subset(, select = -full_info) |>
  head(10) |>
  knitr::kable()

results

# Summary table (from James Grecian)
df <- cmip_simplify(results) %>% tibble() %>% dplyr::select(source_id, experiment_id, member_id, variable_id, grid_label, nominal_resolution)
df <- df %>% distinct() %>% pivot_wider(names_from = variable_id, values_from = variable_id, values_fill = list(variable_id = NA)) 
df %>% arrange(source_id)
  

#dplyr::filter(results, member_id == "r1i1p1f1") |> 
# cmip_simplify()

# Find which source_id has all variable_id
results |> 
  cmip_simplify() |> 
  group_by(source_id) |> 
  summarise(n = n_distinct(variable_id)) |> 
  filter(n == length(unique(query$variable_id))) |> 
  pull(source_id)

# dplyr::filter(results, source_id == "CanESM5") |>
#   cmip_simplify()

# Get source_ids for those entries with all variables, and re-run the query
which_sources <-
  results |> 
  cmip_simplify() |> 
  group_by(source_id) |> 
  filter(!nominal_resolution %in% c("250 km", "500 km")) |> # drop model if it has 250 km resolution
  summarise(n = n_distinct(variable_id)) |> 
  filter(n == length(unique(query$variable_id))) |> 
  pull(source_id)

query <- list(
  type               = "Dataset",
  source_id          = which_sources,
  replica            = "false",
  latest             = "true",
  variable_id        = which_variables,
  project            = "CMIP6",
  frequency          = "mon",                          
  # table_id           = c("Omon", "SImon"),
  experiment_id      = which_scenarios
)

results <- cmip_search(query)
cmip_info(results)

# Summary table again
df <- cmip_simplify(results) %>% tibble() %>% dplyr::select(source_id, experiment_id, member_id, variable_id, grid_label, nominal_resolution)
df <- df %>% distinct() %>% pivot_wider(names_from = variable_id, values_from = variable_id, values_fill = list(variable_id = NA))
df %>% arrange(source_id)

# Still a few NAs, filter those out
summary_table <-
  df |> 
  filter(if_all(everything(), ~ !is.na(.x))) |> 
  arrange(source_id)

summary_table

length(unique(summary_table$source_id))

# Save the summary table as a .csv file
write.csv(summary_table, "out/cmip6/cmip6_summary_table.csv", row.names = FALSE)

# Look at the URLs of files that will be downloaded for the given results
# slow, so switched off by default
if (FALSE) {
  urls <- cmip_urls(results)
}

# Check the file size in terrabytes
cmip_size(results)/1e+6

# Which modelling centers?
unique(summary_table$source_id)


# Filter to get a specific model
which_model <- "MIROC-ES2H"
this_result <- filter(results, source_id == which_model)

# Filter results to include only member_id including 'r1'
# This is the first realization of the model
# TODO: if there are multiple forcings and physics, choose the first
# TODO: if there are native and regridded grids, choose regridded
this_result <- filter(this_result, grepl("r1i", member_id))

#identify variables where regridded grids are available
regrid_options <- c("gr", "gr1")
regrids <- this_result %>% 
  group_by(variable_id, experiment_id) %>% 
  reframe(grid_label = levels(as.factor(grid_label))) %>%
  filter(grid_label %in% regrid_options)

#filter this_result to only include regridded grids and variables with no regridded option
fullregrids <- this_result %>% 
  filter(variable_id %in% regrids$variable_id & grid_label %in% regrid_options)
fullnatives <- this_result %>% 
  filter(!variable_id %in% regrids$variable_id)
this_result <- bind_rows(fullregrids, fullnatives)

write.csv(filter(summary_table, source_id == which_model), paste0("out/cmip6/summaries/cmip6_summary_table_", which_model, ".csv"), row.names = FALSE)

# Check
cmip_info(this_result)
cmip_size(this_result)/1e+6 # size in terrabytes
cmip_size(this_result)/1000 # size in gigabytes

# Download the data
# Define the download folder
setwd("E://")   # switch to hard drive 
cmip_root_set("cmip6_data") # Set the root folder where to save files
#dir.create(cmip_root_get()) # Create the folder if it doesn't exist
files <- cmip_download(this_result, year_range = c(1985, 2100)) # Download the data

# Read in a file to check
t <- terra::rast("/Volumes/roamer/cmip6_data/CMIP6/CMIP/NCAR/CESM2/historical/r1i1p1f1/Omon/chl/gr/20190308/chl_Omon_CESM2_historical_r1i1p1f1_gr_185001-201412.nc")

# Plot the last layer of the raster
terra::plot(t, nlyr(t), col = terrain.colors(100))
