library(rcmip6)
library(dplyr)
library(tidyr)

# Create a vector of the variables we are interested in
which_variables <- c("tos", "siconc", "tauuo", "tauvo", "uo", "vo", "mlotst", "so", "zos", "chl")

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
df <- df %>% pivot_wider(names_from = variable_id, values_from = variable_id, values_fill = list(variable_id = NA))
df %>% arrange(source_id)

# dplyr::filter(results, member_id == "r1i1p1f1") |> 
#   cmip_simplify()

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
  filter(!any(nominal_resolution == "250 km")) |> # drop model if it has 250 km resolution
  summarise(n = n_distinct(variable_id)) |> 
  filter(n == length(unique(query$variable_id))) |> 
  pull(source_id)

query <- list(
  type               = "Dataset",
  source_id = which_sources,
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
df <- df %>% pivot_wider(names_from = variable_id, values_from = variable_id, values_fill = list(variable_id = NA))
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


# Download the data
this_result <- filter(results, source_id == "CESM2")

# Filter results to include only member_id including 'r1'
this_result <- filter(this_result, grepl("r1i", member_id))
cmip_simplify(this_result)

# Check
cmip_info(this_result)
cmip_size(this_result)/1e+6 # size in terrabytes
# cmip_download(results, dest_folder = "data/cmip6")