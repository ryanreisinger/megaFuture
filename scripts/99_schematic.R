#------------------------------
# CMIP6 Delta Schematic
#------------------------------

rm(list=ls())
setwd("C:/Users/jcw2g17/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}


#------------------------------------------
# CMIP6 Jan Single Year Rasters (MIROC-ES2H)
#------------------------------------------

# load in historical, ssp126, and ssp585 rasters
hist <- rast("E:/cmip6_data/CMIP6/CMIP/MIROC/MIROC-ES2H/historical/r1i1p4f2/Omon/tos/gn/20220322/tos_Omon_MIROC-ES2H_historical_r1i1p4f2_gn_185001-201412_bil_1x1.nc")
ssp126 <- rast("E:/cmip6_data/CMIP6/ScenarioMIP/MIROC/MIROC-ES2H/ssp126/r1i1p4f2/Omon/tos/gn/20230524/tos_Omon_MIROC-ES2H_ssp126_r1i1p4f2_gn_201501-210012_bil_1x1.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/ScenarioMIP/MIROC/MIROC-ES2H/ssp585/r1i1p4f2/Omon/tos/gn/20230524/tos_Omon_MIROC-ES2H_ssp585_r1i1p4f2_gn_201501-210012_bil_1x1.nc")

# choose the layers from 1985-01-16 and 2100-01-16
hist <- hist[[time(hist) == "1985-01-16"]]
ssp126 <- ssp126[[time(ssp126) == "2100-01-16"]]
ssp585 <- ssp585[[time(ssp585) == "2100-01-16"]]

# crop to below 40 degrees south
hist <- crop(hist, ext(-180, 180, -90, -40))
ssp126 <- crop(ssp126, ext(-180, 180, -90, -40))
ssp585 <- crop(ssp585, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
hist <- project(hist, "epsg:6932")
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get min/max temps across all rasters
range <- as.data.frame(cbind(minmax(hist), minmax(ssp126), minmax(ssp585)))
range <- range %>% pivot_longer(cols = c(1:2), names_to = "rast", values_to = "temp")
min_temp <- min(range$temp)
max_temp <- max(range$temp)

# plot the rasters
hist <- ggplot() + geom_spatraster(data = hist) +
  scale_fill_viridis_c(limits = c(min_temp, max_temp), na.value = "white") +
  theme_void() +
  guides(fill = FALSE)

ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_viridis_c(limits = c(min_temp, max_temp), na.value = "white") +
  theme_void() +
  guides(fill = FALSE)

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_viridis_c(limits = c(min_temp, max_temp), na.value = "white") +
  theme_void() +
  guides(fill = FALSE)

hist
ssp126
ssp585

# export
ggsave("output/schematic/01_hist_jan1985_MIROC_sst.png", hist, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/01_ssp126_jan2100_MIROC_sst.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/01_ssp585_jan2100_MIROC_sst.png", ssp585, width = 10, height = 10, dpi = 300)


#------------------------------------------
# 30 year averages for Jan SST (MIROC-ES2H)
#------------------------------------------

# clear environment
rm(list=ls())

# load in historical, ssp126, and ssp585 rasters
hist <- rast("E:/cmip6_data/CMIP6/CMIP/MIROC/MIROC-ES2H/historical/r1i1p4f2/Omon/tos/gn/20220322/tos_Omon_MIROC-ES2H_historical_r1i1p4f2_gn_185001-201412_bil_1x1.nc")
ssp126 <- rast("E:/cmip6_data/CMIP6/ScenarioMIP/MIROC/MIROC-ES2H/ssp126/r1i1p4f2/Omon/tos/gn/20230524/tos_Omon_MIROC-ES2H_ssp126_r1i1p4f2_gn_201501-210012_bil_1x1.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/ScenarioMIP/MIROC/MIROC-ES2H/ssp585/r1i1p4f2/Omon/tos/gn/20230524/tos_Omon_MIROC-ES2H_ssp585_r1i1p4f2_gn_201501-210012_bil_1x1.nc")

# limit rasters to 30 year timespans
hist <- hist[[time(hist) >= "1985-01-16" & time(hist) <= "2014-02-16"]]
ssp126 <- ssp126[[time(ssp126) >= "2070-01-16" & time(ssp126) <= "2099-02-16"]]
ssp585 <- ssp585[[time(ssp585) >= "2070-01-16" & time(ssp585) <= "2099-02-16"]]

# isolate january months
hist <- hist[[month(time(hist)) == 1]]
ssp126 <- ssp126[[month(time(ssp126)) == 1]]
ssp585 <- ssp585[[month(time(ssp585)) == 1]]

# crop to below 40 degrees south
hist <- crop(hist, ext(-180, 180, -90, -40))
ssp126 <- crop(ssp126, ext(-180, 180, -90, -40))
ssp585 <- crop(ssp585, ext(-180, 180, -90, -40))

# calculate 30-year average
hist <- mean(hist)
ssp126 <- mean(ssp126)
ssp585 <- mean(ssp585)

# project to antarctic stereographic view
hist <- project(hist, "epsg:6932")
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get min/max temps across all rasters
range <- as.data.frame(cbind(minmax(hist), minmax(ssp126), minmax(ssp585)))
range <- range %>% pivot_longer(cols = c(1), names_to = "rast", values_to = "temp")
min_temp <- min(range$temp)
max_temp <- max(range$temp)

# plot the rasters
hist <- ggplot() + geom_spatraster(data = hist) +
  scale_fill_viridis_c(limits = c(min_temp, max_temp), na.value = "white") +
  theme_void() +
  guides(fill = FALSE)

ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_viridis_c(limits = c(min_temp, max_temp), na.value = "white") +
  theme_void() +
  guides(fill = FALSE)

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_viridis_c(limits = c(min_temp, max_temp), na.value = "white") +
  theme_void() +
  guides(fill = FALSE)

hist
ssp126
ssp585

# export
ggsave("output/schematic/02_hist_jan_MIROC_sst_30yravg.png", hist, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/02_ssp126_jan_MIROC_sst_30yravg.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/02_ssp585_jan_MIROC_sst_30yravg.png", ssp585, width = 10, height = 10, dpi = 300)


#------------------------------------------
# Deltas for Jan SST (3 models)
#------------------------------------------

# clear environment
rm(list=ls())

# 1. MIROC-ES2H
# load in ssp126 and ssp585 deltas
ssp126 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp126/MIROC-ES2H_ssp126_tos_delta.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp585/MIROC-ES2H_ssp585_tos_delta.nc")

# isolate january deltas
ssp126 <- ssp126[[month(time(ssp126)) == 1]]
ssp585 <- ssp585[[month(time(ssp585)) == 1]]

# crop to 40 degrees south
ssp126 <- crop(ssp126, ext(-180, 180, -90, -40))
ssp585 <- crop(ssp585, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get min/max temp differences across all rasters
range <- as.data.frame(cbind(minmax(ssp126), minmax(ssp585)))
range <- range %>% pivot_longer(cols = c(1), names_to = "rast", values_to = "temp")
min_temp <- min(range$temp)
max_temp <- max(range$temp)

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = FALSE)

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = FALSE)

# export
ggsave("output/schematic/03_ssp126_jan_MIROC_sst_delta.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/03_ssp585_jan_MIROC_sst_delta.png", ssp585, width = 10, height = 10, dpi = 300)


# 2. CanESM5

# clear environment
rm(list=ls())

# load in ssp126 and ssp585 deltas
ssp126 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp126/CanESM5_ssp126_tos_delta.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp585/CanESM5_ssp585_tos_delta.nc")

# isolate january deltas
ssp126 <- ssp126[[month(time(ssp126)) == 1]]
ssp585 <- ssp585[[month(time(ssp585)) == 1]]

# crop to 40 degrees south
ssp126 <- crop(ssp126, ext(-180, 180, -90, -40))
ssp585 <- crop(ssp585, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get min/max temp differences across all rasters
range <- as.data.frame(cbind(minmax(ssp126), minmax(ssp585)))
range <- range %>% pivot_longer(cols = c(1), names_to = "rast", values_to = "temp")
min_temp <- min(range$temp)
max_temp <- max(range$temp)

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = FALSE)

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = FALSE)

# export
ggsave("output/schematic/03_ssp126_jan_CanESM5_sst_delta.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/03_ssp585_jan_CanESM5_sst_delta.png", ssp585, width = 10, height = 10, dpi = 300)


# 3. UKESM1-0-LL

# clear environment
rm(list=ls())

# load in ssp126 and ssp585 deltas
ssp126 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp126/UKESM1-0-LL_ssp126_tos_delta.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp585/UKESM1-0-LL_ssp585_tos_delta.nc")

# isolate january deltas
ssp126 <- ssp126[[month(time(ssp126)) == 1]]
ssp585 <- ssp585[[month(time(ssp585)) == 1]]

# crop to 40 degrees south
ssp126 <- crop(ssp126, ext(-180, 180, -90, -40))
ssp585 <- crop(ssp585, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get min/max temp differences across all rasters
range <- as.data.frame(cbind(minmax(ssp126), minmax(ssp585)))
range <- range %>% pivot_longer(cols = c(1), names_to = "rast", values_to = "temp")
min_temp <- min(range$temp)
max_temp <- max(range$temp)

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = FALSE)

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = FALSE)

# export
ggsave("output/schematic/03_ssp126_jan_UKESM1-0-LL_sst_delta.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/03_ssp585_jan_UKESM1-0-LL_sst_delta.png", ssp585, width = 10, height = 10, dpi = 300)


#-------------------------------------
# Average Jan SST deltas across models
#-------------------------------------

# clear environment
rm(list=ls())

# load in ssp126 and ssp585 deltas
ssp126 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp126/mean_ssp126_tos_delta.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/deltas/tos/ssp585/mean_ssp585_tos_delta.nc")

# isolate january deltas
ssp126 <- ssp126[[month(time(ssp126)) == 1]]
ssp585 <- ssp585[[month(time(ssp585)) == 1]]

# crop to 40 degrees south
ssp126 <- crop(ssp126, ext(-180, 180, -90, -40))
ssp585 <- crop(ssp585, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get min/max temp differences across all rasters
range <- as.data.frame(cbind(minmax(ssp126), minmax(ssp585)))
range <- range %>% pivot_longer(cols = c(1:2), names_to = "rast", values_to = "temp")
min_temp <- min(range$temp)
max_temp <- max(range$temp)

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = "none")

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_gradient2(limits = c(min_temp, max_temp), na.value = "white",
                       midpoint = 0, mid = "white", low = "steelblue4", high = "darkred") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/04_ssp126_jan_mean_sst_delta.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/04_ssp585_jan_mean_sst_delta.png", ssp585, width = 10, height = 10, dpi = 300)


#-------------------------------------
# GLORYS Jan 2015 SST
#-------------------------------------

# clear environment
rm(list=ls())

# load in sst
sst <- rast("E:/Satellite_Data/monthly/sst/sst.nc")

# isolate january 2015
sst <- sst[[time(sst) == as.Date("2015-01-01")]]

# crop to 40 degrees south
sst <- crop(sst, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
sst <- project(sst, "epsg:6932")

# plot
sst <- ggplot() + geom_spatraster(data = sst) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/05_glorys_jan_2015_sst.png", sst, width = 10, height = 10, dpi = 300)


#---------------------------------------
# GLORYS Jan 2015 SST deltas - projected
#---------------------------------------

# clear environment
rm(list=ls())

# load in sst
ssp126 <- rast("E:/cmip6_data/CMIP6/deltas/tos/satellite_data/monthly_transformed_ssp126.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/deltas/tos/satellite_data/monthly_transformed_ssp585.nc")

# isolate january 2015
ssp126 <- ssp126[[time(ssp126) == as.Date("2015-01-01")]]
ssp585 <- ssp585[[time(ssp585) == as.Date("2015-01-01")]]

# crop to 40 degrees south
ssp126 <- crop(ssp126, ext(-180, 180, -90, -40))
ssp585 <- crop(ssp585, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get min/max temps across all rasters
range <- as.data.frame(cbind(minmax(ssp126), minmax(ssp585)))
range <- range %>% pivot_longer(cols = c(1:2), names_to = "rast", values_to = "temp")
min_temp <- min(range$temp)
max_temp <- max(range$temp)

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/06_ssp126_jan_2015_sst_projected.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/06_ssp585_jan_2015_sst_projected.png", ssp585, width = 10, height = 10, dpi = 300)


#---------------------------------------------------------------------
# Projected habitat suitability - Jan 2015, 2016, and 2017 (scenarios)
#---------------------------------------------------------------------

# clear environment
rm(list=ls())

# load in projected suitability rasters
ssp126 <- rast("output/projections/ssp126/projections_ssp126.nc")
ssp585 <- rast("output/projections/ssp585/projections_ssp585.nc")

# isolate january 2015
ssp126 <- ssp126[[time(ssp126) == as.Date("2015-01-01")]]
jan15ssp585 <- ssp585[[time(ssp585) == as.Date("2015-01-01")]]
jan16ssp585 <- ssp585[[time(ssp585) == as.Date("2016-01-01")]]
jan17ssp585 <- ssp585[[time(ssp585) == as.Date("2017-01-01")]]

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
jan15ssp585 <- project(jan15ssp585, "epsg:6932")
jan16ssp585 <- project(jan16ssp585, "epsg:6932")
jan17ssp585 <- project(jan17ssp585, "epsg:6932")

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

jan15ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

jan16ssp585 <- ggplot() + geom_spatraster(data = jan16ssp585) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

jan17ssp585 <- ggplot() + geom_spatraster(data = jan17ssp585) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/07_ssp126_jan_2015_suitability_projected.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/07_ssp585_jan_2015_suitability_projected.png", jan15ssp585, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/07_ssp585_jan_2016_suitability_projected.png", jan16ssp585, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/07_ssp585_jan_2017_suitability_projected.png", jan17ssp585, width = 10, height = 10, dpi = 300)

#---------------------------------------------
# Projected habitat suitability - January Mean
#---------------------------------------------

# clear environment
rm(list=ls())

# load in projected suitability rasters
ssp126 <- rast("output/projections/ssp126/monthly_avg_January_ssp126.nc")
ssp585 <- rast("output/projections/ssp585/monthly_avg_January_ssp585.nc")

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/08_ssp126_jan_mean_suitability_projected.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/08_ssp585_jan_mean_suitability_projected.png", ssp585, width = 10, height = 10, dpi = 300)


#----------------------------------------------------
# Projected habitat suitability - January Differences
#----------------------------------------------------

# clear environment
rm(list=ls())

# load in projected suitability difference rasters
ssp126 <- rast("output/differences/ssp126/ssp126_diffs.nc")
ssp585 <- rast("output/differences/ssp585/ssp585_diffs.nc")

# isolate january
ssp126 <- ssp126[[time(ssp126) == as.Date("2020-01-01")]]
ssp585 <- ssp585[[time(ssp585) == as.Date("2020-01-01")]]

# project to antarctic stereographic view
ssp126 <- project(ssp126, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# plot
ssp126 <- ggplot() + geom_spatraster(data = ssp126) +
  scale_fill_gradient2(low = "darkred", high = "steelblue4", na.value = "white", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_void() +
  guides(fill = "none")

ssp585 <- ggplot() + geom_spatraster(data = ssp585) +
  scale_fill_gradient2(low = "darkred", high = "steelblue4", na.value = "white", midpoint = 0,
                       limits = c(-1, 1)) +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/09_ssp126_jan_diffs_suitability_projected.png", ssp126, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/09_ssp585_jan_diffs_suitability_projected.png", ssp585, width = 10, height = 10, dpi = 300)


#--------------------
# Glorys Covariates
#--------------------

# clear environment
rm(list=ls())

# load in covariate rasters
sst <- rast("E:/Satellite_Data/daily/sst/sst_2015.nc")
ssh <- rast("E:/Satellite_Data/daily/ssh/ssh_2015.nc")
sic <- rast("E:/Satellite_Data/daily/sic/sic_2015.nc")
depth <- rast("E:/Satellite_Data/static/depth/depth.nc")
slope <- rast("E:/Satellite_Data/static/slope/slope.nc")

# isolate 1st January 2015
sst <- sst[[time(sst) == as.Date("2015-01-01")]]
ssh <- ssh[[time(ssh) == as.Date("2015-01-01")]]
sic <- sic[[time(sic) == as.Date("2015-01-01")]]

# crop to 40 degrees south
sst <- crop(sst, ext(-180, 180, -90, -40))
ssh <- crop(ssh, ext(-180, 180, -90, -40))
sic <- crop(sic, ext(-180, 180, -90, -40))
depth <- crop(depth, ext(-180, 180, -90, -40))
slope <- crop(slope, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
sst <- project(sst, "epsg:6932")
ssh <- project(ssh, "epsg:6932")
sic <- project(sic, "epsg:6932")
depth <- project(depth, "epsg:6932")
slope <- project(slope, "epsg:6932")

# plot
sst <- ggplot() + geom_spatraster(data = sst) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

ssh <- ggplot() + geom_spatraster(data = ssh) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

sic <- ggplot() + geom_spatraster(data = sic) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

depth <- ggplot() + geom_spatraster(data = depth) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

slope <- ggplot() + geom_spatraster(data = slope) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/00_sst_jan2015_glorys.png", sst, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/00_ssh_jan2015_glorys.png", ssh, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/00_sic_jan2015_glorys.png", sic, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/00_depth_glorys.png", depth, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/00_slope_glorys.png", slope, width = 10, height = 10, dpi = 300)


#-----------------------------
# Jan 2015 habitat suitability
#-----------------------------

# clear environment
rm(list=ls())

# load in habitat suitability raster
pred <- rast("output/predictions/predictions.nc")

# isolate january 2015
pred <- pred[[time(pred) == as.Date("2015-01-01")]]

# project to antarctic stereographic view
pred <- project(pred, "epsg:6932")

# plot
pred <- ggplot() + geom_spatraster(data = pred) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/00_suitability_jan2015.png", pred, width = 10, height = 10, dpi = 300)


#--------------------------------
# Jan avg habitat suitability
#--------------------------------

# clear environment
rm(list=ls())

# load in habitat suitability raster
pred <- rast("output/predictions/monthly_avg_January.nc")

# project to antarctic stereographic view
pred <- project(pred, "epsg:6932")

# plot
pred <- ggplot() + geom_spatraster(data = pred) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/00_suitability_jan_avg.png", pred, width = 10, height = 10, dpi = 300)


#----------------
# Humpback Tracks
#----------------

# clear environment
rm(list=ls())

# load depth for plots
depth <- rast("E:/Satellite_Data/static/depth/depth.nc")

# load tracking data for each population
ea <- readRDS("data/tracks_by_region/EastAtlantic_mpm_6.RDS")
wa <- readRDS("data/tracks_by_region/WestAtlantic_mpm_6.RDS")
ei <- readRDS("data/tracks_by_region/EastIndian_mpm_6.RDS")
wi <- readRDS("data/tracks_by_region/WestIndian_mpm_6.RDS")
wp <- readRDS("data/tracks_by_region/WestPacific_mpm_6.RDS")
ep <- readRDS("data/tracks_by_region/EastPacific_mpm_6.RDS")
p <- readRDS("data/tracks_by_region/Pacific_mpm_6.RDS")

# join all together
tracks <- rbind(ea, wa, ei, wi, wp, ep, p)

# cleanup
rm(ea, wa, ei, wi, wp, ep, p)

# filter to 40 degrees south
tracks <- tracks %>% filter(y < -40)

# make tracks into spatvector
tracks <- tracks %>%
  vect(geom = c("x", "y"), crs = "epsg:4326")

# project to antarctic stereographic view
tracks <- project(tracks, "epsg:6932")

# crop depth to 40 degrees south
depth <- crop(depth, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
depth <- project(depth, "epsg:6932")

#fill to one colour
depth[] <- ifelse(!is.na(depth[]), 1, NA)

# plot
humpies <- ggplot() + geom_spatraster(data = depth) +
  geom_spatvector(data = tracks, aes(col = region), size = 0.1) +
  scale_fill_viridis_c(na.value = "white") +
  scale_color_manual(values = c("#440154FF", "#481F70FF", "#443A83FF", "#365D8DFF", "#47C16EFF", "#8FD744FF", "#FDE725FF")) +
  theme_void() +
  guides(fill = "none", col = "none")

# export
ggsave("output/schematic/00_humpback_tracks.png", humpies, width = 10, height = 10, dpi = 300)


#-----------------------------------------
# regional model predictions - Jan 01 2015
#-----------------------------------------

# predictions taken from 06c_circumpolar_predictions.R
plot(wa)
plot(ei)
plot(ep)

# project to antarctic stereographic view
wa <- project(wa, "epsg:6932")
ei <- project(ei, "epsg:6932")
ep <- project(ep, "epsg:6932")

# plot
wa <- ggplot() + geom_spatraster(data = wa) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

ei <- ggplot() + geom_spatraster(data = ei) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

ep <- ggplot() + geom_spatraster(data = ep) +
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  guides(fill = "none")

# export
ggsave("output/schematic/00_wa_predictions.png", wa, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/00_ei_predictions.png", ei, width = 10, height = 10, dpi = 300)
ggsave("output/schematic/00_ep_predictions.png", ep, width = 10, height = 10, dpi = 300)


#-----------------------------------------
# Background samples
#-----------------------------------------

# clear environment
rm(list=ls())

# load in background samples
ea <- readRDS("output/background/EastAtlantic_background.RDS")
wa <- readRDS("output/background/WestAtlantic_background.RDS")
ei <- readRDS("output/background/EastIndian_background.RDS")
wi <- readRDS("output/background/WestIndian_background.RDS")
wp <- readRDS("output/background/WestPacific_background.RDS")
ep <- readRDS("output/background/EastPacific_background.RDS")
p <- readRDS("output/background/Pacific_background.RDS")

# join all together
background <- rbind(ea, wa, ei, wi, wp, ep, p)

# cleanup
rm(ea, wa, ei, wi, wp, ep, p)

# make into spatvector
background <- background %>%
  vect(geom = c("x", "y"), crs = "epsg:4326")

# project to antarctic stereographic view
background <- project(background, "epsg:6932")

# get depth raster
depth <- rast("E:/Satellite_Data/static/depth/depth.nc")

# crop depth to 40 degrees south
depth <- crop(depth, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
depth <- project(depth, "epsg:6932")

#fill to one colour
depth[] <- ifelse(!is.na(depth[]), 1, NA)

# remove background points that overla with depth NAs
background$depth <- extract(depth, background, ID=F)
background <- background %>% filter(!is.na(depth))

# plot
background <- ggplot() + geom_spatraster(data = depth) +
  geom_spatvector(data = background, aes(col = region), size = 0.1) +
  scale_fill_viridis_c(na.value = "white") +
  scale_color_manual(values = c("#440154FF", "#481F70FF", "#443A83FF", "#365D8DFF", "#47C16EFF", "#8FD744FF", "#FDE725FF")) +
  theme_void() +
  guides(fill = "none", col = "none")

# export
ggsave("output/schematic/00_background_samples.png", background, width = 10, height = 10, dpi = 300)
