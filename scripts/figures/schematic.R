#-------------------------------------------------------------------------------
# Humpback Methods Schematic
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(tidyterra)

# This schematic flows as two streams - one outlining present predictions and
# the other outlining future predictions

# Stream 1 contains the following steps:
# 1. The state-space-modelled tracking data
# 2. Background samples
# 3. Environmental variables
# 4. Algorithm predictions
# 5. Ensemble output

# Steam 2 contains the following steps:
# 1. Future and historical GCM data
# 2. The delta layer for that GCM
# 3. Future environmental variables
# 4. Algorithm predictions
# 5. Ensemble output

# At the end there will be a bonus section showing GCM averaging
# 1. Future predictions from multiple GCMs
# 2. Ensemble mean and standard deviation


#-------------------------------------------------------------------------------
# Stream 1: Present Predictions
#-------------------------------------------------------------------------------

# 1. Tracking Data

# load in tracking data
tracks <- readRDS("data/all_tracks.RDS")

# limit to 40 degrees south
tracks <- tracks %>%
  filter(y < -40)

# convert to terra
trax <- vect(tracks, geom=c("x", "y"), crs="EPSG:4326") %>%
  project("epsg:6932")

# format population 
trax <- trax %>%
  mutate(population = factor(region, levels = c("WestAtlantic", "EastAtlantic",
                                                "WestIndian", "EastIndian",
                                                "WestPacific", "Pacific", "EastPacific"))) %>%
  mutate(population = recode(population,
                             "WestAtlantic" = "West Atlantic",
                             "EastAtlantic" = "East Atlantic",
                             "WestIndian" = "West Indian",
                             "EastIndian" = "East Indian",
                             "WestPacific" = "West Pacific",
                             "Pacific" = "Central Pacific",
                             "EastPacific" = "East Pacific"))

# create lines for each individual in the tracks - this is slow!!!
inds <- unique(trax$id)
for(i in 1:length(inds)){
  ind <- inds[i]
  ind_trax <- trax %>%
    filter(id == ind)
  this_pop <- unique(ind_trax$population)
  ind_trax <- as.lines(ind_trax)
  ind_trax$population <- this_pop
  ind_trax$id <- ind
  if(ind == inds[1]){
    trax_lines <- ind_trax
  } else {
    trax_lines <- c(trax_lines, ind_trax)
  }
  if(i %% 10 == 0){
    print(paste0(i, "/", length(inds)))
  }
}
trax <- vect(trax_lines)

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p1 <- ggplot() +
  geom_spatvector(data = trax, aes(col = population), linewidth = .5) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  theme_void() +
  scale_fill_manual(values = c("grey90", "grey70"), guide = "none") +
  scale_color_manual(values = c("#EEBF6D", "#DB941A",
                                "#00B4CC", "#007E8F",
                                "#ECA498", "#DB5943", "#892B1A"),
                     guide = "none") +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA)) 
p1 + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/1.1 Tracking Data.png", p1,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


# 2. Background Samples

# read in subsampled data
data <- readRDS("output/subsampled/subsampled.RDS")

# convert to terra
datax <- data %>% vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")

# isolate background locations
n <- nrow(data %>% filter(pb == "presence"))
background <- datax %>%
  filter(pb == "background") %>%
  slice_sample(n = n)
nrow(background)

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p2 <- ggplot() +
  geom_spatvector(data = background, col = "#DB5943", size = 1) +
  geom_spatvector(data = coast, aes(fill = surface), col = NA) +
  theme_void() +
  scale_fill_manual(values = c("grey90", "grey70"), guide = "none") +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA)) 
p2 + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/1.2 Background Samples.png", p2,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


# 3. Environmental Variables

# load in covariate rasters
sst <- rast("E:/Satellite_Data/daily/sst/sst_2015.nc")
sic <- rast("E:/Satellite_Data/daily/sic/sic_2015.nc")
depth <- rast("E:/Satellite_Data/static/depth/depth.nc")

# isolate 1st January 2015
sst <- sst[[time(sst) == as.Date("2015-01-01")]]
sic <- sic[[time(sic) == as.Date("2015-01-01")]]

# crop to 40 degrees south
sst <- crop(sst, ext(-180, 180, -90, -40))
sic <- crop(sic, ext(-180, 180, -90, -40))
depth <- crop(depth, ext(-180, 180, -90, -40))

# assign CRS
crs(sst) <- "epsg:4326"
crs(sic) <- "epsg:4326"
crs(depth) <- "epsg:4326"

# project to antarctic stereographic view
sst <- project(sst, "epsg:6932")
sic <- project(sic, "epsg:6932")
depth <- project(depth, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p3a <- ggplot() + geom_spatraster(data = sst) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA)) 
p3a + ggview::canvas(width = 8, height = 8)

p3b <- ggplot() + geom_spatraster(data = sic) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA)) 
p3b + ggview::canvas(width = 8, height = 8)

p3c <- ggplot() + geom_spatraster(data = depth) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p3c + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/1.3a Environmental Variable - SST.png", p3a,
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/1.3b Environmental Variable - SIC.png", p3b,
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/1.3c Environmental Variable - Depth.png", p3c,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


# 4. Algorithm Predictions

# read in algorithm predictions
rf <- rast("output/predictions/rf_prediction.tif")
brt <- rast("output/predictions/brt_prediction.tif")
bart <- rast("output/predictions/bart_prediction.tif")

# limit to January 
rf <- rf[[time(rf) == as_date("2010-01-01")]]
brt <- brt[[time(brt) == as_date("2010-01-01")]]
bart <- bart[[time(bart) == as_date("2010-01-01")]]

# project to antarctic stereographic view
rf <- project(rf, "epsg:6932")
brt <- project(brt, "epsg:6932")
bart <- project(bart, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p4a <- ggplot() + geom_spatraster(data = rf) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA)) 
p4a + ggview::canvas(width = 8, height = 8)

p4b <- ggplot() + geom_spatraster(data = brt) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA)) 
p4b + ggview::canvas(width = 8, height = 8)

p4c <- ggplot() + geom_spatraster(data = bart) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA)) 
p4c + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/1.4a Algorithm Prediction - RF.png", p4a,
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/1.4b Algorithm Prediction - BRT.png", p4b,
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/1.4c Algorithm Prediction - BART.png", p4c,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


# 5. Ensemble Output

# read in ensemble prediction
pred <- rast("output/predictions/ensemble_prediction.tif")

# isolate January
pred <- pred[[time(pred) == as_date("2000-01-15")]]

# project to antarctic stereographic view
pred <- project(pred, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p5 <- ggplot() + geom_spatraster(data = pred) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p5 + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/1.5 Ensemble Output.png", p5,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


#-------------------------------------------------------------------------------
# Stream 2: Future Predictions
#-------------------------------------------------------------------------------

# 1. Future and Historical GCM Data 

# load in historical and future data (CanESM5)
hist <- rast("E:/cmip6_data/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/Omon/tos/gn/20190429/tos_Omon_CanESM5_historical_r1i1p1f1_gn_185001-201412__climatology.nc")
ssp585 <- rast("E:/cmip6_data/CMIP6/ScenarioMIP/CCCma/CanESM5/ssp585/r1i1p1f1/Omon/tos/gn/20190429/tos_Omon_CanESM5_ssp585_r1i1p1f1_gn_201501-210012__climatology.nc")

# limit to January
hist <- hist[[1]]
ssp585 <- ssp585[[1]]

# crop to 40 degrees south
e <- ext(-180, 180, -90, -40)
hist <- crop(hist, e)
ssp585 <- crop(ssp585, e)

# project to antarctic stereographic view
hist <- project(hist, "epsg:6932")
ssp585 <- project(ssp585, "epsg:6932")

# get minmax values
min <- min(c(minmax(hist)[1,], minmax(ssp585)[1,]))
max <- max(c(minmax(hist)[2,], minmax(ssp585)[2,]))

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p6a <- ggplot() +
  geom_spatraster(data = hist) +
  geom_spatvector(data = coast, fill = "grey80", color = NA) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none",
                       limits = c(min, max)) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p6a + ggview::canvas(width = 8, height = 8)

p6b <- ggplot() +
  geom_spatraster(data = ssp585) +
  geom_spatvector(data = coast, fill = "grey80", color = NA) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none",
                       limits = c(min, max)) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p6b + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/2.1a GCM Historical Data.png", p6a, 
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/2.1b GCM Future Data.png", p6b, 
       width = 8, height = 8, units = "in", bg = "transparent")


# 2. Delta Layer

# calculate delta layer from hist and ssp585
delta <- ssp585 - hist

# plot
p7 <- ggplot() +
  geom_spatraster(data = delta) +
  geom_spatvector(data = coast, fill = "grey80", color = NA) +
  scale_fill_gradient2(na.value = "transparent", low = "steelblue4", high = "red4",
                       guide = "none") +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p7 + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/2.2 Delta Layer.png", p7,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


# 3. Future Environmental Variables

# load in covariate rasters
sst <- rast("E:/cmip6_data/CMIP6/deltas/tos/satellite_data/transformed/CanESM5_ssp585_glorysres.tif")
sic <- rast("E:/cmip6_data/CMIP6/deltas/siconc/satellite_data/transformed/CanESM5_ssp585_glorysres.tif")

# limit to January 2015
sst <- sst[[time(sst) == as.Date("2015-01-01")]]
sic <- sic[[time(sic) == as.Date("2015-01-01")]]

# crop to 40 degrees south
sst <- crop(sst, ext(-180, 180, -90, -40))
sic <- crop(sic, ext(-180, 180, -90, -40))

# project to antarctic stereographic view
sst <- project(sst, "epsg:6932")
sic <- project(sic, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p8a <- ggplot() + geom_spatraster(data = sst) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p8a + ggview::canvas(width = 8, height = 8)

p8b <- ggplot() + geom_spatraster(data = sic) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p8b + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/2.3a Future Environmental Variable - SST.png", p8a,
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/2.3b Future Environmental Variable - SIC.png", p8b,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


# 4. Algorithm Predictions

# DUMPED THIS SECTION FOR NOW


# 5. Ensemble Output

# read in ensemble prediction
proj <- rast("output/projections/ssp585/CanESM5/ensemble_prediction.tif")

# isolate January
proj <- proj[[time(proj) == as_date("2010-01-01")]]

# project to antarctic stereographic view
proj <- project(proj, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p10 <- ggplot() + geom_spatraster(data = proj) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p10 + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/2.5 Future Ensemble Output.png", p10,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up
rm(list=ls())


#-------------------------------------------------------------------------------
# Bonus: GCM Averaging
#-------------------------------------------------------------------------------

# 1. Future Predictions from Multiple GCMs

# read in predictions for UKESM1-0-LL and IPSL-CM6A-LR
ukesm <- rast("output/projections/ssp585/UKESM1-0-LL/ensemble_prediction.tif")
ipsl <- rast("output/projections/ssp585/IPSL-CM6A-LR/ensemble_prediction.tif")

# isolate January
ukesm <- ukesm[[time(ukesm) == as_date("2010-01-01")]]
ipsl <- ipsl[[time(ipsl) == as_date("2010-01-01")]]

# project to antarctic stereographic view
ukesm <- project(ukesm, "epsg:6932")
ipsl <- project(ipsl, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p11 <- ggplot() + geom_spatraster(data = ukesm) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p11 + ggview::canvas(width = 8, height = 8)

p12 <- ggplot() + geom_spatraster(data = ipsl) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p12 + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/3.1a GCM Future Prediction - UKESM1-0-LL.png", p11,
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/3.1b GCM Future Prediction - IPSL-CM6A-LR.png", p12,
       width = 8, height = 8, units = "in", bg = "transparent")

# clean up 
rm(list=ls())


# 2. Ensemble Mean and Standard Deviation

# read in all eight GCM ensemble predictions
canesm <- rast("output/projections/ssp585/CanESM5/ensemble_prediction.tif")
ukesm <- rast("output/projections/ssp585/UKESM1-0-LL/ensemble_prediction.tif")
ipsl <- rast("output/projections/ssp585/IPSL-CM6A-LR/ensemble_prediction.tif")
nor <- rast("output/projections/ssp585/NorESM2-MM/ensemble_prediction.tif")
mri <- rast("output/projections/ssp585/MRI-ESM2-0/ensemble_prediction.tif")
hadgem <- rast("output/projections/ssp585/HadGEM3-GC31-LL/ensemble_prediction.tif")
cesm2 <- rast("output/projections/ssp585/CESM2-WACCM/ensemble_prediction.tif")
access <- rast("output/projections/ssp585/ACCESS-ESM1-5/ensemble_prediction.tif")

# stack them
gcm_stack <- c(canesm, ukesm, ipsl, nor, mri, hadgem, cesm2, access)

# limit to January
gcm_stack <- gcm_stack[[time(gcm_stack) == as_date("2010-01-01")]]

# calculate mean and sd
mean <- app(gcm_stack, mean, na.rm = T)
sd <- app(gcm_stack, sd, na.rm = T)

# project to antarctic stereographic view
mean <- project(mean, "epsg:6932")
sd <- project(sd, "epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

# plot
p13 <- ggplot() + geom_spatraster(data = mean) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p13 + ggview::canvas(width = 8, height = 8)

# plot
p14 <- ggplot() + geom_spatraster(data = sd) +
  scale_fill_viridis_c(na.value = "transparent", guide = "none") +
  geom_spatvector(data = coast, fill = "grey80", col = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA, color = NA))
p14 + ggview::canvas(width = 8, height = 8)

# export
ggsave("output/schematic/3.2a GCM Ensemble Mean.png", p13,
       width = 8, height = 8, units = "in", bg = "transparent")
ggsave("output/schematic/3.2b GCM Ensemble Standard Deviation.png", p14,
       width = 8, height = 8, units = "in", bg = "transparent")
