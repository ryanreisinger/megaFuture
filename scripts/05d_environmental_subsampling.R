# environmental subsampling following Pili et al. (2025) 
# https://doi.org/10.1002/ecog.08002

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(dplyr)
  library(lubridate)
  library(terra)
  library(tidyterra)
  library(sf)
  library(umap)
  library(dbscan)
}

# read in extracted data
data <- readRDS("output/extraction/extracted.RDS")

# candidate variables
preds <- c("depth", "slope", "sst", "sal", 
           "sic", "curr", "mld", "dshelf")

# select variables and other key info
data <- data %>%
  select(all_of(preds), pb, region, date, x, y)

# remove NAs
data <- data %>%
  na.omit()

# isolate background data
bg <- data %>%
  filter(pb == "background")

# if dataset is bigger than 30,000 points, randomly sample background points to 30,000 [to work with dbscan]
if(nrow(bg) > 30000){
  bg <- sample_n(bg, 30000)
}

# rejoin presence data
data <- bind_rows(data %>% filter(pb == "presence"), bg)

# scale the environmental data
scaled <- data %>%
  select(all_of(preds)) %>%
  mutate(across(all_of(preds), scale))

# dimensionality reduction with umap
umap_config <- umap.defaults
umap_config$random_state <- 7
umap_config$n_neighbors <- 5
umap_config$n_components <- 5

scaled_umap <- umap(scaled, config = umap_config)$layout %>%
  data.frame()

# clustering with dbscan
set.seed(777)
cluster <- hdbscan(scaled_umap, minPts = 2)$cluster

# bind cluster info
data <- data %>%
  bind_cols(cluster = cluster)

# isolate points not assigned to a cluster
zeroes <- data %>%
  filter(cluster == 0)

# filter to 1 location per cluster
data <- data %>%
  group_by(cluster, pb) %>%
  sample_n(1) %>%
  ungroup() %>%
  filter(cluster != 0)

# join the data
all <- bind_rows(data, zeroes) %>% 
  select(-cluster)

# convert to terra
allx <- all %>% vect(geom = c("x", "y"), crs = "epsg:4326") %>%
  project("epsg:6932")
plot(allx, pch = ".")

# export
saveRDS(all, "output/subsampled/subsampled.RDS")
