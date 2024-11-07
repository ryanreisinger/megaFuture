rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(terra)
  library(tidyverse)
  library(tidyterra)
}

#load in tracks and metadata
tracks <- readRDS("data/aniMotum_mpm_6.RDS")
meta <- read.csv("data/metadata.csv")

#append region to tracks
tracks <- tracks %>%
  rename(individual_id = id_original) %>%
  left_join(select(meta, individual_id, region))

#check for NA
na_check <- tracks %>%
  filter(is.na(region)) %>%
  mutate(individual_id = as.factor(individual_id))
levels(na_check$individual_id)

#all NAs are Mn - fix individual_ids and assign to relevant stock
na_check <- na_check %>% 
  mutate(individual_id = as.character(substr(idtid, 1, 15))) %>%
  select(-region) %>%
  left_join(select(meta, individual_id, region))

#rejoin NA tracks to all
tracks <- tracks %>% 
  anti_join(na_check, by = "id") %>%
  bind_rows(na_check)

#plot by region
tracks_terra <- tracks %>%
  vect(geom = c("x", "y"),
       crs = "epsg:4326")
ggplot() + geom_spatvector(data = tracks_terra, aes(color = region), size = 0.5) + theme_minimal()

#identify each region
tracks <- tracks %>% 
  mutate(region = as.factor(region))
regions <- levels(tracks$region)

#split tracks into regions and save
for(i in regions){
  tracks_region <- tracks %>% 
    filter(region == i)
  
  saveRDS(tracks_region, paste0("data/", i, "_mpm_6.RDS"))
}
