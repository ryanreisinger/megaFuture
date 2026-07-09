#-------------------------------------------------------------------------------
# Fig. 2 - Data Map
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(terra)
library(tidyterra)

# read in tracking data
tracks <- readRDS("data/all_tracks.RDS")

# limit to 40 degrees south
tracks <- tracks %>%
  filter(y < -40)

# convert to terra
trax <- vect(tracks, geom=c("x", "y"), crs="EPSG:4326") %>%
  project("epsg:6932")

# read in coastline
coast <- readRDS("data/coast_ice_vect.RDS")

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
trax_lines <- vect(trax_lines)

# plot
p1 <- ggplot() +
  geom_spatvector(data = trax_lines, aes(col = population), linewidth = .5) +
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
ggsave("output/imagery/tracking_map/tracks.png",
       p1, width = 8, height = 8, dpi = 300)


