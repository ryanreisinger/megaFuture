#-------------------------------------------------
# Subtract monthly average predictions from projections
#-------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}

#loop over each month
for(i in c(10, 11, 12, 1, 2, 3, 4, 5)){
  
  #define month
  this.month <- i
  
  #define longname for month for reading files
  longname <- as.character(month(this.month, label=T, abbr=F))
  
  #read in monthly predictions and projections
  modern <- rast(paste0("output/predictions/monthly_avg_", longname, ".nc"))
  ssp126 <- rast(paste0("output/projections/ssp126/monthly_avg_", longname, "_ssp126.nc"))
  ssp585 <- rast(paste0("output/projections/ssp585/monthly_avg_", longname, "_ssp585.nc"))
  
  #resample ssp scenarios to modern grid
  ssp126 <- resample(ssp126, modern, method = "bilinear", threads = T)
  ssp585 <- resample(ssp585, modern, method = "bilinear", threads = T)
  
  #calculate differences
  diff126 <- ssp126 - modern
  diff585 <- ssp585 - modern
  
  #plot differences
  diff126_ant <- project(diff126, "epsg:6932")
  p1 <- ggplot() + geom_spatraster(data = diff126_ant) +
    scale_fill_gradient2() + theme_void() +
    ggtitle(paste0(longname, " Average")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(p1)
  
  diff585_ant <- project(diff585, "epsg:6932")
  p2 <- ggplot() + geom_spatraster(data = diff585_ant) +
    scale_fill_gradient2() + theme_void() +
    ggtitle(paste0(longname, " Average")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(p2)
  
  #export plots
  ggsave(paste0("output/differences/ssp126/", longname, ".png"), p1, width = 6, height = 6, dpi = 300)
  ggsave(paste0("output/differences/ssp585/", longname, ".png"), p2, width = 6, height = 6, dpi = 300)
  
  #join rasters to other months
  if(!exists("ssp126_diffs")){
    ssp126_diffs <- diff126
    ssp585_diffs <- diff585
  } else {
    ssp126_diffs <- c(ssp126_diffs, diff126)
    ssp585_diffs <- c(ssp585_diffs, diff585)
  }
  
}

#create time information
for(i in c(10, 11, 12, 1, 2, 3, 4, 5)){
  
  #define month
  this.month <- i
  
  #create date
  date <- as_date(paste0("2020-", this.month, "-01"))
  
  #join to all dates 
  if(!exists("all_dates")){
    all_dates <- date
  } else {
    all_dates <- c(all_dates, date)
  }
}

#assign times
time(ssp126_diffs) <- all_dates
time(ssp585_diffs) <- all_dates
  
#export difference rasters
writeCDF(ssp126_diffs, "output/differences/ssp126/ssp126_diffs.nc", overwrite = T)
writeCDF(ssp585_diffs, "output/differences/ssp585/ssp585_diffs.nc", overwrite = T)

