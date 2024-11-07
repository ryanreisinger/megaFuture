#create monthly average predictions (Oct to May)

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(terra)
  library(tidyterra)
}

#load all predictions
preds <- rast("output/predictions/predictions.nc")

#list all months 
months <- c(10, 11, 12, 1, 2, 3, 4, 5)

#for each month
for(i in months){
  
  #define month
  this.month <- i
  
  #subset predictions to this month
  monthly_preds <- preds[[month(time(preds)) == this.month]]
  
  #calculate monthly average
  monthly_avg <- mean(monthly_preds, na.rm = TRUE)
  
  #extract written month name
  longname <- as.character(month(time(monthly_preds[[1]]), label=T, abbr=F))
  
  #export
  writeCDF(monthly_avg, filename = paste0("output/predictions/monthly_avg_", longname, ".nc"))
  
  #reproject
  monthly_avg <- project(monthly_avg, "epsg:6932")
  g1 <- ggplot() + geom_spatraster(data=monthly_avg) + theme_void() + 
    scale_fill_viridis_c(na.value="white", name = "Habitat Suitability") + 
    ggtitle(paste0(longname, " Average")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(g1)
  
  #export
  ggsave(paste0("output/predictions/monthly_avg_", longname, ".png"), 
         g1, width=10, height=8)
}
