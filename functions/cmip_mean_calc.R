##' cmip_mean_calc
##'
##' Calculate average 30-year values for historical and future CMIP6 scenarios
##'
##' @title cmic_mean_calc
##' @param scenario the CMIP6 scenario, e.g. "historical", "ssp126", "ssp585"
##'
##' @import tidyverse
##' 
##' @export

cmip_mean_calc <- function(scenario){
  
  #identify subdirectory name
  numeric_directory <- list.dirs()[2]
  
  #list all regridded nc files
  files <- list.files(numeric_directory)
  regridded <- subset(files, grepl("bil_1x1.nc$", files))
  
  #join all files together into one raster
  for(i in 1:length(regridded)){
    
    #load in each regridded file
    which_file <- regridded[i]
    r <- rast(paste0(numeric_directory, "/", which_file))
    
    #skip file if time range is not within desired range
    if(scenario == "historical" & max(year(time(r))) < 1985){
      next
    }
    
    if(scenario != "historical" & max(year(time(r))) < 2071){
      next
    }
    
    #join all rasters
    if(!exists("full_rast")){
      full_rast <- r
    } else {
      full_rast <- c(full_rast, r)
    }
  }
  
  #clip time range
  if(scenario == "historical"){
    full_rast <- full_rast[[year(time(full_rast)) >= 1985]]
  }
  
  if(scenario != "historical"){
    full_rast <- full_rast[[year(time(full_rast)) >= 2071 & year(time(full_rast)) <= 2100]]
  }
  
  #calculate average values for this period
  mean_rast <- mean(full_rast, na.rm = TRUE)
}

#return mean raster
return(mean_rast)