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
    
    if(scenario != "historical" & max(year(time(r))) < 2070){
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
    full_rast <- full_rast[[year(time(full_rast)) >= 2070 & year(time(full_rast)) <= 2099]]
  }
  
  #calculate average per month
  for(i in 1:12){
    this_month <- i
    
    #extract all rasters for this month
    monthly_rast <- full_rast[[month(time(full_rast)) == this_month]]
    
    #check that there are exactly 30 rasters
    if(nlyr(monthly_rast) != 30){
      stop(paste0("Error: not exactly 30 rasters for month", this_month))
    }
    
    #calculate average values for this month
    month_mean <- mean(monthly_rast, na.rm = TRUE)
    
    #assign a time as the first time of the monthly rast
    time(month_mean) <- time(monthly_rast)[1]
    
    #join to all monthly means
    if(!exists("mean_rast")){
      mean_rast <- month_mean
    } else {
      mean_rast <- c(mean_rast, month_mean)
    }
  }
  
  #return mean raster
  return(mean_rast)
}
