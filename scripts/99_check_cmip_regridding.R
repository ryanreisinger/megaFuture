#visualise to check cdo regridding performance

setwd("E:/cmip6_data")
library(terra)

#define variable
which_var <- "uas"

#define frequency name - Amon, Omon, or SImon
which_freq <- "Amon"

#define scenario 
which_scenario <- "historical"

#check model by model

#####################
### AWI-CM-1-1-MR ###
#####################

#set working directory to this model
setwd(paste0("CMIP6/CMIP/AWI/AWI-CM-1-1-MR/historical/r1i1p1f1/", which_freq, "/", which_var, "/gn/20200511"))

#list available files
files <- list.files()

#identify the first regridded file and plot 
regridded <- subset(files, grepl("bil_1x1.nc$", files))
regrid <- regridded[1]
regrid <- rast(regrid)
plot(regrid[[1]])

#check against the first native file
native <- subset(files, !files %in% regridded)
nat <- native[1]
nat <- rast(nat)
plot(nat[[1]])


##################
### CAS-ESM2-0 ###
##################

#set working directory to this model
setwd(paste0("E:/cmip6_data/CMIP6/CMIP/CAS/CAS-ESM2-0/historical/r1i1p1f1/", which_freq, "/", which_var, "/gn/20201227"))

#list available files
files <- list.files()

#identify the first regridded file and plot
regridded <- subset(files, grepl("bil_1x1.nc$", files))
regrid <- regridded[1]
regrid <- rast(regrid)
plot(regrid[[1]])

#check against the first native file
native <- subset(files, !files %in% regridded)
nat <- native[1]
nat <- rast(nat)
plot(nat[[1]])


####################
### CMCC-CM2-SR5 ###
####################

#set working directory to this model
setwd(paste0("E:/cmip6_data/CMIP6/CMIP/CMCC/CMCC-CM2-SR5/historical/r1i1p1f1/", which_freq, "/", which_var, "/gn/20200616"))

#list available files
files <- list.files()

#identify the first regridded file and plot
regridded <- subset(files, grepl("bil_1x1.nc$", files))
regrid <- regridded[1]
regrid <- rast(regrid)
plot(regrid[[1]])

#check against the first native file
native <- subset(files, !files %in% regridded)
nat <- native[1]
nat <- rast(nat)
plot(nat[[1]])


#################
### CMCC-ESM2 ###
#################

#set working directory to this model
setwd(paste0("E:/cmip6_data/CMIP6/CMIP/CMCC/CMCC-ESM2/historical/r1i1p1f1/", which_freq, "/", which_var, "/gn/20210114"))

#list available files
files <- list.files()

#identify the first regridded file and plot
regridded <- subset(files, grepl("bil_1x1.nc$", files))
regrid <- regridded[1]
regrid <- rast(regrid)
plot(regrid[[1]])

#check against the first native file
native <- subset(files, !files %in% regridded)
nat <- native[1]
nat <- rast(nat)
plot(nat[[1]])


#######################
### HadGEM3-GC31-MM ###
#######################

#set working directory to this model
setwd(paste0("E:/cmip6_data/CMIP6/CMIP/MOHC/HadGEM3-GC31-MM/historical/r1i1p1f3/", which_freq, "/", which_var, "/gn/20191207"))

#list available files
files <- list.files()

#identify the first regridded file and plot
regridded <- subset(files, grepl("bil_1x1.nc$", files))
regrid <- regridded[1]
regrid <- rast(regrid)
plot(regrid[[1]])

#check against the first native file
native <- subset(files, !files %in% regridded)
nat <- native[1]
nat <- rast(nat)
plot(nat[[1]])


#####################
### MPI-ESM1-2-HR ###
#####################

#set working directory to this model
setwd(paste0("E:/cmip6_data/CMIP6/CMIP/MPI-M/MPI-ESM1-2-HR/historical/r1i1p1f1/", which_freq, "/", which_var, "/gn/20190710"))

#list available files
files <- list.files()

#identify the first regridded file and plot
regridded <- subset(files, grepl("bil_1x1.nc$", files))
regrid <- regridded[1]
regrid <- rast(regrid)
plot(regrid[[1]])

#check against the first native file
native <- subset(files, !files %in% regridded)
nat <- native[1]
nat <- rast(nat)
plot(nat[[1]])


##################
### MRI-ESM2-0 ###
##################

#set working directory to this model
setwd(paste0("E:/cmip6_data/CMIP6/CMIP/MRI/MRI-ESM2-0/historical/r1i1p1f1/", which_freq, "/", which_var, "/gn/20190222"))

#list available files
files <- list.files()

#identify the first regridded file and plot
regridded <- subset(files, grepl("bil_1x1.nc$", files))
regrid <- regridded[1]
regrid <- rast(regrid)
plot(regrid[[1]])

#check against the first native file
native <- subset(files, !files %in% regridded)
nat <- native[1]
nat <- rast(nat)
plot(nat[[1]])
