## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
# library(stringr)
# library(readxl)
# library(SOmap)

# -----------------------------------
## AMMC (Virginia Andrews-Goff and Mike Double)
## email 2019-11-08
# -----------------------------------

# -------------
## Metadata
# -------------

meta <- read.csv("./data_incoming/AMMC/Metadata file.csv",
                 stringsAsFactors = F,
                 header = T)

# Deployment lons and lats are swapped
lon <- meta$deployment_decimal_latitude
lat <- meta$deployment_decimal_longitude
meta$deployment_decimal_latitude <- lat
meta$deployment_decimal_longitude <- lon
rm(lat, lon)

# Replace entries that should be NA
meta$sex <- na_if(meta$sex, "no")
meta$how_sexed <- na_if(meta$how_sexed, "no")
meta$genotyped <- na_if(meta$genotyped, "no")
meta$age_class <- na_if(meta$age_class, "no")
meta$age. <- na_if(meta$age., "no")
meta$progesterone <- na_if(meta$progesterone, "no")
meta$If.yes..status <- na_if(meta$If.yes..status, "-")

# Standardise age class entries
meta$age_class <- recode(meta$age_class, "Sub-adult" = "Subadult")

# Add comments column
meta$comments <- NA

# -------------
## Tracks
# -------------

# -------------
## PTTs 01

# Function for formatting
myFun <- function(x) {
  foo <- read.csv(x,
                  stringsAsFactors = F)
  
  
  d <- data.frame("dataset_identifier" = "AMMC",
                  "individual_id" = foo$id,
                  "device_id" = foo$id,
                  "date" = foo$date,
                  "decimal_latitude" = foo$lat,
                  "decimal_longitude" = foo$lon,
                  "location_quality" = foo$lc,
                  "device_type" = "PTT")
  
  d$date <- strptime(d$date, format = "%F %T")
  
  return(d)
}

# Get the filenames
fls <- list.files("./data_incoming/AMMC/", full.names = T)
fls <- fls[grepl("hbw_", fls)]

# rbind all the formatted data
dat_01 <- do.call(rbind, lapply(fls, myFun))
rm(fls)

# -------------
## PTTs 02

foo <- read.csv("./data_incoming/AMMC/PTT_96382.csv",
                stringsAsFactors = F)

# Sort out the date
foo$date <- strptime(foo$gmt, format = "%d/%m/%Y %H:%M")

dat_02 <- data.frame("dataset_identifier" = "AMMC",
                            "individual_id" = foo$ptt,
                            "device_id" = foo$ptt,
                            "date" = foo$date,
                            "decimal_latitude" = foo$latitude,
                            "decimal_longitude" = foo$longitude,
                            "location_quality" = foo$class,
                            "device_type" = "PTT")

rm(foo)

# -------------
## PTTs 03

foo <- read.csv("./data_incoming/AMMC/WA_Fisheries.csv",
                stringsAsFactors = F)

# Sort out the date
foo$date <- strptime(foo$DT, format = "%F %T")

dat_03 <- data.frame("dataset_identifier" = "AMMC",
                     "individual_id" = foo$Tag,
                     "device_id" = foo$Tag,
                     "date" = foo$date,
                     "decimal_latitude" = foo$Latitude,
                     "decimal_longitude" = foo$Longitude,
                     "location_quality" = foo$Quatlity,
                     "device_type" = "PTT")

rm(foo)

# -------------
## Combine data
dat <- rbind(dat_01, dat_02, dat_03)

# -----------------------------------
# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_AMMC.RDS")
saveRDS(meta, "./data_formatted/meta/meta_AMMC.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_AMMC.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_AMMC.csv", row.names = F)
