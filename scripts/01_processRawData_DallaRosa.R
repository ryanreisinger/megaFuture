## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(stringr)

# -----------------------------------
## Luciano Dalla Rosa
## use data compiled for RAATD
# -----------------------------------

# -------------
## Metadata
# -------------

foo <- read.csv("./data_incoming/RAATD/SCAR_Metadata_2017_forWEBDAV.csv",
                 stringsAsFactors = F,
                 header = T)

## Get only Dalla Rosa data
foo <- filter(foo, data_owner == "Luciano Dalla Rosa")

## Create metadata in the right format
meta <- data.frame("dataset_identifier" = "DallaRosa_AP",
                   "data_owner" = foo$data_owner,
                   "contact_email" = foo$contact,
                   "file_name" = "RAATD2017_HUWH.csv",
                   "individual_id" = foo$individual_id,
                   "device_id" = foo$device_id,
                   "device_type" = foo$device_type,
                   "year" = foo$year,
                   "month" = foo$month,
                   "day" = foo$day,
                   "time" = foo$deployment_time,
                   "time_zone" = "UTC",
                   "deployment_site" = foo$deployment_site,
                   "deployment_decimal_latitude" = foo$deployment_decimal_latitude,
                   "deployment_decimal_longitude" = foo$deployment_decimal_longitude,
                   "sex" = foo$sex,
                   "how_sexed" = NA,
                   "age_class" = NA,
                   "genotyped" = NA,
                   "age." = NA,
                   "progesterone" = NA,
                   "If.yes..status" = NA,
                   "comments" = NA,
                   stringsAsFactors = FALSE)

rm(foo)

# -------------
## Tracks
# -------------
  
foo <- read.csv("./data_incoming/RAATD/RAATD2017_HUWH.csv",
                stringsAsFactors = F)

# Get only Dalla Rosa data
foo <- filter(foo, individual_id %in% meta$individual_id)

# Sort out the date
foo$date.full <- paste0(foo$year,
                        "-",
                        foo$month,
                        "-",
                        foo$day,
                        " ",
                        foo$time)

foo$date.time <- strptime(foo$date.full, format = "%Y-%m-%d %H:%M:%S")

dat <- data.frame("dataset_identifier" = "DallaRosa_AP",
                     "individual_id" = foo$individual_id,
                     "device_id" = foo$individual_id,
                     "date" = foo$date.time,
                     "decimal_latitude" = foo$decimal_latitude,
                     "decimal_longitude" = foo$decimal_longitude,
                     "location_quality" = foo$location_quality,
                     "device_type" = "PTT")

rm(foo)

# -----------------------------------
# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_DallaRosa_AP.RDS")
saveRDS(meta, "./data_formatted/meta/meta_DallaRosa_AP.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_DallaRosa_AP.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_DallaRosa_AP.csv", row.names = F)
