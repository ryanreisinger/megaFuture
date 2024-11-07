## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(stringr)

# -----------------------------------
## Ari Friedlaender
## use data compiled for RAATD
# -----------------------------------

## Most of these PTTs are included in the Friedlaender dataset
## only these non-duplicates are processed here

these.ids <- c("113206", "113207", "113208", "113210", "113211")

# -------------
## Metadata
# -------------

foo <- read.csv("./data_incoming/RAATD/SCAR_Metadata_2017_forWEBDAV.csv",
                 stringsAsFactors = F,
                 header = T)

## Get only Friedlaender data
foo <- filter(foo, data_owner == "Ari Friedlaender")

## Create metadata in the right format
meta <- data.frame("dataset_identifier" = "Friedlaender_RAATD",
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

## Keep only data that is not in the Friedlaender dataset
meta <- dplyr::filter(meta, meta$device_id %in% these.ids)

# -------------
## Tracks
# -------------
  
foo <- read.csv("./data_incoming/RAATD/RAATD2017_HUWH.csv",
                stringsAsFactors = F)

# Get only Friedlaender data
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

dat <- data.frame("dataset_identifier" = "Friedlaender_RAATD",
                     "individual_id" = foo$individual_id,
                     "device_id" = foo$individual_id,
                     "date" = foo$date.time,
                     "decimal_latitude" = foo$decimal_latitude,
                     "decimal_longitude" = foo$decimal_longitude,
                     "location_quality" = foo$location_quality,
                     "device_type" = "PTT")

rm(foo)

## Keep only data that is not in the Friedlaender dataset
dat <- dplyr::filter(dat, dat$device_id %in% these.ids)

# -----------------------------------
# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Friedlaender_RAATD.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Friedlaender_RAATD.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Friedlaender_RAATD.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Friedlaender_RAATD.csv", row.names = F)
