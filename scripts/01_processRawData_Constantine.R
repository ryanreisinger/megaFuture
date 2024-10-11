## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(stringr)

# -----------------------------------
## Rochelle Constantine
## email 2019-11-15
## but, use data compiled for RAATD
# -----------------------------------

# -------------
## Metadata
# -------------

foo <- read.csv("./data_incoming/RAATD/SCAR_Metadata_2017_forWEBDAV.csv",
                 stringsAsFactors = F,
                 header = T)

## Get only Raoul data
foo <- filter(foo, dataset_identifier == "HUWH_Constantine_Raoul_2015")

## Split the individual_id to get the ptt
foo$id <- str_split_fixed(foo$individual_id, "_", n = 2)[,2]

## Create metadata in the right format
meta <- data.frame("dataset_identifier" = "Constantine_Raoul_2015",
                   "data_owner" = foo$data_owner,
                   "contact_email" = foo$contact,
                   "file_name" = "Constantine_Raoul_2015_Raw_humpback_speed_for RAATD.xlsx.csv",
                   "individual_id" = foo$id,
                   "device_id" = foo$id,
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
                   "comments" = foo$comments)

rm(foo)

# -------------
## Tracks
# -------------
  
foo <- read.csv("./data_incoming/Constantine/Constantine_Raoul_2015_Raw_humpback_speed_for RAATD.xlsx.csv",
                stringsAsFactors = F)

# Sort out the date
foo$date.day <- strptime(foo$date, format = "%d/%m/%Y")
foo$date.day <- format(foo$date.day, format = "%F")

foo$date.time <- strptime(foo$time, format = "%H:%M:%S")
foo$date.time <- format(foo$time, format = "%H:%M:%S")

foo$date <- paste(foo$date.day, foo$date.time, sep = " ")

dat <- data.frame("dataset_identifier" = "Constantine_Raoul_2015",
                     "individual_id" = foo$ptt,
                     "device_id" = foo$ptt,
                     "date" = foo$date,
                     "decimal_latitude" = foo$latitude,
                     "decimal_longitude" = foo$longitude,
                     "location_quality" = foo$class,
                     "device_type" = "PTT")

rm(foo)

# -----------------------------------
# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Constantine_Raoul_2015.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Constantine_Raoul_2015.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Constantine_Raoul_2015.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Constantine_Raoul_2015.csv", row.names = F)
