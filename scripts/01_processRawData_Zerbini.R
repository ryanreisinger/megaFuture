## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("D:\\UCSC\\Analysis\\mega")

library(dplyr)
library(stringr)

# -----------------------------------
## Alex Zerbini
## emails DATE (data) and DATE (metadata)
# -----------------------------------

# -------------
## Metadata
# -------------

# foo <- read.csv("./data_incoming/RAATD/SCAR_Metadata_2017_forWEBDAV.csv",
#                  stringsAsFactors = F,
#                  header = T)
# 
# ## Create metadata in the right format
# meta <- data.frame("dataset_identifier" = "Constantine_Raoul_2015",
#                    "data_owner" = foo$data_owner,
#                    "contact_email" = foo$contact,
#                    "file_name" = "Constantine_Raoul_2015_Raw_humpback_speed_for RAATD.xlsx.csv",
#                    "individual_id" = foo$id,
#                    "device_id" = foo$id,
#                    "device_type" = foo$device_type,
#                    "year" = foo$year,
#                    "month" = foo$month,
#                    "day" = foo$day,
#                    "time" = foo$deployment_time,
#                    "time_zone" = "UTC",
#                    "deployment_site" = foo$deployment_site,
#                    "deployment_decimal_latitude" = foo$deployment_decimal_latitude,
#                    "deployment_decimal_longitude" = foo$deployment_decimal_longitude,
#                    "sex" = foo$sex,
#                    "how_sexed" = NA,
#                    "age_class" = NA,
#                    "genotyped" = NA,
#                    "age." = NA,
#                    "progesterone" = NA,
#                    "If.yes..status" = NA,
#                    "comments" = foo$comments)
# 
# rm(foo)

# -------------
## Tracks
# -------------
  
foo <- read.csv("./data_incoming/Zerbini/Brazil_03_18_Ryan_20191121.csv",
                stringsAsFactors = F)

# Sort out the date
foo$date.day <- strptime(foo$Date, format = "%m/%d/%y")
foo$date.day <- format(foo$date.day, format = "%F")

foo$date.time <- strptime(foo$Time, format = "%H:%M:%S")
foo$date.time <- format(foo$Time, format = "%H:%M:%S")

foo$date <- paste(foo$date.day, foo$date.time, sep = " ")

dat <- data.frame("dataset_identifier" = "Zerbini",
                     "individual_id" = foo$ID,
                     "device_id" = foo$ID,
                     "date" = foo$date,
                     "decimal_latitude" = foo$Latitude,
                     "decimal_longitude" = foo$Longitude,
                     "location_quality" = foo$Quality,
                     "device_type" = "PTT")

rm(foo)

# -------------
## Metadata
# -------------

foo <- dat[dat$location_quality == "Tagging", ]
foo$date <- strptime(foo$date, format = "%Y-%m-%d %H:%M:%S")

## Create metadata in the right format
meta <- data.frame("dataset_identifier" = "Zerbini",
                   "data_owner" = "Alex Zerbini",
                   "contact_email" = "alex.zerbini@noaa.gov",
                   "file_name" = "Brazil_03_18_Ryan_20191121.csv",
                   "individual_id" = foo$individual_id,
                   "device_id" = foo$device_id,
                   "device_type" = foo$device_type,
                   "year" = year(foo$date),
                   "month" = month(foo$date),
                   "day" = day(foo$date),
                   "time" = format(foo$date, "%H:%M:%S"),
                   "time_zone" = "UTC",
                   "deployment_site" = NA,
                   "deployment_decimal_latitude" = foo$decimal_latitude,
                   "deployment_decimal_longitude" = foo$decimal_longitude,
                   "sex" = NA,
                   "how_sexed" = NA,
                   "age_class" = NA,
                   "genotyped" = NA,
                   "age." = NA,
                   "progesterone" = NA,
                   "If.yes..status" = NA,
                   "comments" = NA)

rm(foo)

# -----------------------------------
# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Zerbini.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Zerbini.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Zerbini.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Zerbini.csv", row.names = F)
