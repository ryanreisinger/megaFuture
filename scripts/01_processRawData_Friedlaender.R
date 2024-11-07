## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## January 2020

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)

# -----------------------------------
## Ben Weinstein
## Dropbox, December 2019
# -----------------------------------

# -------------
## 2012
# -------------

## Tracks
dat2012 <- fread("./data_incoming/Friedlaender/2012/03. 2012_WC_DAP/Ari_data-Argos.csv",
                    stringsAsFactors = F,
                 fill = TRUE)

# Sort dates
dat2012 <- as.data.frame(dat2012)
dat2012$date <- strptime(dat2012$Date,
                         format = "%H:%M:%S %d-%b-%Y", tz = "GMT")

dat2012 <- data.frame("dataset_identifier" = "Friedlaender_2012",
                  "individual_id" = dat2012$Ptt,
                  "device_id" = dat2012$Ptt,
                  "date" = dat2012$date,
                  "decimal_latitude" = dat2012$Latitude,
                  "decimal_longitude" = dat2012$Longitude,
                  "location_quality" = dat2012$LocationQuality,
                  "device_type" = "PTT")

# Remove NAs
dat2012 <- dat2012[!is.na(dat2012$individual_id), ]

## Meta
foo <- readxl::read_xlsx("./data_incoming/Friedlaender/2012/2012_deployment_info.xlsx",
                  sheet = "Sheet1")

# Date
foo$year <- year(foo$Date)
foo$month <- month(foo$Date)
foo$day <- day(foo$Date)

meta2012 <- data.frame("dataset_identifier" = "Friedlaender_2012",
                   "data_owner" = "Ari Friedlaender",
                   "contact_email" = "ari.friedlaender@ucsc.edu",
                   "file_name" = "Ari_data-Argos.csv",
                   "individual_id" = foo$`Tag #`,
                   "device_id" = foo$`Tag #`,
                   "device_type" = "Argos",
                   "year" = foo$year,
                   "month" = foo$month,
                   "day" = foo$day,
                   "time" = NA,
                   "time_zone" = "UTC",
                   "deployment_site" = "Antarctic Peninsula",
                   "deployment_decimal_latitude" = foo$`Latitude (S)`,
                   "deployment_decimal_longitude" = foo$`Longitude (W)`,
                   "sex" = NA,
                   "how_sexed" = NA,
                   "age_class" = NA,
                   "genotyped" = NA,
                   "age." = foo$`Age Class`,
                   "progesterone" = NA,
                   "If.yes..status" = NA,
                   "comments" = NA)

rm(foo)

# Fix coordinates
meta2012$deployment_decimal_latitude <- meta2012$deployment_decimal_latitude * -1
meta2012$deployment_decimal_longitude <- meta2012$deployment_decimal_longitude * -1


# -------------
## 2013
# -------------

## Data
dat2013 <- fread("./data_incoming/Friedlaender/2013/03. 2013_WC_DAP/current-hbw-Argos.csv",
                 stringsAsFactors = FALSE,
                 fill = TRUE)

# Sort dates
dat2013 <- as.data.frame(dat2013)
dat2013$date <- strptime(dat2013$Date,
                         format = "%H:%M:%S %d-%b-%Y", tz = "GMT")

dat2013 <- data.frame("dataset_identifier" = "Friedlaender_2013",
                      "individual_id" = dat2013$Ptt,
                      "device_id" = dat2013$Ptt,
                      "date" = dat2013$date,
                      "decimal_latitude" = dat2013$Latitude,
                      "decimal_longitude" = dat2013$Longitude,
                      "location_quality" = dat2013$LocationQuality,
                      "device_type" = "PTT")

# Remove NAs
dat2013 <- dat2013[!is.na(dat2013$individual_id), ]

## Meta
foo <- readxl::read_xlsx("./data_incoming/Friedlaender/2013/2013_deployment_info.xlsx",
                         sheet = "Sheet1")

foo <- foo[!is.na(foo$`Tag ID`), ]

# Drop minkes
foo <- foo[foo$Species == "Mn", ]

# Date
foo$year <- year(foo$Date)
foo$month <- month(foo$Date)
foo$day <- day(foo$Date)
foo$time <- format(foo$Time, "%H:%M:%S")

meta2013 <- data.frame("dataset_identifier" = "Friedlaender_2013",
                       "data_owner" = "Ari Friedlaender",
                       "contact_email" = "ari.friedlaender@ucsc.edu",
                       "file_name" = "current-hbw-Argos.csv",
                       "individual_id" = foo$`Tag ID`,
                       "device_id" = foo$`Tag ID`,
                       "device_type" = "Argos",
                       "year" = foo$year,
                       "month" = foo$month,
                       "day" = foo$day,
                       "time" = NA,
                       "time_zone" = "UTC",
                       "deployment_site" = "Antarctic Peninsula",
                       "deployment_decimal_latitude" = foo$Latitude,
                       "deployment_decimal_longitude" = foo$Longitude,
                       "sex" = NA,
                       "how_sexed" = NA,
                       "age_class" = NA,
                       "genotyped" = NA,
                       "age." = NA,
                       "progesterone" = NA,
                       "If.yes..status" = NA,
                       "comments" = NA)

rm(foo)

# Fix coordinates
meta2013$deployment_decimal_latitude <- meta2013$deployment_decimal_latitude * -1
meta2013$deployment_decimal_longitude <- meta2013$deployment_decimal_longitude * -1

# Keep only IDs for which there is tracking data
meta2013 <- filter(meta2013, individual_id %in% unique(dat2013$individual_id))


# -------------
## 2014 - no tag deployments
# -------------


# -------------
## 2015
# -------------

## Data
dat2015 <- fread("./data_incoming/Friedlaender/2015/2015_WC_DAP/AP_2015-Locations.csv",
                 stringsAsFactors = FALSE,
                 fill = TRUE)

# Sort dates
dat2015 <- as.data.frame(dat2015)
dat2015$date <- strptime(dat2015$Date,
                         format = "%H:%M:%S %d-%b-%Y", tz = "GMT")

dat2015 <- data.frame("dataset_identifier" = "Friedlaender_2015",
                      "individual_id" = dat2015$Ptt,
                      "device_id" = dat2015$Ptt,
                      "date" = dat2015$date,
                      "decimal_latitude" = dat2015$Latitude,
                      "decimal_longitude" = dat2015$Longitude,
                      "location_quality" = dat2015$Quality,
                      "device_type" = "PTT")

# Remove NAs
dat2015 <- dat2015[!is.na(dat2015$individual_id), ]

## Meta
foo <- readxl::read_xlsx("./data_incoming/Friedlaender/2015/2015_deployment_info.xlsx",
                         sheet = "Sheet1")

# Keep only humpbacks
foo <- foo[foo$Species == "Humpback", ]

# Date
foo$year <- year(foo$`Date (UTC)`)
foo$month <- month(foo$`Date (UTC)`)
foo$day <- day(foo$`Date (UTC)`)
foo$time <- format(foo$`Time (UTC)`, "%H:%M:%S")

meta2015 <- data.frame("dataset_identifier" = "Friedlaender_2015",
                       "data_owner" = "Ari Friedlaender",
                       "contact_email" = "ari.friedlaender@ucsc.edu",
                       "file_name" = "AP_2015-Locations.csv",
                       "individual_id" = foo$`Argos Number`,
                       "device_id" = foo$`Argos Number`,
                       "device_type" = "Argos",
                       "year" = foo$year,
                       "month" = foo$month,
                       "day" = foo$day,
                       "time" = NA,
                       "time_zone" = "UTC",
                       "deployment_site" = "Antarctic Peninsula",
                       "deployment_decimal_latitude" = foo$`Deployment Lat`,
                       "deployment_decimal_longitude" = foo$`Deployment Long`,
                       "sex" = NA,
                       "how_sexed" = NA,
                       "age_class" = NA,
                       "genotyped" = NA,
                       "age." = NA,
                       "progesterone" = NA,
                       "If.yes..status" = NA,
                       "comments" = NA)

rm(foo)

# Keep only IDs for which there is tracking data
meta2015 <- filter(meta2015, individual_id %in% unique(dat2015$individual_id))


# -------------
## 2016 - Tracking data. Metadata are taking from Weinstein et al. 2017
# -------------

# Get list of all location files and bind them
foo <- list.files("./data_incoming/Friedlaender/2016/",
                  full.names = T,
                  recursive = T)

foo <- foo[grepl("Locations.csv", foo)]

dat2016 <- bind_rows(lapply(foo, read.csv, stringsAsFactors = F))

dat2016 <- as.data.frame(dat2016)
dat2016$date <- strptime(dat2016$Date,
                         format = "%H:%M:%S %d-%b-%Y", tz = "GMT")

dat2016 <- data.frame("dataset_identifier" = "Friedlaender_2016",
                      "individual_id" = dat2016$Ptt,
                      "device_id" = dat2016$Ptt,
                      "date" = dat2016$date,
                      "decimal_latitude" = dat2016$Latitude,
                      "decimal_longitude" = dat2016$Longitude,
                      "location_quality" = dat2016$Quality,
                      "device_type" = "PTT")

## Meta
foo <- read.csv("./data_incoming/Friedlaender/weinstein_supplement.csv",
                         stringsAsFactors = F)

# Date
foo$date <- strptime(foo$Tagging.Date, format = "%m/%d/%Y", tz = "UTC")
foo$year <- year(foo$date)
foo$month <- month(foo$date)
foo$day <- day(foo$date)
foo$time <- NA

# Keep only 2016 data
foo <- foo[foo$year == 2016, ]

meta2016 <- data.frame("dataset_identifier" = "Friedlaender_2016",
                       "data_owner" = "Ari Friedlaender",
                       "contact_email" = "ari.friedlaender@ucsc.edu",
                       "file_name" = NA,
                       "individual_id" = foo$ID,
                       "device_id" = foo$ID,
                       "device_type" = "Argos",
                       "year" = foo$year,
                       "month" = foo$month,
                       "day" = foo$day,
                       "time" = NA,
                       "time_zone" = "UTC",
                       "deployment_site" = "Antarctic Peninsula",
                       "deployment_decimal_latitude" = foo$Lat.,
                       "deployment_decimal_longitude" = foo$Long.,
                       "sex" = NA,
                       "how_sexed" = NA,
                       "age_class" = NA,
                       "genotyped" = NA,
                       "age." = NA,
                       "progesterone" = NA,
                       "If.yes..status" = NA,
                       "comments" = NA)

rm(foo)


# -------------
## 2017 - Tracking data only. TODO: find metadata
# -------------

# Get list of all location files and bind them
foo <- list.files("./data_incoming/Friedlaender/2017/",
                  full.names = T,
                  recursive = T)

foo <- foo[grepl("Locations.csv", foo)]

dat2017 <- bind_rows(lapply(foo, read.csv, stringsAsFactors = F))

dat2017 <- as.data.frame(dat2017)
dat2017$date <- strptime(dat2017$Date,
                         format = "%H:%M:%S %d-%b-%Y", tz = "GMT")

dat2017 <- data.frame("dataset_identifier" = "Friedlaender_2017",
                      "individual_id" = dat2017$Ptt,
                      "device_id" = dat2017$Ptt,
                      "date" = dat2017$date,
                      "decimal_latitude" = dat2017$Latitude,
                      "decimal_longitude" = dat2017$Longitude,
                      "location_quality" = dat2017$Quality,
                      "device_type" = "PTT")

# TODO: Get metadata.


# -------------
## 2018 - Tracking data only. TODO: find metadata
# -------------

# Get list of all location files and bind them
foo <- list.files("./data_incoming/Friedlaender/2018/",
                  full.names = T,
                  recursive = T)

foo <- foo[grepl("Locations.csv", foo)]

dat2018 <- bind_rows(lapply(foo, read.csv, stringsAsFactors = F))

dat2018 <- as.data.frame(dat2018)
dat2018$date <- strptime(dat2018$Date,
                         format = "%H:%M:%S %d-%b-%Y", tz = "GMT")

dat2018 <- data.frame("dataset_identifier" = "Friedlaender_2018",
                      "individual_id" = dat2018$Ptt,
                      "device_id" = dat2018$Ptt,
                      "date" = dat2018$date,
                      "decimal_latitude" = dat2018$Latitude,
                      "decimal_longitude" = dat2018$Longitude,
                      "location_quality" = dat2018$Quality,
                      "device_type" = "PTT")

# TODO: Get metadata.

# -----------------------------------
# Join years
dat <- rbind(dat2012, dat2013, dat2015, dat2016, dat2017, dat2018)
meta <- rbind(meta2012, meta2013, meta2015, meta2016)
# meta <- rbind(meta2012, meta2013, meta2015, meta2016, meta2017, meta2018)

# -----------------------------------
# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Friedlaender.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Friedlaender.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Friedlaender.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Friedlaender.csv", row.names = F)
