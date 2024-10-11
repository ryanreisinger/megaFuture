## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## February 2020

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(lubridate)

## Tracks

# Get list of all location files and bind them
foo <- list.files("./data_incoming/Seakamela",
                  full.names = T,
                  recursive = T)

foo <- foo[grepl("Locations.csv", foo)]

dat <- bind_rows(lapply(foo, read.csv, stringsAsFactors = F))


# Parse dates
dat <- as.data.frame(dat)
# dat$date <- parse_date_time(dat$Date,
#                             orders = c("HMSdmy"))

dat$date <- strptime(dat$Date, format = "%H:%M:%S %d-%b-%Y")

dat[which(is.na(dat$date)), ]$date <- strptime(dat[which(is.na(dat$date)), ]$Date,
                                               format = "%Y-%m-%d %H:%M")

# Format
dat <- data.frame("dataset_identifier" = "Oceans&Coasts_Seakamela",
                      "individual_id" = dat$Ptt,
                      "device_id" = dat$Ptt,
                      "date" = dat$date,
                      "decimal_latitude" = dat$Latitude,
                      "decimal_longitude" = dat$Longitude,
                      "location_quality" = dat$Quality,
                      "device_type" = "PTT")

## Meta
foo <- read.csv("./data_incoming/Seakamela/Soutf African HBW Tagging Metadata_May 2019_RR.csv",
             stringsAsFactors = F)


# Date
foo$date <- strptime(foo$Date,
                     format = "%d/%m/%Y", tz = "GMT")


foo$year <- year(foo$date)
foo$month <- month(foo$date)
foo$day <- day(foo$date)
foo$time <- NA

meta <- data.frame("dataset_identifier" = "Oceans&Coasts_Seakamela",
                       "data_owner" = "Mdu Seakamela",
                       "contact_email" = "smseakamela@environment.gov.za",
                       "file_name" = NA,
                       "individual_id" = foo$PTT.ID,
                       "device_id" = foo$PTT.ID,
                       "device_type" = "PTT",
                       "year" = foo$year,
                       "month" = foo$month,
                       "day" = foo$day,
                       "time" = NA,
                       "time_zone" = "UTC",
                       "deployment_site" = foo$Area,
                       "deployment_decimal_latitude" = foo$Latitude,
                       "deployment_decimal_longitude" = foo$Longitude,
                       "sex" = NA,
                       "how_sexed" = NA,
                       "age_class" = foo$Type.of.individual,
                       "genotyped" = NA,
                       "age." = NA,
                       "progesterone" = NA,
                       "If.yes..status" = NA,
                       "comments" = NA)

rm(foo)

# Keep only IDs for which there is tracking data
meta <- filter(meta, individual_id %in% unique(dat$individual_id))

# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Seakamela.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Seakamela.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Seakamela.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Seakamela.csv", row.names = F)