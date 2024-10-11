## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## February 2020

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(data.table)

# -----------------------------------
## Daniel Palacios
## Email 13 January 2020 and 14 January 2020 (forwarded from R Constantine)
# -----------------------------------

## Tracks
dat <- fread("./data_incoming/Mate&Palacios/OSU_tracks_Argos_2007ANT.csv",
                 stringsAsFactors = F,
                 fill = TRUE)
# Sort dates
dat <- as.data.frame(dat)
dat$date <- strptime(dat$timevalue,
                         format = "%m/%d/%Y %H:%M:%S", tz = "GMT")

dat <- data.frame("dataset_identifier" = "OSU_2007ANT",
                      "individual_id" = dat$tag_id,
                      "device_id" = dat$ptt,
                      "date" = dat$date,
                      "decimal_latitude" = dat$latitude,
                      "decimal_longitude" = dat$longitude,
                      "location_quality" = dat$lc,
                      "device_type" = "PTT")

# Remove NAs
dat <- dat[!is.na(dat$individual_id), ]


## Meta
foo <- fread("./data_incoming/Mate&Palacios/Metadata file_SORP CircumPolar Humpbacks_Nov 2019_OSU.csv",
             header = T,
             skip = 6)

## Create metadata in the right format
meta <- foo[,1:22]
meta$comments <- foo$V23
rm(foo)

# -----------------------------------
# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Mate&Palacios.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Mate&Palacios.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Mate&Palacios.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Mate&Palacios.csv", row.names = F)
