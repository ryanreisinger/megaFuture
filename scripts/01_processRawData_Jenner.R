## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(stringr)
library(readxl)
# library(SOmap)

# -----------------------------------
## Curt Jenner
## email (via RC) 2019-11-07
# -----------------------------------

# -------------
## Metadata
# -------------

meta <- read.csv("./data_incoming/Jenner/Metadata file_SORP CircumPolar Humpbacks_Nov 2019_Jenner.csv",
                 stringsAsFactors = F,
                 skip = 6,
                 header = T)

## Remove example lines
meta <- filter(meta,
               dataset_identifier != "EXAMPLE_1" &
                 dataset_identifier != "EXAMPLE_2" &
                 dataset_identifier != "EXAMPLE_3")

## Dataset identifier should all be 2014, based on deployment year
## and replace spaces
meta$dataset_identifier <- "CWR WAVES 2014"
meta$dataset_identifier <- str_replace_all(meta$dataset_identifier, " ", "_")

# Replace entries that should be NA
meta$sex <- na_if(meta$sex, "no")
meta$how_sexed <- na_if(meta$how_sexed, "-")
meta$genotyped <- na_if(meta$genotyped, "no")
meta$age. <- na_if(meta$age., "no")
meta$progesterone <- na_if(meta$progesterone, "no")
meta$If.yes..status <- na_if(meta$If.yes..status, "")

# Tag name to match name in data
meta[meta$individual_id == "Mn_WAVES14_Lander1", "individual_id"] <- "Mn_WAVES14_Lander01"

## Drop comments
meta$comments <- meta$X
meta$X <- NULL

# -------------
## Tracks
# -------------

# -------------
## PTTs

# Function for formatting
myFun <- function(x) {
  foo <- read.csv(x,
                  stringsAsFactors = F)
  
  bar <- filter(meta, device_id == unique(foo$tag.local.identifier))
  
  d <- data.frame("dataset_identifier" = bar$dataset_identifier,
                  "individual_id" = bar$individual_id,
                  "device_id" = foo$tag.local.identifier,
                  "date" = foo$timestamp,
                  "decimal_latitude" = foo$location.lat,
                  "decimal_longitude" = foo$location.long,
                  "location_quality" = foo$argos.lc,
                  "device_type" = "PTT")
  
  d$date <- strptime(d$date, format = "%F %T")
  
  return(d)
}

# Get the filenames
fls <- list.files("./data_incoming/Jenner/", full.names = T)
fls <- fls[grepl("movebankexport", fls)]

# rbind all the formatted data
datPTT <- do.call(rbind, lapply(fls, myFun))
rm(fls)

# -------------
## GPS

## All the .xlsx files seem to have the same data
foo <- readxl::read_xlsx("./data_incoming/Jenner/Mn_WAVES14_Lander01_DAP_31Jan2014-FastGPSpositions.xlsx",
                         sheet = "Sheet1")

# Get the appropriate metadata
bar <- filter(meta, individual_id == unique(foo$Name))

# Sort out the date
foo$date.day <- format(foo$Day, format = "%F")
foo$date.time <- format(foo$Time, format = "%T")
foo$date <- paste(foo$date.day, foo$date.time, sep = " ")
foo$date <- strptime(foo$date, format = "%F %T")

# Create the formmated dataframe
datGPS <- data.frame("dataset_identifier" = bar$dataset_identifier,
                "individual_id" = bar$individual_id,
                "device_id" = bar$device_id,
                "date" = foo$date,
                "decimal_latitude" = foo$Latitude,
                "decimal_longitude" = foo$Longitude,
                "location_quality" = NA,
                "device_type" = "GPS")

rm(foo, bar)

# Combine PTT and GPS
dat <- rbind(datPTT, datGPS)

# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_CWR_WAVES_2014.RDS")
saveRDS(meta, "./data_formatted/meta/meta_CWR_WAVES_2014.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_CWR_WAVES_2014.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_CWR_WAVES_2014.csv", row.names = F)