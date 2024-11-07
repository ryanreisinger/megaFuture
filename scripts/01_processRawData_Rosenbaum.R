## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

# -----------------------------------
## Howard Rosenbaum
## email 2020-04-14
# -----------------------------------

# -------------
## Metadata
# -------------

meta <- read.csv("./data_incoming/Rosenbaum/Metadata file_SORP CircumPolar Humpbacks_Nov 2019_EC.csv",
                 stringsAsFactors = F,
                 skip = 1,
                 header = T)

## Add comments
meta$comments <- meta$X
meta$X <- NULL

## Add dataset identifier
meta$dataset_identifier <- "Rosenbaum"

## Copy individual id to device id
meta$device_id <- meta$individual_id

## Females read in as "FALSE"
meta$sex <- "F"

# -------------
## Tracks
# -------------

foo <- read.csv("./data_incoming/Rosenbaum/HumpbacksGabon_2Tracks.csv",
                stringsAsFactors = F)


dat <- data.frame("dataset_identifier" = unique(meta$dataset_identifier),
                  "individual_id" = foo$PTT,
                  "device_id" = foo$PTT,
                  "date" = strptime(foo$hdatetime, format = "%m-%d-%Y %H:%M:%S"),
                  "decimal_latitude" = foo$lat1,
                  "decimal_longitude" = foo$lon1,
                  "location_quality" = foo$loc_class,
                  "device_type" = "PTT")


# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Rosenbaum.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Rosenbaum.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Rosenbaum.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Rosenbaum.csv", row.names = F)