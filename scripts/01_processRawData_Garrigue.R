## Process Southern Ocean humpback whale tracking data

## Ryan Reisinger
## November 2019

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(stringr)

# -----------------------------------
## Claire Garrigue
## email 2019-11-14
# -----------------------------------

# -------------
## Metadata
# -------------

meta <- read.csv("./data_incoming/Garrigue/2019_Metadata file_SORP-HW-NC-MMS.csv",
                 stringsAsFactors = F,
                 skip = 6,
                 header = T)

## Remove example lines and empty line
meta <- filter(meta,
               dataset_identifier != "" &
               dataset_identifier != "EXAMPLE_1" &
                 dataset_identifier != "EXAMPLE_2" &
                 dataset_identifier != "EXAMPLE_3")

# Replace entries that should be NA
meta$age. <- na_if(meta$age., "no")
meta$progesterone <- na_if(meta$progesterone, "no")
meta$If.yes..status <- na_if(meta$If.yes..status, "")

## Add comments
meta$comments <- meta$X
meta$X <- NULL

# -------------
## Tracks
# -------------

foo <- read.csv("./data_incoming/Garrigue/2019_SORP-HW-NC-MMS.csv",
                stringsAsFactors = F)

# Update IDs to match metadata
foo$id_ptt <- as.integer(str_split(foo$id, "-", n = 2, simplify = TRUE)[,2])
foo2 <- left_join(x = foo,
          y = select(meta, individual_id, device_id),
          by = c("id_ptt" = "device_id"))

dat <- data.frame("dataset_identifier" = unique(meta$dataset_identifier),
                  "individual_id" = foo2$individual_id,
                  "device_id" = foo2$id_ptt,
                  "date" = foo2$time,
                  "decimal_latitude" = foo2$lat,
                  "decimal_longitude" = foo2$lon,
                  "location_quality" = foo2$lq,
                  "device_type" = "PTT")


# Write files
saveRDS(dat, "./data_formatted/tracks/tracks_Garrigue.RDS")
saveRDS(meta, "./data_formatted/meta/meta_Garrigue.RDS")

write.csv(dat, "./data_formatted/tracks/tracks_Garrigue.csv", row.names = F)
write.csv(meta, "./data_formatted/meta/meta_Garrigue.csv", row.names = F)