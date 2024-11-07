## Combine processed data and produce initial summaries

## Ryan Reisinger
## November 2019

setwd("D:\\UCSC\\Analysis\\mega")

library(dplyr)

meta <- read.csv("./out/fastOutMeta.csv", stringsAsFactors = F)

# Group Friedaender datasets
meta[which(grepl(pattern = "Friedlaender", x = meta$dataset_identifier)), "dataset_identifier"] <- "Friedlaender"

# Group
meta <- group_by(meta, dataset_identifier) %>%
  summarise(., n())

print(meta)
