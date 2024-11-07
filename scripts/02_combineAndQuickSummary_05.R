## Combine processed data and produce initial summaries

## Ryan Reisinger
## November 2019

setwd("D:\\UCSC\\Analysis\\mega")

library(foieGras)
library(dplyr)
library(sf)
library(ggplot2)
library(pals)
library(SOmap)


# -------------
## Tracks
# -------------

trk.fls <- list.files("./data_formatted/tracks/", full.names = T)
trk.fls <- trk.fls[grepl(".csv", trk.fls)]

tracks <- do.call(rbind, lapply(trk.fls, read.csv, stringsAsFactors = F))

# -------------
## Metadata
# -------------

met.fls <- list.files("./data_formatted/meta/", full.names = T)
met.fls <- met.fls[grepl(".csv", met.fls)]

meta <- do.call(rbind, lapply(met.fls, read.csv, stringsAsFactors = F))

# Add entries for tracks that have no metadata

meta$meta_source <- "meta" # add a flag indicating if metadata were generated from tracks

`%nin%` = Negate(`%in%`) # Negation

foo.tracks <- unique(tracks$individual_id) # Track ids
foo.meta <- unique(meta$individual_id) # Meta ids
foo.new <- foo.tracks[which(foo.tracks %nin% foo.meta)] # Which track ids are not in meta
foo.new <- foo.new[! is.na(foo.new)] # Drop NAs

# Make extra metadata id by id
extra.meta <- data.frame()

for (i in 1:length(foo.new)) {
  this.id <- foo.new[i]
  this.dat <- filter(tracks, tracks$individual_id == this.id)
  this.dat <- this.dat[1, ]
  this.meta <- data.frame("dataset_identifier" = this.dat$dataset_identifier,
                          "data_owner" = NA,
                          "contact_email" = NA,
                          "file_name" = NA,
                          "individual_id" = this.dat$individual_id,
                          "device_id" = this.dat$device_id,
                          "device_type" = this.dat$device_type,
                          "year" = NA,
                          "month" = NA,
                          "day" = NA,
                          "time" = NA,
                          "time_zone" = NA,
                          "deployment_site" = NA,
                          "deployment_decimal_latitude" = NA,
                          "deployment_decimal_longitude" = NA,
                          "sex" = NA,
                          "how_sexed" = NA,
                          "age_class" = NA,
                          "genotyped" = NA,
                          "age." = NA,
                          "progesterone" = NA,
                          "If.yes..status" = NA,
                          "comments" = NA,
                          "meta_source" = "track")
  extra.meta <- bind_rows(extra.meta, this.meta)
}

meta <- bind_rows(meta, extra.meta)

# Drop metadata for which there is no tracking data
meta <- filter(meta, meta$individual_id %in% foo.tracks)

# Write a quick combined copy
write.csv(meta, "./out/fastOutMeta.csv", row.names = F)

# -------------
## Quick stats
# -------------
length(unique(tracks$individual_id))

nrow(meta)
nrow(tracks)
min(tracks$date, na.rm = T)
max(tracks$date, na.rm = T)

# -------------
## Wrap longitudes
tracks$decimal_longitude <- wrap_lon(tracks$decimal_longitude)

# -------------
# Replace old location classes
tracks <- tracks %>%
  mutate(location_quality = ifelse(location_quality == "-9", "Z", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "-3", "Z", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "-2", "B", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "-1", "A", location_quality)) %>%
  mutate(location_quality = ifelse(location_quality == "Z", "B", location_quality))

# -------------
# Filter individuals not to use
tracks <- dplyr::filter(tracks, individual_id != 112694)
tracks <- dplyr::filter(tracks, individual_id != 20683)
tracks <- dplyr::filter(tracks, individual_id != "Mn_WAVES14_Lander01")
tracks <- dplyr::filter(tracks, individual_id != "Entangled whale")


# -------------
# Dates
# tracks$date <- strptime(tracks$date, format = "%F %T")

# -------------
## Organise for foieGras

## Assign location class to GPS tracks
tracks[tracks$device_type == "GPS", "location_quality"] <- "G"

## Create a dataframe for foieGras
dat <- dplyr::select(tracks,
              individual_id,
              date,
              location_quality,
              decimal_longitude,
              decimal_latitude,)

dat <- dplyr::rename(dat,
              id = individual_id,
              date = date,
              lc = location_quality,
              lon = decimal_longitude,
              lat = decimal_latitude)

## Drop the handful of records with no date information
dat <- dplyr::filter(dat, !is.na(dat$date))

## Drop records in the far northern hemisphere
## These are all test locations or errors
dat <- dplyr::filter(dat, dat$lat < 20)

## In Brazil data, drop 'Tagging' records
dat <- dplyr::filter(dat, dat$lc != "Tagging")

# Look at distribution of Argos fix intervals
ids <- unique(dat$id)
intrvls <- data.frame()

for (i in ids) {
  print(i)
  this.d <- dat[dat$id == i, ]
  this.d <- this.d[complete.cases(this.d), ]
  if (nrow(this.d) > 0) {
    this.d$date <- strptime(this.d$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    this.d <- this.d[order(this.d$date),]
    this.dif <- diff(this.d$date)
    units(this.dif) <- "hours"
    this.d$dif = c(0, as.numeric(this.dif))
    intrvls <- rbind(intrvls, this.d)
  }
}

summary(intrvls$dif)
hist(intrvls$dif, breaks = seq(0, 91*24, 24))

## Prefilter
# dat <- fit_ssm(d = dat, pf = TRUE) # This is failing with a strange error

## Prefilter individually to avoid errors with projection
ids <- unique(dat$id)
all.d <- data.frame()

for (i in ids) {
  print(i)
  this.d <- dat[dat$id == i, ]
  this.d <- this.d[complete.cases(this.d), ]
  
  if (nrow(this.d) >2 ) {
  this.d <- fit_ssm(this.d, vmax = 12, pf = TRUE)
  this.d <- st_transform(this.d, crs = "+proj=longlat +datum=WGS84")
  pts <- st_coordinates(this.d)
  that.d <- as.data.frame(this.d)[ , c("id", "date", "lc", "keep")]
  that.d$lon <- pts[,1]
  that.d$lat <- pts[,2]
  that.d <- that.d[that.d$keep == "TRUE", ]
  that.d$keep <- NULL
  # plot(that.d$date)
  all.d <- rbind(all.d, that.d)
  
  # # Time gaps
  # that.d$dif = c(0, as.numeric(diff(that.d$date), units="days"))
  # 
  # # Plot
  # p1 <- ggplot(data = that.d, aes(x = date, y = dif)) +
  #   geom_point(show.legend = T) +
  #   theme_bw() +
  #   geom_vline(xintercept = 0) +
  #   guides(color = guide_legend(title = "location_to_keep")) +
  #   geom_line(color = "gray40") + 
  #   labs(x = "Days since deployment", y = "Distance from deployment")
  # print(p1)
  
  }
}

dat <- all.d

#--------------------------------------------------------------------------
## Quick plot of filtered tracks
library(pals)
write.csv(dat, "./out/fastOutTracks.csv", row.names = F) # Write a copy

#---------------------------------
## Get ice data from Raymond et al. 2015
## Proportion of the year for which sea ice >85% is present
## Jan 2003 - Dec 2010
meanice <- raster("./data_env/ice.grd")

## Trim 15%
meanice[meanice < 0.01] <- NA

## Project
crs(meanice) <- "+proj=longlat +datum=WGS84 +no_defs"
meanice <- projectRaster(meanice,
                         res = 25000,
                         crs ="+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

#---------------------------------
## Get sea-ice extent, 2015
## from NSIDC sea-ice index
## https://nsidc.org/data/seaice_index/archives
## ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/shapefiles/shp_extent/
library(rgdal)

## Minimum (February)
minice <- readOGR(dsn = "./data_env/seaice/extent_S_201502_polyline_v3.0", layer = "extent_S_201502_polyline_v3.0")

## Maximum (October)
maxice <- readOGR(dsn = "./data_env/seaice/extent_S_201510_polyline_v3.0", layer = "extent_S_201510_polyline_v3.0")

#---------------------------------
## Trim tracking data
dat.copy <- dat[dat$lat < -10, ]

## Trim the metadata
meta.copy <- meta[meta$deployment_decimal_latitude < -10, ]

# Remove entries without deployment locations
meta.copy <- meta.copy[-which(is.na(meta.copy$deployment_decimal_latitude)), ]


#---------------------------------
## Plot
tiff("./out/figs/quickMap.tiff",
     height = 8,
     width = 11,
     units = "in",
     res = 300)

SOmap(trim = -10,
      bathy_legend = FALSE,
      border_col = c("white", "white"),
      border_width = 0.01,
      straight = TRUE,
      graticules = TRUE)
# plot(meanice, col = ocean.ice(125), add = TRUE, legend = TRUE)
# plot(meanice, col = ocean.ice(125), add = TRUE, legend.only = TRUE,
#      horizontal = TRUE,
#      legend.args = list(text = "Average proportion of year with sea ice cover >85%"))

# SOleg(ice, position = "topright", col = ocean.ice(125), ticks = 6,
#       tlabs = c("0", "20", "40", "60", "80", "100"),
#       trim = -10, label = "Sea Ice", type = "continuous")
SOplot(dat.copy$lon,
       dat.copy$lat,
       cex = 0.3, pch = 16, col = "#d95f02")
SOplot(meta.copy$deployment_decimal_longitude,
       meta.copy$deployment_decimal_latitude,
       cex = 1, pch = 16, col = "black")

plot(maxice, add = T, col = "white", lwd = 3)
plot(maxice, add = T, col = "#7570b3", lwd = 2)
plot(minice, add = T, col = "white", lwd = 3)
plot(minice, add = T, col = "#1b9e77", lwd = 2)

dev.off()

#---------------------------------
## Zoom on -50
dat.copy50 <- dat[dat$lat < -50, ]
meta.copy50 <- meta[meta$deployment_decimal_latitude < -50, ]

# Remove entries without deployment locations
meta.copy50 <- meta.copy50[-which(is.na(meta.copy50$deployment_decimal_latitude)), ]

tiff("./out/figs/quickMap50.tiff",
     height = 8,
     width = 12,
     units = "in",
     res = 300)

SOmap(trim = -50,
      bathy_legend = FALSE,
      border_col = c("white", "white"),
      border_width = 0.01,
      straight = TRUE,
      graticules = TRUE)
# plot(meanice, col = ocean.ice(125), add = TRUE, legend = TRUE)
# plot(meanice, col = ocean.ice(125), add = TRUE, legend.only = TRUE,
#      horizontal = TRUE,
#      legend.args = list(text = "Average proportion of year with sea ice cover >85%"))

# SOleg(ice, position = "topright", col = ocean.ice(125), ticks = 6,
#       tlabs = c("0", "20", "40", "60", "80", "100"),
#       trim = -10, label = "Sea Ice", type = "continuous")
SOplot(dat.copy50$lon,
       dat.copy50$lat,
       cex = 0.3, pch = 16, col = "#d95f02")
SOplot(meta.copy50$deployment_decimal_longitude,
       meta.copy50$deployment_decimal_latitude,
       cex = 1, pch = 16, col = "black")

plot(maxice, add = T, col = "white", lwd = 3)
plot(maxice, add = T, col = "#7570b3", lwd = 2)
plot(minice, add = T, col = "white", lwd = 3)
plot(minice, add = T, col = "#1b9e77", lwd = 2)

dev.off()

#--------------------------------------------------------------------------
## Quick summaries
meta.summary <- group_by(meta, dataset_identifier) %>%
  count(.)

dat.summary <- dat
dat.summary$month <- format(dat.summary$date, "%m")
dat.summary$year <- format(dat.summary$date, "%Y")
dat.summary <- group_by(dat.summary, year, month) %>%
  count(.)

p1 <- ggplot(dat.summary, aes(year, month, fill = n)) +
  geom_tile() +
  theme_bw()

p1

dat.summary$year <- as.integer(dat.summary$year)

p2 <- ggplot(dat.summary, aes(x = month, y = year, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Number of\nlocations") +
  coord_polar(theta = "x") +
  scale_y_continuous(limits = c(1999, 2020), breaks = seq(2002, 2019, 1),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.y = element_text(margin=margin(r=-260, l = 260),
                                   colour = "white"),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank())

tiff("./out/locs_by_month.tiff", width = 8, height = 8, units = "in", res = 300)
print(p2)
dev.off()
