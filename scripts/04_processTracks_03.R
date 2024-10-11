## Combine processed data, filter and fit movement models

## Ryan Reisinger
## March 2020

library(aniMotum)
library(dplyr)
library(sf)
library(ggplot2)
# library(SOmap)
library(lubridate)
library(geosphere)
library(pals)

## Get custom theme and plot sizes
source("./scripts/99_theme_and_fig_size.R")

hrs <- 6 # Time interval
what_speed <- 5 # Speed threshold

# -------------
## Metadata
# -------------

# Get processed metadata
meta <- read.csv("./out/fastOutMeta.csv", stringsAsFactors = F)

# -------------
## Tracks
# -------------

trk.fls <- list.files("./data_private/data_formatted/tracks/", full.names = T)
trk.fls <- trk.fls[grepl(".csv", trk.fls)]

tracks <- do.call(rbind, lapply(trk.fls, read.csv, stringsAsFactors = F))

# Replace old location classes
tracks <- tracks %>%
  dplyr::mutate(location_quality = ifelse(location_quality == "-9", "Z", location_quality)) %>%
  dplyr::mutate(location_quality = ifelse(location_quality == "-3", "Z", location_quality)) %>%
  dplyr::mutate(location_quality = ifelse(location_quality == "-2", "B", location_quality)) %>%
  dplyr::mutate(location_quality = ifelse(location_quality == "-1", "A", location_quality)) %>%
  dplyr::mutate(location_quality = ifelse(location_quality == "8", NA, location_quality)) %>%
  dplyr::mutate(location_quality = ifelse(location_quality == " ", NA, location_quality)) %>%
  dplyr::mutate(location_quality = ifelse(location_quality == "Z", "B", location_quality))

# -------------
# Dates
tracks$date <- ymd_hms(tracks$date)

# -------------
## Where there is metadata, filter locations before deployment date

ids <- unique(tracks$individual_id)
ids <- ids[!is.na(ids)]

new.dat <- data.frame()

for (i in ids) {
  print(i)
  this.dat <- tracks[tracks$individual_id == i, ]
  this.dat <- this.dat[order(this.dat$date), ]
  this.meta <- meta[meta$individual_id == i, ]
  if (nrow(this.meta) > 0 & this.meta$meta_source == "meta") {
    start.date <- paste(this.meta$year, this.meta$month, this.meta$day, sep = "-")
    start.date <- ymd(start.date)
    this.dat <- dplyr::filter(this.dat, this.dat$date >= start.date)
  }
  new.dat <- rbind(new.dat, this.dat)
}

tracks <- new.dat
rm(ids, new.dat)

# -------------
## Wrap longitudes
# tracks$decimal_longitude <- wrap_lon(tracks$decimal_longitude)

# -------------
# Filter individuals not to use
tracks <- dplyr::filter(tracks, individual_id != 112694)
# tracks <- dplyr::filter(tracks, individual_id != 20683)
tracks <- dplyr::filter(tracks, individual_id != "Mn_WAVES14_Lander01")
tracks <- dplyr::filter(tracks, individual_id != "Entangled whale")

# -------------
## Organise for foieGras

## Assign locaiton class to GPS tracks
tracks[tracks$device_type == "GPS", "location_quality"] <- "G"

## Create a dataframe for aniMotum
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

#--------------------------------------------------------------------------
## Remove short deployments
nlocs <- dat %>%
  group_by(id) %>%
  tally %>%
  filter(., n > 2)

dat <- dplyr::filter(dat, dat$id %in% nlocs$id)

## Drop the handful of records with no date information
dat <- dplyr::filter(dat, !is.na(dat$date))

## Drop records in the far northern hemisphere
## These are all test locations or errors
dat <- dplyr::filter(dat, dat$lat < 20)

## In Brazil data, drop 'Tagging' records
dat <- dplyr::filter(dat, dat$lc != "Tagging")

#--------------------------------------------------------------------------
## Prefilter
# dat <- aniMotum::fit_ssm(x = dat, pf = TRUE) # This is failing with a strange error

## Prefilter individually to avoid errors with projection
ids <- unique(dat$id)
all.d <- data.frame()


for (i in ids) {
  print(i)
  this.d <- dat[dat$id == i, ]
  this.d <- this.d[complete.cases(this.d), ]
  if(nrow(this.d) >2) {
  this.d <- fit_ssm(this.d, vmax = what_speed, pf = TRUE, spdf = TRUE)
  this.d <- st_transform(this.d, crs = "+proj=longlat +datum=WGS84")
  pts <- st_coordinates(this.d)
  that.d <- as.data.frame(this.d)[ , c("id", "date", "lc", "keep")]
  that.d$lon <- pts[,1]
  that.d$lat <- pts[,2]
  that.d <- that.d[that.d$keep == "TRUE", ]
  that.d$keep <- NULL
  all.d <- rbind(all.d, that.d)
  }
}

dat <- all.d
rm(all.d)

#--------------------------------------------------------------------------
## Split tracks with large gaps
## into segments

## Filter
nlocs <- dat %>%
  group_by(id) %>%
  tally %>%
  filter(., n > 2)

dat <- dplyr::filter(dat, dat$id %in% nlocs$id)

int.thresh <- 3 # Gap threshold in days

ids <- unique(dat$id)
all.d <- data.frame()

for (i in 1:length(ids)) {
  this.id <- ids[i]
  sub <- dat[dat$id == this.id, ]
  intervals <- diff(sub$date)
  units(intervals) <- "days"
  sub$int <- c(0, intervals)
  sub$dx <- 0
  sub[sub$int > int.thresh, "dx"] <- 1
  sub$dx2 <- cumsum(sub$dx)
  sub$id <- paste0(sub$id, "_segment", sub$dx2)
  sub$int <- NULL
  sub$dx <- NULL
  sub$dx2 <- NULL
  all.d <- rbind(all.d, sub)
}

dat <- all.d
rm(all.d)

# ----------------------
## Filter again to remove fragments
nlocs <- dat %>%
  group_by(id) %>%
  tally %>%
  filter(., n > 2)

dat <- dplyr::filter(dat, dat$id %in% nlocs$id)


#--------------------------------------------------------------------------
## Fit the SSM in foieGras
if(TRUE) {
  
  hrs <- hrs # Time interval
  
  # Fit SSM id by id to detect any problems
  out <- data.frame()
  these_aic <- data.frame()
  ids <- unique(dat$id)
  for (i in ids) {
    print(i)
    this_dat <- dplyr::filter(dat, id == i)
    tryCatch({
      ## Fit SSM
      this_fit <- aniMotum::fit_ssm(this_dat, pf = FALSE, spdf = FALSE,
                                    model = "rw",
                                    time.step = hrs,
                                    vmax = what_speed)
      ## Grab the output
      this_out <- grab(this_fit, "predicted", as_sf = FALSE)
      ## Store the aic
      this_aic <- this_fit$ssm[[1]]$AICc
    }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    if (nrow(this_out) > 0) {
      out <- rbind(out, this_out)
      these_aic <- rbind(these_aic, this_aic)
    }
  }
  
  
  # Filter out tracks with less than 3 points
  nlocs <- out %>%
    group_by(id) %>%
    tally %>%
    filter(., n > 2)
  out <- dplyr::filter(out, out$id %in% nlocs$id)
  
  # Check a plot of each track
  if (TRUE) {
    ids <- unique(out$id)
    
    pdf("./out/checkTracksAniMotum.pdf", paper = "a4", useDingbats = F)
    for (i in ids) {
      print(i)
      this.d <- out[out$id == i, ]
      p <- ggplot() +
        geom_path(data = this.d, aes(x = lon, y = lat), color = "black") +
        geom_point(data = this.d, aes(x = lon, y = lat), color = "blue") +
        theme_minimal() +
        labs(title = i)
      print(p)
    }
    dev.off()
  }
  
}

# Save
saveRDS(out, paste0("./out/aniMotum_ssm_", hrs, ".RDS"))

## What is the distribution of x and y uncertainty
loc_se <- data.frame()
loc_se <- rbind(loc_se, summary(out$x.se))
loc_se <- rbind(loc_se, summary(out$y.se))
names(loc_se) <- names(summary(out$y.se))
loc_se$which <- c("x", "y")
loc_se$timestep <- hrs
write.csv(loc_se, paste0("./out/location_error_", hrs, ".csv"), row.names = F)

# Write the AICs
names(these_aic) <- "AIC"
these_aic$timestep <- hrs
write.csv(these_aic, paste0("./out/ssm_aic_", hrs, ".csv"), row.names = FALSE)

## Fit a move persistence model
mpm_out <- data.frame()
ids <- unique(out$id)

for (i in ids) {
  print(i)
  this_out <- dplyr::filter(out, id == i)
  tryCatch({
this_mpm <- fit_mpm(x = this_out, model = "mpm")
mpm_out_dat <- grab(this_mpm, what = "data", as_sf = F)
mpm_out_fit <- grab(this_mpm, what = "fitted", as_sf = F)
this_mpm_out <- cbind(mpm_out_dat, mpm_out_fit[ , c("g", "logit_g", "logit_g.se")])
rm(mpm_out_dat, mpm_out_fit)
mpm_out <- rbind(mpm_out, this_mpm_out)
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

## Save output
mpm_out$lon <- mpm_out$x
mpm_out$lat <- mpm_out$y

saveRDS(mpm_out, paste0("./out/aniMotum_mpm_", hrs, ".RDS"))

## How many segments?
length(unique(mpm_out$id))

# How many individuals?
splitfun <- function(string = "53348_segment0") {
split_string <- strsplit(string, "_")
split_string <- unlist(split_string)[1]
return(split_string)
}
split_ids <- lapply(mpm_out$id, splitfun)
length(unique(split_ids))

## Plot move persistence against latitude
## to get an idea of a sensible cutoff
tiff("./out/figs/density_persistence_by_lat.tiff", width = 8, height = 6, units = "in", res = 300)
ggplot(data = mpm_out, aes(x = lat, y = g)) +
  geom_bin2d() +
  geom_smooth(colour = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(name = "Number of\nlocations") +
  labs(x = "Latitude", y = "Move persistence") +
  theme_bw() # Spike in move persistence around -40 deg
dev.off()

# Plot with IWC management areas
ggplot(data = mpm_out, aes(x = lon, y = lat, colour = g)) +
  geom_point() +
  coord_quickmap() +
  scale_colour_viridis_c() +
  # IWC management areas:
  geom_vline(xintercept = -170) +
  geom_vline(xintercept = -120) +
  geom_vline(xintercept = -60) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 70) +
  geom_vline(xintercept = 130)

## Calculate bearings for map

# TODO: Replace SOmap here
## Map in SOmap
prj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
my_points_ll <- mpm_out[mpm_out$lat < -40, c("lon", "lat", "g")]
coordinates(my_points_ll) <- c("lon", "lat")
projection(my_points_ll) <- "+proj=longlat +datum=WGS84"
my_points_ll$b <- geosphere::bearing(my_points_ll) # add bearing for maps TODO: individual by individual
my_points <- SOproj(my_points_ll, target = prj)

gg <- plot(SOgg(SOmap(trim = -40,
                      bathy_legend = FALSE,
                      border_col = c("white", "white"),
                      border_width = 0.01,
                      straight = TRUE,
                      graticules = TRUE)))

# Move persistence map
tiff("./out/figs/quickMapMovePersistence.tiff",
     height = 8,
     width = 8,
     units = "in",
     res = 300)
gg + geom_point(data = as.data.frame(my_points), aes(lon, lat, colour = g), size = 1) +
  scale_colour_viridis_c(name = "Move\npersistence", option = "plasma")
dev.off()

# Bearing map
clrs <- ocean.balance(100)
min_bearing <- min(-1*abs(as.data.frame(my_points)$b), na.rm = T)
max_bearing <- max(-1*abs(as.data.frame(my_points)$b), na.rm = T)

tiff("./out/figs/quickMapBearing.tiff",
     height = 8,
     width = 8,
     units = "in",
     res = 300)
gg + geom_point(data = as.data.frame(my_points), aes(lon, lat, colour = -1*abs(b)), size = 1) +
  scale_colour_gradientn(name = "Bearing", colours = clrs, breaks = c(min_bearing, -90, max_bearing), labels = c("South", "East/West", "North"))
dev.off()


## Look at longitudinal distribution
pdf("./out/figs/longitudinal_distribution.pdf", paper = "a4", useDingbats = F, width = 7, height = 7)
ggplot(data = mpm_out, aes(x = lon)) +
  geom_histogram(binwidth = 5, aes(fill = ..count..)) +
  scale_x_continuous(limits = c(-180, +180), breaks = seq(-180, 180, 45)) +
  # scale_y_log10(limits = c(-250, 250)) +
  scale_y_continuous(limits = c(-300, 500)) +
  # IWC managment areas:
  geom_vline(xintercept = -170) +
  geom_vline(xintercept = -120) +
  geom_vline(xintercept = -60) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 70) +
  geom_vline(xintercept = 130) +
  coord_polar(start = pi) +
  theme_minimal()
dev.off()





#--------------------------------------------------------------------------
## OR
## Alternatively to fitting a mpm, 
## fit the SSM in bsam
if (FALSE) {
  library(bsam)
  fit <- bsam::fit_ssm(dat, tstep = 0.5, model = "DCRWS") # Fails
}

#--------------------------------------------------------------------------
## OR
## Fit bsam SSMs one-by-one, skipping errors # Doesn't deal with dateline crossing
## Save each individual, for running in batches
if (FALSE) {
  ids <- unique(dat$id)
  all.fit <- data.frame()
  
  for (i in 1:length(ids)) {
    print(ids[i])
    this.d <- dat[dat$id == ids[i], ]
    
    this.fit <- try(bsam::fit_ssm(data = this.d, tstep = 0.5, model = "DCRWS"))
    
    if(class(this.fit) == "try-error") {
      this.fit <- NULL
      that.dat <- NULL } else {
        that.dat <- this.fit[[1]]$summary
        saveRDS(that.dat, paste0("./out/bsam_fits/fit_", i, "_", ids[i], ".RDS")) # save each id for batch runs
        all.fit <- rbind(all.fit, that.dat)
      }
    
    rm(this.fit, that.dat)
    
  }
  
  # Check a plot of each track
  ids <- unique(all.fit$id)
  
  pdf("./out/checkTracksBSAM.pdf", paper = "a4", useDingbats = F)
  for (i in ids) {
    print(i)
    this.d <- all.fit[all.fit$id == i, ]
    print(SOmap_auto(this.d$lon, this.d$lat, pcol = as.factor(this.d$b.5)), main = i)
  }
  dev.off()
  
}