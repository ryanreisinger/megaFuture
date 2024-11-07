# Summary of tracking data

setwd("C:\\Users\\Ryan Reisinger\\Documents\\Academic\\UCSC\\Work\\Analysis\\mega")

library(dplyr)
library(ggplot2)

#--------------------------------------
## Tracks

trk.fls <- list.files("./data_formatted/tracks/", full.names = T)
trk.fls <- trk.fls[grepl(".csv", trk.fls)]

tracks <- do.call(rbind, lapply(trk.fls, read.csv, stringsAsFactors = F))

length(unique(tracks$individual_id))

nrow(tracks)
min(tracks$date, na.rm = T)
max(tracks$date, na.rm = T)

#--------------------------------------
## Summarise collated data - metadata
meta <- read.csv("./out/fastOutMeta.csv", stringsAsFactors = F)

# Group Friedaender datasets
meta[which(grepl(pattern = "Friedlaender", x = meta$dataset_identifier)), "dataset_identifier"] <- "Friedlaender"

# Group
meta_summary <- group_by(meta, dataset_identifier) %>%
  summarise(., n())

print(meta_summary)

# Total tracks by contributor
sum(meta_summary$`n()`)

#--------------------------------------
# Filtered and fitted data

# Get fitted data
dat <- readRDS("./out/foieGras_mpm_withbreedingstock.RDS")

# Filter south of 40S
dat <- dat[dat$lat < -40, ]

# N tracks
length(unique(dat$id_original))
length(unique(dat$id))

# Dates
min(dat$date)
max(dat$date)

# Summary
ids_retained <- unique(dat$id_original)
meta <- dplyr::filter(meta, meta$individual_id %in% ids_retained)
meta <- group_by(meta, dataset_identifier) %>%
  count(.)
print(meta)
sum(meta$n)


# N tracking days
dat.summary <- dat
dat.summary$day <- format(dat.summary$date, "%d")
dat.summary$month <- format(dat.summary$date, "%m")
dat.summary$year <- format(dat.summary$date, "%Y")

foo <- group_by(dat.summary, year, month, day, id_original) %>%
  count(.)
foo$n <- NULL
foo <- group_by(foo, id_original) %>%
  count(.)
sum(foo$n)
mean(foo$n)
min(foo$n)
max(foo$n)

rm(foo)

# Plot of monthly effort
dat.summary <- group_by(dat.summary, year, month) %>%
  count(.)

p1 <- ggplot(dat.summary, aes(year, month, fill = n)) +
  geom_tile() +
  theme_bw()
p1


# Look at nlocs by year and month
ggplot(group_by(dat.summary, year) %>% summarise(n  = sum(n)), aes(x=year, y=n)) + geom_point()
ggplot(group_by(dat.summary, month) %>% summarise(n  = sum(n)), aes(x=month, y=n)) + geom_point()

# Proportion by certain years and months

# Months
foo <- group_by(dat.summary, month) %>% summarise(n  = sum(n))
n_total <- sum(foo$n)
n_foo <- sum(filter(foo, month %in% c("11", "12", "01", "02", "03"))$n)
n_foo/n_total

# Years
foo <- group_by(dat.summary, year) %>% summarise(n  = sum(n))
n_total <- sum(foo$n)
n_foo <- sum(filter(foo, year %in% c("2013", "2014", "2015", "2016", "2017"))$n)
n_foo/n_total

# Year as integer
dat.summary$year <- as.integer(dat.summary$year)

p2 <- ggplot(dat.summary, aes(x = month, y = year, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Number of\nlocations") +
  coord_polar(theta = "x") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits = c(1999, 2020), breaks = seq(2002, 2019, 1),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(size = 9),
        axis.text.y = element_text(margin=margin(r=-170, l = 170),
                                   colour = "white", size = 6),
        axis.text.x = element_text(colour = "black",
                                 size = 8),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank())

pdf("./out/figs/locs_by_month_filtered_dat.pdf", width = 5.5, height = 5.5, useDingbats = F)
print(p2)
dev.off()