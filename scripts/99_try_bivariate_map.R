library(terra)
library(dplyr)
library(ggplot2)
library(classInt)
library(tidyterra)

# Directory for rasters
dir <- "out/large_ignore/"

# Get consistent theme
source("scripts/99_theme_and_fig_size.R")

# Load rasters
present <- rast(paste0(dir, "predictions/monthly_avg_January.nc"))
future <- rast(paste0(dir, "projections/ssp585/monthly_avg_January_ssp585.nc"))

# Make same extent and res
present <- resample(present, future, method = "bilinear")

# Convert to data frame
df <- as.data.frame(c(present, future), xy = TRUE, na.rm = TRUE)
names(df)[3:4] <- c("present", "future")

# Create quantile bins for both
quantiles <- c(0, 0.33, 0.66, 1)
df <- df %>%
  mutate(
    present_q = cut(present, breaks = quantile(present, quantiles), labels = c(1, 2, 3), include.lowest = TRUE),
    future_q = cut(future, breaks = quantile(future, quantiles), labels = c(1, 2, 3), include.lowest = TRUE),
    bivar_class = as.numeric(future_q) * 10 + as.numeric(present_q)  # e.g., 11, 12,..., 33
  )

# Bivariate palette (as matrix for clarity)
bivariate_pal <- c(
  "11" = "#e8e8e8", "12" = "#ace4e4", "13" = "#5ac8c8",
  "21" = "#dfb0d6", "22" = "#a5add3", "23" = "#5698b9",
  "31" = "#be64ac", "32" = "#8c62aa", "33" = "#3b4994"
)

ggplot(df, aes(x = x, y = y)) +
  geom_raster(aes(fill = factor(bivar_class))) +
  scale_fill_manual(values = bivariate_pal, name = "Bivariate class") +
  coord_equal() +
  theme(legend.position = "none") +
  labs(title = "Bivariate Habitat Suitability: Present vs Future")

# Create dummy matrix for legend
legend_df <- expand.grid(present = c(1, 2, 3), future = c(3, 2, 1))
legend_df$bivar_class <- legend_df$future * 10 + legend_df$present
legend_df$col <- bivariate_pal[as.character(legend_df$bivar_class)]

ggplot(legend_df, aes(x = present, y = future, fill = col)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Med", "High")) +
  scale_y_continuous(breaks = 1:3, labels = c("High", "Med", "Low")) +
  labs(x = "Present Suitability", y = "Future Suitability", title = "Bivariate Legend") +
  theme_minimal()
