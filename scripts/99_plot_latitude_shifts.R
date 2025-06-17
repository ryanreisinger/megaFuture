# Working with predictions and projections

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

dir <- "out/large_ignore/"

# Get consistent theme
source("scripts/99_theme_and_fig_size.R")

#-------------------------------------
# Load the monthly average predictions
pred_01 <- rast(paste0(dir, "predictions/monthly_avg_January.nc"))
pred_02 <- rast(paste0(dir, "predictions/monthly_avg_February.nc"))
pred_03 <- rast(paste0(dir, "predictions/monthly_avg_March.nc"))
pred_04 <- rast(paste0(dir, "predictions/monthly_avg_April.nc"))
pred_05 <- rast(paste0(dir, "predictions/monthly_avg_May.nc"))
pred_10 <- rast(paste0(dir, "predictions/monthly_avg_October.nc"))
pred_11 <- rast(paste0(dir, "predictions/monthly_avg_November.nc"))
pred_12 <- rast(paste0(dir, "predictions/monthly_avg_December.nc"))

# Load the monthly projections
# SSP585
proj_01_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_January_ssp585.nc"))
proj_02_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_February_ssp585.nc"))
proj_03_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_March_ssp585.nc"))
proj_04_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_April_ssp585.nc"))
proj_05_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_May_ssp585.nc"))
proj_10_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_October_ssp585.nc"))
proj_11_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_November_ssp585.nc"))
proj_12_585 <- rast(paste0(dir, "projections/ssp585/monthly_avg_December_ssp585.nc"))

# SSP126
proj_01_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_January_ssp126.nc"))
proj_02_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_February_ssp126.nc"))
proj_03_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_March_ssp126.nc"))
proj_04_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_April_ssp126.nc"))
proj_05_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_May_ssp126.nc"))
proj_10_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_October_ssp126.nc"))
proj_11_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_November_ssp126.nc"))
proj_12_126 <- rast(paste0(dir, "projections/ssp126/monthly_avg_December_ssp126.nc"))

#-------------------------------------
# Function to compute differrence by latitude plots

# present <- pred_01
# future_126 <- proj_01_126
# future_585 <- proj_01_585

diffR <- function(present, future_126, future_585) {

  # Force rasters to have the same extent and resolution
  present <- resample(present, future_126, method = "bilinear")
  
  # Create a latitude raster using the y-coordinates
  lat_raster <- init(present, fun = "y")
  
  # Stack all four rasters together
  suit_stack <- c(present, future_126, future_585, lat_raster)
  names(suit_stack) <- c("Present", "Future_SSP126", "Future_SSP585", "Latitude")
  
  # Convert to data frame with all values
  df <- as.data.frame(suit_stack, xy = TRUE, na.rm = FALSE)
  
  # Drop NA rows (cells that are NA in any layer)
  df <- df %>% filter(!is.na(Present) & !is.na(Future_SSP126) & !is.na(Future_SSP585))
  
  # Define latitude bands (e.g., every 2°)
  df <- df %>%
    mutate(lat_band = cut(Latitude, breaks = seq(min(df$y), max(df$y), by = 2), include.lowest = TRUE))
  
  # Summarise by latitude band
  summary <- df %>%
    group_by(lat_band) %>%
    summarise(
      lat_mid = mean(Latitude, na.rm = TRUE),
      mean_present = mean(Present, na.rm = TRUE),
      mean_ssp126 = mean(Future_SSP126, na.rm = TRUE),
      mean_ssp585 = mean(Future_SSP585, na.rm = TRUE),
      diff_126 = mean_ssp126 - mean_present,
      diff_585 = mean_ssp585 - mean_present
    )
return(summary)
}

#-------------------------------------
# Run diffR for each month
# January
summary_01 <- diffR(present = pred_01,
                    future_126 = proj_01_126,
                    future_585 = proj_01_585)
# February
summary_02 <- diffR(present = pred_02,
                    future_126 = proj_02_126,
                    future_585 = proj_02_585)
# March
summary_03 <- diffR(present = pred_03,
                    future_126 = proj_03_126,
                    future_585 = proj_03_585)
# April
summary_04 <- diffR(present = pred_04,
                    future_126 = proj_04_126,
                    future_585 = proj_04_585)
# May
summary_05 <- diffR(present = pred_05,
                    future_126 = proj_05_126,
                    future_585 = proj_05_585)
# October
summary_10 <- diffR(present = pred_10,
                    future_126 = proj_10_126,
                    future_585 = proj_10_585)
# November
summary_11 <- diffR(present = pred_11,
                    future_126 = proj_11_126,
                    future_585 = proj_11_585)
# December
summary_12 <- diffR(present = pred_12,
                    future_126 = proj_12_126,
                    future_585 = proj_12_585)

#-------------------------------------
# Plot function

plotR <- function(summary_data, month_name){
p <- ggplot(summary_data, aes(x = lat_mid)) +
  geom_line(aes(y = mean_present, color = "Present"), size = .75) +
  geom_line(aes(y = mean_ssp126, color = "SSP1-2.6"), size = .75) +
  geom_line(aes(y = mean_ssp585, color = "SSP5-8.5"), size = .75) +
  scale_y_continuous(limits = c(0, 0.3), expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(limits = c(-75, -40), expand = expansion(mult = c(0, 0))) +
  scale_color_manual(values = c("Present" = "black", "SSP1-2.6" = "#56B4E9", "SSP5-8.5" = "#E69F00")) +
  labs(title = month_name, x = "Latitude (°)", y = "Mean Habitat Suitability", color = "Scenario") +
  theme_custom()
return(p)
}

#-------------------------------------
# Make plots and combine
p_01 <- plotR(summary_01, month_name = "January")
p_02 <- plotR(summary_02, month_name = "February")
p_03 <- plotR(summary_03, month_name = "March")
p_04 <- plotR(summary_04, month_name = "April")
p_05 <- plotR(summary_05, month_name = "May")
p_10 <- plotR(summary_10, month_name = "October")
p_11 <- plotR(summary_11, month_name = "November")
p_12 <- plotR(summary_12, month_name = "December")


all_months <- p_10 + p_11 + p_12 + p_01 + p_02 +
  p_03 + p_04 + p_05 +
  plot_layout(ncol = 1,
                                        guides = "collect",
                                        axes = "collect")
png(paste0("./out/figures/latitude_shifts_monthly.png"),
    width = single_col_in*300,
    height = 2000,
    res = 300)
all_months
dev.off()

#-------------------------------------
 # Show change using barplots

# Function to produce bar chart showing diffrerences

changeR <- function(summary_data, month_name) {
# Reshape to long format for faceted plotting
summary_long <- summary_data %>%
  select(lat_mid, diff_126, diff_585) %>%
  pivot_longer(cols = starts_with("diff"), names_to = "Scenario", values_to = "Change") %>%
  mutate(Scenario = recode(Scenario,
                           diff_126 = "SSP1-2.6",
                           diff_585 = "SSP5-8.5"))

# Bar plot by scenario
p <- ggplot(summary_long, aes(x = lat_mid, y = Change, fill = Scenario)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  labs(title = month_name, x = "Latitude (°)",
       y = "Change in Suitability (Future - Present)",
       fill = "Scenario") +
  scale_y_continuous(limits = c(-0.15, +0.15), expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(values = c("SSP1-2.6" = "#56B4E9", "SSP5-8.5" = "#E69F00")) +
  theme_custom()

return(p)

}

#-------------------------------------
bar_01 <- changeR(summary_01, month_name = "January")
bar_02 <- changeR(summary_02, month_name = "February")
bar_03 <- changeR(summary_03, month_name = "March")
bar_04 <- changeR(summary_04, month_name = "April")
bar_05 <- changeR(summary_05, month_name = "May")
bar_10 <- changeR(summary_10, month_name = "October")
bar_11 <- changeR(summary_11, month_name = "November")
bar_12 <- changeR(summary_12, month_name = "December")

# Combine all bar plots into one figure
all_bars <- bar_10 + bar_11 + bar_12 + bar_01 + bar_02 +
  bar_03 + bar_04 + bar_05 +
  plot_layout(ncol = 1,
              guides = "collect",
              axes = "collect")

png(paste0("./out/figures/latitude_shifts_monthly_bars.png"),
    width = single_col_in*300,
    height = 2000,
    res = 300)
all_bars
dev.off()
