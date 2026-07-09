#-------------------------------------------------------------------------------
# Key model outputs including cross-validation, PDPs, and variable importance
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

library(tidyverse)
library(tidymodels)

# Get consistent theme
source("code/scripts/99_theme_and_fig_size.R")

#-------------------------------------------------------------------------------
# 1. Cross-validation results
#-------------------------------------------------------------------------------
# can also use this for final hyperparameter configurations

# read in cross-validation results
rf <- readRDS("output/random forests/cbi_scores.rds")
brt <- readRDS("output/boosted regression trees/cbi_scores.rds")
bart <- readRDS("output/bayesian additive regression trees/cbi_scores.rds")

# combine into one dataframe
data <- bind_rows(
  rf %>% select(region, .estimate) %>% mutate(algo = "RF"),
  brt %>% select(region, .estimate) %>% mutate(algo = "BRT"),
  bart %>% select(region, .estimate) %>% mutate(algo = "BART")
)

# organise regions
data <- data %>%
  mutate(region = factor(region, levels = c("WestAtlantic", "EastAtlantic",
                                            "WestIndian", "EastIndian",
                                            "WestPacific", "Pacific", "EastPacific"))) %>%
  mutate(region = recode(region,
                         "WestAtlantic" = "West Atlantic",
                         "EastAtlantic" = "East Atlantic",
                         "WestIndian" = "West Indian",
                         "EastIndian" = "East Indian",
                         "WestPacific" = "West Pacific",
                         "Pacific" = "Central Pacific",
                         "EastPacific" = "East Pacific"))

# calculate mean and sd per algorithm
summary_data <- data %>%
  group_by(algo) %>%
  summarise(mean_cbi = mean(.estimate),
            sd_cbi = sd(.estimate))

# plot
p1 <- ggplot() +
  geom_jitter(data = data, aes(x = algo, y = .estimate, col = region), 
              shape = 4, width = 0.1) +
  labs(x = "Algorithm", y = "Continuous Boyce Index (CBI)") +
  theme_custom() +
  scale_color_manual(values = c("#F2CF91", "#DB941A",
                                "#00B4CC", "#007E8F",
                                "#E27865", "#DB5943", "#892B1A"),
                     name = "") +
  ylim(0.5,1) +
  theme(panel.grid.minor = element_blank())
p1 + ggview::canvas(width = 5, height = 5)

# export 
ggsave("output/imagery/supplementary/model_performance_scores.png", 
       plot = p1, width = 5, height = 5, units = "in", dpi = 300)

# summary stats for results section
summary_data


#-------------------------------------------------------------------------------
# 2. Variable Importance Scores
#-------------------------------------------------------------------------------

# clear up
rm(list=ls())

# read in variable importance results
rf <- readRDS("output/random forests/varimp_scores.rds")
brt <- readRDS("output/boosted regression trees/varimp_scores.rds")
bart <- readRDS("output/bayesian additive regression trees/varimp_scores.rds")

# scale importance scores to 0-1 range for each algorithm
rf <- rf %>%
  filter(Variable != "month") %>%
  mutate(importance_scaled = Importance / max(Importance),
         algo = "Random Forest")

brt <- brt %>%
  filter(Variable != "month") %>%
  mutate(importance_scaled = Importance / max(Importance),
         algo = "Boosted Regression Trees")
bart <- bart %>%
  filter(Variable != "month") %>%
  mutate(importance_scaled = Importance / max(Importance),
         algo = "Bayesian Additive Regression Trees")

# combine into one dataframe
data <- bind_rows(rf, brt, bart) 

# capitalise variable names for plotting
data <- data %>%
  mutate(Variable = case_when(
    Variable == "sst" ~ "SST",
    Variable == "sal" ~ "SSS",
    Variable == "mld" ~ "MLD",
    Variable == "sic" ~ "SIC",
    Variable == "dshelf" ~ "DShelf",
    Variable == "depth" ~ "Depth",
    Variable == "slope" ~ "Slope"))

data %>% 
  group_by(Variable) %>%
  summarise(mean_importance = mean(importance_scaled)) %>%
  arrange(-mean_importance)
    

# plot
source("code/scripts/99_theme_and_fig_size.R")
p2 <- ggplot(data, aes(x = reorder(Variable, importance_scaled), y = Importance, fill = algo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~algo, scales = "free_x") +
  labs(x = "Variable", y = "Variable Importance") +
  theme_custom() +
  scale_fill_manual(values = c("Random Forest" = "#F2CF91",
                               "Boosted Regression Trees" = "#00B4CC",
                               "Bayesian Additive Regression Trees" = "#E27865"),
                    guide = "none") 
p2 + ggview::canvas(width = 10, height = 5)

# export
ggsave("output/imagery/supplementary/variable_importance_scores.png", 
       plot = p2, width = 10, height = 5, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
# 3. Partial Dependence Plots
#-------------------------------------------------------------------------------

# clear up
rm(list=ls())

# read in PDP results
rf <- readRDS("output/random forests/pdp_values.rds")
brt <- readRDS("output/boosted regression trees/pdp_values.rds")
bart <- readRDS("output/bayesian additive regression trees/pdp_values.rds")

# append algorithm names
rf <- rf %>% mutate(algorithm = "Random Forest")
brt <- brt %>% mutate(algorithm = "Boosted Regression Trees")
bart <- bart %>% mutate(algorithm = "Bayesian Additive Regression Trees")

# join all together
pdp <- bind_rows(rf, brt, bart)

# remove month
pdp <- pdp %>% filter(var != "month")

# scale all yhat values to 0-1
pdp <- pdp %>%
  group_by(var, algorithm) %>%
  mutate(yhat = (yhat - min(yhat)) / (max(yhat) - min(yhat))) %>%
  ungroup()

# rename variables
var_names <- c(
  "depth" = "Depth (m)",
  "slope" = "Seafloor Slope (°)",
  "dshelf" = "Distance to Shelf Break (km)",
  "sst" = "Sea Surface Temperature (°C)",
  "sal" = "Salinity (PSU)",
  "mld" = "Mixed Layer Depth (m)",
  "sic" = "Sea Ice Concentration (%)",
  "curr" = "Current Velocity (m/s)"
)
pdp <- pdp %>%
  mutate(var = recode(var, !!!var_names))

# plot
source("code/scripts/99_theme_and_fig_size.R")
p3 <- ggplot(pdp, aes(x = x, y = yhat)) +
  geom_line(aes(group = algorithm), col = "grey70", lwd = 0.5, alpha = 0.75) +
  geom_smooth(method = "gam", se = F, col = "black", lwd = 1) +
  facet_wrap(~var, scales = "free_x", nrow = 2) + 
  ylim(-0.01, 1.01) + 
  theme_custom() +
  ylab("Scaled Partial Effect") + 
  xlab("Predictor Values") 
p3 + ggview::canvas(width = 10, height = 6)

# export
ggsave("output/imagery/supplementary/partial_dependence_plots.png",
       plot = p3, width = 10, height = 6, units = "in", dpi = 300)
