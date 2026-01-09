# variable selection by collinearity and contribution

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(terra)
  library(tidyterra)
  library(tidyverse)
  library(tidymodels)
  library(tidysdm)
  library(future)
  library(miceRanger)
  library(vip)
}

# candidate variables
candidate_vars <- c("depth", "slope", "sst", "sal", 
                    "sic", "curr", "mld", "dshelf")

# read in environmentally subsampled data
data <- readRDS("output/subsampled/subsampled.RDS")

#-------------------------------------------------------
# Run collinearity testing
#-------------------------------------------------------

# packages for testing
library(car)
library(embarcadero)

# fit regression model
regdata <- data %>%
  mutate(pb1 = ifelse(pb == "presence", 1, 0)) %>%
  dplyr::select(pb1, all_of(candidate_vars))
m1 <- lm(pb1 ~ ., data = regdata)

# calculate variance inflation factor
original_scores <- vif(m1)
scores <- original_scores
scores

# all scores are below 5 so okay to continue

#-------------------------------------------------------
# Variable contribution testing
#-------------------------------------------------------

# balance presence and absence data
n_p <- nrow(regdata %>% filter(pb1 == 1))
regdata <- regdata %>%
  group_by(pb1) %>%
  slice_sample(n = n_p) %>%
  ungroup()

# rerun variable.step with the regression_vars
xdata <- regdata %>% dplyr::select(all_of(candidate_vars))
ydata <- regdata %>% rename(pb = pb1) %>% pull(pb)

# run varimp.diag
p1 <- varimp.diag(xdata, ydata, iter = 20)
p1

# get data from plot
p1data <- p1 %>% pluck("data")

# calculate range of importance for each variable and isolate scores at 10 and 20
p1data %>%
  group_by(variable) %>%
  summarise(diff = max(imp) - min(imp))
p1data %>%
  pivot_wider(names_from = trees, values_from = imp) %>%
  rename(ten = `10`, twenty = `20`) %>%
  dplyr::select(variable, ten, twenty)

# define key vars based on p1 and differences
# criteria: is the VarImp below 0.05 for any number of trees?
# criteria: does the VarImp decline when using 10 AND 20 trees?
# criteria: does the range of VarImp scores for that variable exceed 0.1 (if declining with fewer trees)?
key_reg_vars <- candidate_vars[!candidate_vars %in% c("curr")] # remove current - little contribution

# set regression dataframe to use key_vars only
regdata <- data %>%
  mutate(month = month(date)) %>%
  dplyr::select(pb, region, month, all_of(key_reg_vars))

# save regression dataframe for modelling
saveRDS(regdata,
        paste0("output/varselection/model_data.rds"))

# save the plots
ggsave(paste0("output/varselection/BART_varimp.png"),
       p1, width = 10, height = 10)

# save varimp data
write_csv(p1data, 
          paste0("output/varselection/BART_varimp.csv"))

# save key variable list
key_reg_var_df <- data.frame(key_vars = key_reg_vars)
write_csv(key_reg_var_df, 
          paste0("output/varselection/key_vars.csv"))

# save VIF scores
vif_scores <- data.frame(covariate = names(scores),
                           vif = scores, 
                           row.names = 1:length(scores))

write_csv(vif_scores,
          paste0("output/varselection/vif_scores.csv"))
