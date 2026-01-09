# master script to run all models

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(dplyr)
  library(lubridate)
  library(terra)
  library(tidyterra)
  library(tidymodels)
  library(themis)
  library(tidysdm)
  library(bonsai)
  library(butcher)
  library(bundle)
  library(future)
  library(DALEXtra)
  library(vip)
  library(dbarts)
}

# 1. Configuration 

# number of available cores
cores <- 8

# set seed
set.seed(777)

# read in the data for modelling
master_data <- readRDS("output/varselection/model_data.rds")

# 2. Source Modelling Scripts

# random forests
print("Random Forests")
source("code/scripts/06x_random_forests.R")

# boosted regression trees
print("Boosted Regression Trees")
source("code/scripts/06x_boosted_regression_trees.R")

# bayesian additive regression trees
print("Bayesian Additive Regression Trees")
source("code/scripts/06x_bayesian_additive_regression_trees.R")