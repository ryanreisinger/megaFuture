#run Random Forests

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(caret)
  library(ranger)
  library(miceRanger)
}

#list of all populations
pops <- c("EastAtlantic", "EastIndian", "EastPacific", "Pacific", "WestAtlantic", "WestIndian", "WestPacific")

#loop over each populations 
for(i in pops){
  
  #define population
  this.pop <- i
  
  #read in tracks and background data
  tracks <- readRDS(paste0("output/extraction/", this.pop, "_presences_extracted.RDS"))
  back <- readRDS(paste0("output/extraction/", this.pop, "_background_extracted.RDS"))
  
  #define predictors
  predictors <- c("depth", "slope", "dshelf", "sst", "mld", 
                  "sal", "ssh", "sic", "month")
  
  
  # 1. Formatting
  
  #create presence-absence column
  tracks$pa <- "presence"
  back$pa <- "absence"
  
  #create month covariate
  tracks <- tracks %>%
    mutate(month = month(date))
  back <- back %>%
    mutate(month = month(date))
  
  #bind dataframes together
  tracks <- tracks %>% select(all_of(predictors), pa, individual_id)
  back <- back %>% select(all_of(predictors), pa, individual_id)
  data <- bind_rows(tracks, back)
  
  #cleanup
  rm(tracks, back)
  
  
  # 2. Create Random Forests
  
  #remove predictors with more than 10% NA values
  data <- data[colSums(is.na(data)) < 0.1*nrow(data)]
  predictors <- names(data)
  predictors <- subset(predictors, predictors != "pa" & predictors != "individual_id")
  
  #impute missing values
  if(sum(is.na(data)) > 0){
    imp <- miceRanger(data, m = 1)
    data <- completeData(imp)[[1]]
  }
  
  #set individual ID as factor
  data <- data %>% 
    mutate(individual_id = as.factor(individual_id))
  
  #set number of folds for cross-validation (default 10)
  k_number <- 10
  
  #if fewer than 10 individuals in data, set k_number to number of individuals - 1
  if(nlevels(data$individual_id) < 10){
    k_number <- nlevels(data$individual_id) - 1
  }
  
  #establish folds by ID
  folds <- groupKFold(group = data$individual_id, k = k_number)
  
  #setup parameter grid
  param_grid <- expand.grid(mtry = 2:4, 
                            splitrule = "gini", 
                            min.node.size = 1)
  
  #setup cross-validation scheme
  cv_scheme <- trainControl(method = "cv",
                            number = length(folds),
                            search = "grid",
                            classProbs = TRUE,
                            sampling = "down",
                            summaryFunction = twoClassSummary,
                            index = folds)
  
  #isolate covariates and pa
  X <- data %>% select(all_of(predictors))
  Y <- as.factor(data$pa) 
  
  #fit the model
  rf <- train(x = X,
              y = Y,
              method = "ranger",
              metric = "ROC",
              trControl = cv_scheme,
              tuneGrid = param_grid,
              importance = "impurity")
  
  #inspect
  rf
  
  #variable importance
  varImp(rf)
  
  #plot and export variable importance
  source("code/functions/varimpR.R")
  varimpR(rf, this.pop)
  
  #save model
  saveRDS(rf, paste0("output/random_forests/", this.pop, "_rf.RDS"))
}
