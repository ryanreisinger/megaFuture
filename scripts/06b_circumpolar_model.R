#circumpolar random forest combining all regional models

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Humpbacks")

{
  library(tidyverse)
  library(caret)
  library(ranger)
  library(miceRanger)
}

# 1. Create dataframe with all tracks + background samples including extracted covariates

#define predictors
predictors <- c("depth", "slope", "dshelf", "sst", "mld", "sal", 
                "ssh", "sic", "month")

#create vector of all populations
pops <- c("EastAtlantic", "EastIndian", "EastPacific", "Pacific", "WestAtlantic", "WestIndian", "WestPacific")

#setup null variable for all extracted fixes
extractions <- NULL

#loop over all populations
for(i in pops){
  
  #define population
  this.pop <- i
  
  #read in tracks and background data
  tracks <- readRDS(paste0("output/extraction/", this.pop, "_presences_extracted.RDS"))
  back <- readRDS(paste0("output/extraction/", this.pop, "_background_extracted.RDS"))
  
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
  
  #join dataframe with data for all populations
  extractions <- bind_rows(extractions, data)
  
  #cleanup
  rm(tracks, back, data, this.pop)
}

#remove predictors with more than 10% NA values
extractions <- extractions[colSums(is.na(extractions)) < 0.1*nrow(extractions)]

#impute missing values
imp <- miceRanger(extractions, m = 1)
extractions <- completeData(imp)[[1]]
extractions <- extractions %>% 
  mutate(individual_id = as.factor(individual_id))

#export all data
saveRDS(extractions, file = "output/extraction/circumpolar_extracted.RDS")

#cleanup
rm(imp, i)


# 2. Predict each regional model to all data

#start here if happy with file
extractions <- readRDS("output/extraction/circumpolar_extracted.RDS")

#loop over every population 
for(i in pops){
  
  #define population
  this.pop <- i
  
  #load RF model for this population
  pop_rf <- readRDS(paste0("output/random_forests/", this.pop, "_rf.RDS"))
  
  #predict model to data
  extractions$predval <- predict(pop_rf, newdata = extractions, type = "prob")$presence
  
  #rename column to population
  extractions <- extractions %>%
    rename(!! sym(this.pop) := predval)
  
  #cleanup
  rm(pop_rf, this.pop)
}


# 3. Create circumpolar model using regional predictions

#establish folds by ID
folds <- groupKFold(group = extractions$individual_id, k = 10)

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
X <- extractions %>% select(all_of(predictors), all_of(pops))
Y <- as.factor(extractions$pa) 

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
varimpR(rf, mod_name = "circumpolar")

#save model
saveRDS(rf, "output/random_forests/circumpolar_rf.RDS")
