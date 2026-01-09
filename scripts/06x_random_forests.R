# random forests script to run within 06_run_all_models

# convert pb to ordered factor
data <- master_data %>% mutate(pb = as.factor(pb))
data$pb <- ordered(data$pb, levels = c("presence", "background"))

# set number of folds to number of breeding stocks
v <- length(unique(data$region))

# define RF
rf_mod <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger", #use ranger package
             importance = "impurity" #gini index for importance
  ) %>%
  set_args(trees = 1000, #1000 trees
           mtry = tune(), #tune mtry
           min_n = 1) #minimum number of samples in a node

# create workflow
rf_wf <- workflow() %>%
  add_model(rf_mod)

# define hyperparameter values to vary over 
mtry <- c(1, 2, 3)
grid <- expand_grid(mtry = mtry)

# create cross-validation folds
folds <- group_vfold_cv(data = data, 
                        group = region, #split training/testing data by stock
                        v = v, #number of folds
                        balance = "groups" #one stock per fold
)

#define formula for modelling
rec <- recipe(pb~ ., data = data) %>%
  update_role(region, new_role = "ID") %>%
  step_downsample(pb)

#update workflow
rf_wf <- rf_wf %>%
  add_recipe(rec)

# enable parallelisation
plan(multisession, workers = cores)

#run models with tuning
tun <- tune_grid(rf_wf,
                 resamples = folds,
                 grid = grid,
                 metrics = sdm_metric_set(),
                 control = control_grid(verbose=F)) 

#get metric scores for each tuning value
metrics <- collect_metrics(tun, summarize = F)

#extract best model
best <- show_best(tun, metric = "boyce_cont") %>%
  filter(n == v)

#set up model
best_mod <- rand_forest() %>%
  set_engine(engine = "ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  set_args(trees = 1000, mtry = best$mtry[1], min_n = 1)

#update workflow
best_wf <- rf_wf %>%
  update_model(best_mod)

#run best model on all data
best_fit <- best_wf %>%
  fit(data)              


#---------------------------------------------
# Get Model Info for Supplementary Material
#---------------------------------------------

# need the variable importance scores, partial dependence plot values and CBI scores

# CBI scores (from metrics)
metrics <- metrics %>%
  filter(.metric == "boyce_cont")

# identify which subarea was being tested in each resample 
for(i in 1:nrow(folds)){
  
  # get fold
  this_fold <- assessment(folds$splits[[i]])
  
  # extract region
  this_region <- unique(this_fold$region)
  
  # extract resample id
  this_resample <- folds$id[i]
  
  # create df
  df <- data.frame(id = this_resample, 
                   region = this_region)
  
  # join to other resamples
  if(i == 1){
    resample_subareas <- df
  } else {
    resample_subareas <- rbind(resample_subareas, df)
  }
}

# append subarea info to metrics
metrics <- metrics %>% left_join(resample_subareas)

# only keep relevant columns
metrics <- metrics %>%
  dplyr::select(region, mtry, .estimate)

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(mtry == best$mtry[1])

# export
saveRDS(metrics, 
        paste0("output/random forests/cbi_scores.rds"))


# Variable Importance Scores
vi_scores <- vi(best_fit)

# export
saveRDS(vi_scores, 
        paste0("output/random forests/varimp_scores.rds"))


# Partial Dependence Plot Data

#get explainer
explainer <- explain_tidymodels(model = best_fit, 
                                data = dplyr::select(data, -pb),
                                y = as.integer(data$pb),
                                verbose = T)

#compute partial dependence
pdps <- model_profile(explainer, 
                      variables = names(data)[!names(data) %in% c("pb", "subarea")],
                      N = 500)

#extract pdp predictive values
pdp_ovr <- as_tibble(pdps$agr_profiles) %>%
  rename(x = `_x_`, yhat = `_yhat_`, var = `_vname_`) %>%
  dplyr::select(var, x, yhat) %>%
  mutate(yhat = 1-yhat)

# export PDP values
saveRDS(pdp_ovr, 
        paste0("output/random forests/pdp_values.rds"))

# remove large DALEXtra objects
rm(pdps, pdp_ovr, explainer)


#---------------------------------------------
# Export the model
#---------------------------------------------

saveRDS(best_fit, 
        paste0("output/random forests/rf_model.rds"))
