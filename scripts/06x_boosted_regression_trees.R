# boosted regression trees script to run with 06_run_all_models

rm(list=setdiff(ls(), c("cores", "master_data")))

# convert pb to ordered factor
data <- master_data %>% mutate(pb = as.factor(pb))
data$pb <- ordered(data$pb, levels = c("presence", "background"))

# set number of folds to number of breeding stocks
v <- length(unique(data$region))

#define BRT
brt_mod <- boost_tree() %>%
  set_mode("classification") %>%
  set_engine("lightgbm", num_threads = 1 #use lightgbm package
  ) %>%
  set_args(trees = tune(),
           tree_depth = tune(), 
           learn_rate = tune(), 
           min_n = 20) 

#create workflow
brt_wf <- workflow() %>%
  add_model(brt_mod)

#define hyperparameter values to vary over 
learn.rate <- c(0.005, 0.01, 0.5)
tree.depth <- c(1, 3, 5)
trees <- c(200, 500, 1000, 2000, 5000)
grid <- expand_grid(learn_rate = learn.rate, tree_depth = tree.depth, trees = trees)

#create cross-validation folds
folds <- group_vfold_cv(data = data, 
                        group = region, #split training/testing data by region
                        v = v, #number of folds
                        balance = "groups" #one subarea per fold
)

#define formula for modelling
rec <- recipe(pb~ ., data = data) %>%
  update_role(region, new_role = "ID") %>%
  step_downsample(pb)

#update workflow
brt_wf <- brt_wf %>%
  add_recipe(rec)

# enable parallelisation
#plan(multisession, workers = cores)
plan(sequential)

#run models with tuning
tun <- tune_grid(brt_wf,
                 resamples = folds,
                 grid = grid,
                 metrics = sdm_metric_set(),
                 control = control_grid(verbose=T)) 

#get metric scores for each tuning value
metrics <- collect_metrics(tun, summarize = F)

#extract best model
best <- show_best(tun, metric = "boyce_cont") %>%
  filter(n == v)

#set up model
best_mod <- boost_tree() %>%
  set_engine(engine = "lightgbm") %>%
  set_mode("classification") %>%
  set_args(min_n = 20, trees = best$trees[1], 
           tree_depth = best$tree_depth[1], learn_rate = best$learn_rate[1])

#update workflow
best_wf <- brt_wf %>%
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

# only keep relevant columns
metrics <- metrics %>%
  dplyr::select(region, trees, tree_depth, learn_rate, .estimate)

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(trees == best$trees[1],
         tree_depth == best$tree_depth[1],
         learn_rate == best$learn_rate[1])

# export
saveRDS(metrics, 
        paste0("output/boosted regression trees/cbi_scores.rds"))


# Variable Importance Scores
vi_scores <- vi(best_fit)

# export
saveRDS(vi_scores, 
        paste0("output/boosted regression trees/varimp_scores.rds"))

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
        paste0("output/boosted regression trees/pdp_values.rds"))

# remove large DALEXtra objects
rm(pdps, pdp_ovr, explainer)


#---------------------------------------------
# Export the model
#---------------------------------------------

saveRDS(best_fit, 
        paste0("output/boosted regression trees/brt_model.rds"))
