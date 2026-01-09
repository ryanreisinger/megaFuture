# bayesian additive regression trees script to run with 06_run_all_models

rm(list=setdiff(ls(), c("cores", "master_data")))

# convert pb to ordered factor
data <- master_data %>% mutate(pb = as.factor(pb))
data$pb <- ordered(data$pb, levels = c("presence", "background"))

# convert region to numeric
data$region <- as.numeric(as.factor(data$region))

# set number of folds to number of breeding stocks
v <- length(unique(data$region))

#define BART
bart_mod <- parsnip::bart() %>%
  set_mode("classification") %>%
  set_engine("dbarts") %>%
  set_args(trees = tune()) #tune trees

#create workflow
bart_wf <- workflow() %>%
  add_model(bart_mod)

#define tree values to vary over 
trees <- c(50, 100, 200, 300)
grid <- expand_grid(trees = trees)

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
bart_wf <- bart_wf %>%
  add_recipe(rec)

# enable parallelisation
plan(multisession, workers = cores)

#run models with tuning
tun <- tune_grid(bart_wf,
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
best_mod <- parsnip::bart() %>%
  set_engine(engine = "dbarts") %>%
  set_mode("classification") %>%
  set_args(trees = best$trees[1])

#update workflow
best_wf <- bart_wf %>%
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
  dplyr::select(region, trees, .estimate)

# get corresponding regions to original names
data2 <- readRDS(paste0("output/varselection/model_data.rds"))
data2 <- data2 %>% rename(region_name = region)
data2$region <- as.numeric(as.factor(data2$region_name))
regions <- data2 %>% group_by(region) %>%
  summarise(region_name = unique(region_name)) 

# append these names
metrics <- metrics %>%
  left_join(regions) %>%
  dplyr::select(-region) %>%
  rename(region = region_name)

# only keep best hyperparameter settings
metrics <- metrics %>%
  filter(trees == best$trees[1])

# export
saveRDS(metrics, 
        paste0("output/bayesian additive regression trees/cbi_scores.rds"))

# Variable Importance Scores

# extract the underlying dbarts model
bart1 <- extract_fit_parsnip(best_fit)$fit

# get variable usage counts from posterior
var_counts <- bart1$varcount

# get mean for each variable
mean_vi <- colMeans(var_counts)

# create a data frame with variable names and their importance scores
vi_scores <- data.frame(Variable = names(mean_vi), 
                        Importance = mean_vi)

# subtract the minimum VI score
vi_scores$Importance <- vi_scores$Importance - min(vi_scores$Importance) + 5

# export
saveRDS(vi_scores, 
        paste0("output/bayesian additive regression trees/varimp_scores.rds"))

# Partial Dependence Plot Data

# compute partial dependence values
part <- pdbart(bart1, pl = F)

# define initial max and min val as 0
max_val <- 0
min_val <- 0

# for each variable
for(i in 1:length(part$xlbs)){
  
  # get the variable name
  var <- part$xlbs[i]
  
  # get the levels of this variable where points are logged
  levs <- part$levs[[i]]
  
  # get the partial dependence values
  vals <- part$fd[[i]]
  
  # get the mean partial dependence value for each level
  mean_vals <- colMeans(vals)
  
  # get the max and min values for this variable
  max_val_i <- max(vals)
  min_val_i <- min(vals)
  
  # if this is the biggest max val or smallest min val, update
  if(max_val_i > max_val){
    max_val <- max_val_i
  }
  if(min_val_i < min_val){
    min_val <- min_val_i
  }
  
  # join into a data frame
  pdp <- data.frame(var = var, 
                    x = levs, 
                    yhat = mean_vals)
  
  # join to all vars
  if(i == 1){
    pdps <- pdp
  } else {
    pdps <- rbind(pdps, pdp)
  }
}

# scale yhat by the min and max values
pdps <- pdps %>%
  mutate(yhat = (yhat - min_val) / (max_val - min_val)) %>%
  mutate(yhat = 1 - yhat)

# export PDP values
saveRDS(pdps, 
        paste0("output/bayesian additive regression trees/pdp_values.rds"))


#---------------------------------------------
# Export the model
#---------------------------------------------

# butcher and bundle to retain pointers
bart2 <- butcher::butcher(best_fit)
bart3 <- bundle::bundle(bart2)

saveRDS(bart3, 
        paste0("output/bayesian additive regression trees/bart_model.rds"))
