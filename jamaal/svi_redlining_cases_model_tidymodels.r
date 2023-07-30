############################################################################
############################################################################
###                                                                      ###
###                      AIM-AHEAD POSTER MODELS V2                      ###
###                                                                      ###
############################################################################
############################################################################

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, randomForest, caret, xgboost, tidymodels, vip)

doParallel::registerDoParallel()


# Using the combined file, run a random forest model to predict the covid_cases
# based on the SVI scores (RPL_THEMES) and the HOLC grade.
#
input_df <- read_sf("jamaal/data/covid_redling_combined_files/baltimore_nyc_chicago_combined.gpkg")
input_df <- input_df %>%
  filter(zcta != "60666") %>%
  filter(!is.na(holc_grade)) %>%
  filter(!is.na(RPL_THEMES))

input_df <- input_df %>% 
  mutate(holc_a = if_else(holc_grade == "A", 1,  0), 
         holc_b = if_else(holc_grade == "B",  1, 0),
         holc_c = if_else(holc_grade == "C",  1, 0),
         holc_d = if_else(holc_grade == "D",  1, 0)) 
         
# W use the, SVI score, the HOLC grade, and maybe the city as predictors.
# Create a version of the dataframe that can be used for learning (no
# geometry).
# modeling_df = data.frame(cases_per_1000 = input_df$cases_per_1000,
#                          RPL_THEMES = input_df$RPL_THEMES,
#                          holc_grade = as.factor(input_df$holc_grade),
#                          city = as.factor(input_df$city))

# modeling_df = data.frame(cases_per_1000 = input_df$cases_per_1000,
#                          RPL_THEMES = input_df$RPL_THEMES,
#                          holc_grade = as.factor(input_df$holc_grade))

modeling_df <- input_df %>% 
  select(cases_per_1000, RPL_THEMES, holc_grade, 
         holc_a, holc_b, holc_c, holc_d) %>% 
  mutate(holc_grade = as.factor(holc_grade))

##################################################################
##                   Random Forest Tidymodels                   ##
##################################################################


# Use an 80/20 split for training and testing.
#
set.seed(48744)

rf_split <- initial_split(data = modeling_df, prop = .8)
rf_training <- training(rf_split)
rf_testing <- testing(rf_split)

rf_spec <- rand_forest(trees = 1000, 
                       mtry = tune(), 
                       min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")


rf_grid <- grid_latin_hypercube(
  min_n(),
  finalize(mtry(), rf_training))


# rf_wf <- workflow() %>% 
#   add_formula(cases_per_1000 ~ RPL_THEMES + holc_grade) %>% 
#   add_model(rf_spec)


rf_wf <- workflow() %>% 
  add_formula(cases_per_1000 ~ RPL_THEMES + holc_a + holc_b + holc_c + holc_d) %>% 
  add_model(rf_spec)

#create CV folds for the resampling tuning

rf_folds <- vfold_cv(rf_training, 10)

rf_res <- tune_grid(
  rf_wf,
  resamples = rf_folds,
  grid = rf_grid,
  control = control_grid(save_pred = TRUE)
)

#grab best xgboost for RMSE and Rsquare

rf_best_rmse <- select_best(rf_res, "rmse")
rf_best_rsq <- select_best(rf_res, "rsq")

final_rmse_rf <- finalize_workflow(rf_wf, rf_best_rmse)


#random forest final overall results

final_rf_res <- last_fit(final_rmse_rf, rf_split)

collect_metrics(final_rf_res)

#pulling VIP values

rf_vip <- final_rf_res %>% 
  extract_fit_parsnip() %>% 
  vip() 

rf_vip + theme_minimal()

#final rf overall results

final_rf_res <- last_fit(final_rmse_rf, rf_split)

collect_metrics(final_rf_res)

##################################################################
##                      xgBoost Tidymodels                      ##
##################################################################

#80/20 test split
xg_split <- initial_split(modeling_df, .8)

xg_train <- training(xg_split)
xg_test <- testing(xg_split)

xgb_spec <- boost_tree(trees = 1000, 
                       tree_depth = tune(),
                       min_n = tune(), 
                       loss_reduction = tune(), 
                       sample_size = tune(), 
                       mtry = tune(), 
                       learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

#set hyperparameters grid search

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), xg_train),
  learn_rate(),
  size = 30
)

xgb_qf <- workflow() %>% 
  add_formula(cases_per_1000 ~ RPL_THEMES + holc_grade) %>% 
  add_model(xgb_spec)

#create CV folds for the resampling tuning

xg_folds <- vfold_cv(xg_train, 10)


xgb_res <- tune_grid(
  xgb_qf,
  resamples = xg_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

#grab best xgboost for RMSE and Rsquare

best_rmse <- select_best(xgb_res, "rmse")
best_rsq <- select_best(xgb_res, "rsq")

final_rmse_xgb <- finalize_workflow(xgb_qf, best_rmse)

#pulling VIP values

xg_vip <- final_rmse_xgb %>% 
  fit(data = xg_train) %>% 
  extract_fit_parsnip() %>% 
  vip() 

xg_vip + theme_minimal()


#ginal overall results

final_xgboost_res <- last_fit(final_rmse_xgb, xg_split)

collect_metrics(final_xgboost_res)

