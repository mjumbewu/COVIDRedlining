if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, xgboost, caret, tidymodels, vip)

# Using the combined file, run a random forest model to predict the covid_cases
# based on the SVI scores (RPL_THEMES) and the HOLC grade.
#
input_df <- read_sf("jamaal/data/covid_redling_combined_files/baltimore_nyc_chicago_combined.gpkg")
input_df <- input_df %>%
  filter(zcta != "60666") %>%
  filter(!is.na(holc_grade)) %>%
  filter(!is.na(RPL_THEMES))

# Add a column representing high cases, where "high" is defined as being in the
# top quartile.
#


input_df <- input_df %>%
  mutate(high_cases = as.factor(ifelse(cases_per_1000 > quantile(cases_per_1000, 0.5), 1, 0)), 
         case_quantiles = ntile(x = cases_per_1000, n = 5), 
         high_quant = as.factor(if_else(case_quantiles == 5, 1, 0)), 
         nyc = if_else(city == "NYC", 1, 0))

# We'll use the, SVI score, the HOLC grade, and maybe the city as predictors.
# Create a version of the dataframe that can be used for learning (no
# geometry).
modeling_df = data.frame(high_cases = input_df$high_cases,
                         RPL_THEMES = input_df$RPL_THEMES,
                         holc_grade = as.factor(input_df$holc_grade),
                         high_quant = input_df$high_quant, 
                         nyc = as.factor(input_df$nyc))


#tidymodels median------------------
# Use an 80/20 split for training and testing.
#
set.seed(48744)
# splitter <- createDataPartition(modeling_df$high_cases, p = 0.8, list = FALSE)
# train_df <- modeling_df[splitter, ]
# test_df <- modeling_df[-splitter, ]

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
  set_mode("classification")

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

xgb_wf <- workflow() %>% 
  add_formula(high_cases ~ RPL_THEMES + holc_grade) %>% 
  add_model(xgb_spec)

#create CV folds for the resampling tuning

xg_folds <- vfold_cv(xg_train, 10)


xgb_res <- tune_grid(
  xgb_wf,
  resamples = xg_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

#grab best xgboost for RMSE and Rsquare



best_auc <- select_best(xgb_res, "roc_auc")

final_auc_xgb <- finalize_workflow(xgb_wf, best_auc)

#finalizing workflows

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc)

#pulling VIP values

xg_vip <- final_xgb %>% 
  fit(data = xg_train) %>% 
  extract_fit_parsnip() %>% 
  vip() 

xg_vip + theme_minimal()

#final overall results

final_xgboost_res <- last_fit(final_xgb, xg_split)

collect_metrics(final_xgboost_res)

#xgboost top half with nyc dummy-------------------

xgb_nyc_wf <- workflow() %>% 
  add_formula(high_cases ~ RPL_THEMES + holc_grade + nyc) %>% 
  add_model(xgb_spec)

xgb_nyc_res <- tune_grid(
  xgb_nyc_wf,
  resamples = xg_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

#grab best xgboost for auc_roc for NYC

best_auc <- select_best(xgb_nyc_res, "roc_auc")

final_nyc_auc_xgb <- finalize_workflow(xgb_nyc_wf, best_auc)

#pulling VIP values

xg_nyc_vip <- final_nyc_auc_xgb %>% 
  fit(data = xg_train) %>% 
  extract_fit_parsnip() %>% 
  vip() 

xg_nyc_vip + theme_minimal()

#final overall NYC results

final_nyc_xgboost_res <- last_fit(final_nyc_auc_xgb, xg_split)

collect_metrics(final_nyc_xgboost_res)



#tidymodels upper quantile---------------------



xgb_wf_quant <- workflow() %>% 
  add_formula(high_quant ~ RPL_THEMES + holc_grade) %>% 
  add_model(xgb_spec)

#create CV folds for the resampling tuning

xg_folds <- vfold_cv(xg_train, 10)


xgb_quant_res <- tune_grid(
  xgb_wf_quant,
  resamples = xg_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

#grab best xgboost for roc_auc


best_auc <- select_best(xgb_quant_res, "roc_auc")

#finalizing workflows

final_auc_xgb <- finalize_workflow(
  xgb_wf_quant,
  best_auc)

#pulling VIP values

xg_vip <- final_auc_xgb %>% 
  fit(data = xg_train) %>% 
  extract_fit_parsnip() %>% 
  vip() 

xg_vip + theme_minimal()

#final overall results

final_xgboost_res <- last_fit(final_auc_xgb, xg_split)

collect_metrics(final_xgboost_res)


#xgboost top quantile with nyc dummy-------------------

xgb_nyc_wf <- workflow() %>% 
  add_formula(high_quant ~ RPL_THEMES + holc_grade + nyc) %>% 
  add_model(xgb_spec)

xgb_nyc_res <- tune_grid(
  xgb_nyc_wf,
  resamples = xg_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

#grab best xgboost for auc_roc for NYC

best_auc <- select_best(xgb_nyc_res, "roc_auc")

final_nyc_auc_xgb <- finalize_workflow(xgb_nyc_wf, best_auc)

#pulling VIP values

xg_nyc_vip <- final_nyc_auc_xgb %>% 
  fit(data = xg_train) %>% 
  extract_fit_parsnip() %>% 
  vip() 

xg_nyc_vip + theme_minimal()

#final overall NYC results

final_nyc_xgboost_res <- last_fit(final_nyc_auc_xgb, xg_split)

collect_metrics(final_nyc_xgboost_res)

#Mjumbe's xgboost specifications---------------------------

# Train the model. This is a classification problem, so we'll try the xgboost package.
#
fit <- train(high_cases ~ .,
             data = train_df,
             method = "xgbTree",
             ntree = 1000,
             importance = TRUE,
             verbosity = 0)

# Predict the high_cases for the test set.
#
pred <- predict(fit, test_df)

# Get the confusion matrix.
#
confusionMatrix(pred, test_df$high_cases)

#
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  0  1
#          0  9 12
#          1 16 13
#
#                Accuracy : 0.44
#                  95% CI : (0.2999, 0.5875)
#     No Information Rate : 0.5
#     P-Value [Acc > NIR] : 0.8389
#
#                   Kappa : -0.12
#
#  Mcnemar's Test P-Value : 0.5708
#
#             Sensitivity : 0.3600
#             Specificity : 0.5200
#          Pos Pred Value : 0.4286
#          Neg Pred Value : 0.4483
#              Prevalence : 0.5000
#          Detection Rate : 0.1800
#    Detection Prevalence : 0.4200
#       Balanced Accuracy : 0.4400
#
#        'Positive' Class : 0

# Plot the variable importance for the random forest model.
#
var_imp <- varImp(fit, conditional = TRUE)
imp_plot <- plot(var_imp, main = "XGBoost Variable Importance")

# Plot the ROC curve.
#
roc_plot <- plot.roc(test_df$high_cases, pred, main = "ROC Curve")
