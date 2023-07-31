if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, randomForest, caret, xgboost, tidymodels, vip)

# Using the combined file, run a random forest model to predict the covid_cases
# based on the SVI scores (RPL_THEMES) and the HOLC grade.
#
input_df <- read_sf("jamaal/data/covid_redling_combined_files/baltimore_nyc_chicago_combined.gpkg")
input_df <- input_df %>%
  filter(zcta != "60666") %>%
  filter(!is.na(holc_grade)) %>%
  filter(!is.na(RPL_THEMES))

# We'll use the, SVI score, the HOLC grade, and maybe the city as predictors.
# Create a version of the dataframe that can be used for learning (no
# geometry).
# modeling_df = data.frame(cases_per_1000 = input_df$cases_per_1000,
#                          RPL_THEMES = input_df$RPL_THEMES,
#                          holc_grade = as.factor(input_df$holc_grade),
#                          city = as.factor(input_df$city))
modeling_df = data.frame(cases_per_1000 = input_df$cases_per_1000,
                         RPL_THEMES = input_df$RPL_THEMES,
                         holc_grade = as.factor(input_df$holc_grade))

# Use an 80/20 split for training and testing.
#
set.seed(48744)
splitter <- createDataPartition(modeling_df$cases_per_1000, p = 0.8, list = FALSE)
train_df <- modeling_df[splitter, ]
test_df <- modeling_df[-splitter, ]

# Train the model. This is a regression problem, so we'll use the randomForest package.
#
rf_fit <- train(cases_per_1000 ~ .,
                data = train_df,
                method = "xgbTree",
                ntree = 1000,
                importance = TRUE)

# Predict the cases_per_1000 for the test set.
#
rf_pred <- predict(rf_fit, test_df)

# Calculate the RMSE.
#
rf_rmse <- sqrt(mean((rf_pred - test_df$cases_per_1000)^2))
rf_rmse

# Calculate the correlation between the predicted and actual values.
#
rf_cor <- cor(rf_pred, test_df$cases_per_1000)
rf_cor

# Calculate the R-squared value.
#
rf_rsq <- 1 - sum((rf_pred - test_df$cases_per_1000)^2) / sum((test_df$cases_per_1000 - mean(test_df$cases_per_1000))^2)
rf_rsq

# Calculate the MAE and MAPE
#
rf_mae <- mean(abs(rf_pred - test_df$cases_per_1000))
rf_mae
rf_mape <- mean(abs((rf_pred - test_df$cases_per_1000)/test_df$cases_per_1000))
rf_mape

# Plot the predicted vs. actual values for the model.
#
rf_plot <- plot(rf_pred, test_df$cases_per_1000, main = "Random Forest Predicted vs. Actual", xlab = "Predicted", ylab = "Actual")
rf_plot

# Plot the variable importance for the random forest model.
#
rf_var_imp <- varImp(rf_fit, conditional = TRUE)
rf_var_imp
rf_imp_plot <- plot(rf_var_imp, main = "Random Forest Variable Importance")


#################################################################
##                    XGBOOST On Case Rates                    ##
#################################################################

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

doParallel::registerDoParallel()


xgb_res <- tune_grid(
  xgb_qf,
  resamples = xg_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)


# xgb_res %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   select(mean, mtry:sample_size) %>%
#   pivot_longer(mtry:sample_size,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "RMSE")

#grab best xgboost for RMSE and Rsquare

best_rmse <- select_best(xgb_res, "rmse")
best_rsq <- select_best(xgb_res, "rsq")

final_rmse_xgb <- finalize_workflow(xgb_qf, best_rmse)

#pulling VIP values

asdf <- final_rmse_xgb %>%
  fit(data = xg_train) %>%
  extract_fit_parsnip() %>%
  vip()

asdf + theme_minimal()


asdf <- final_rmse_xgb %>%
  fit(data = xg_train) %>%
  extract_fit_parsnip()

#ginal overall results

final_xgboost_res <- last_fit(final_rmse_xgb, xg_split)

collect_metrics(final_xgboost_res)
