if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, xgboost, caret)

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
  mutate(high_cases = as.factor(ifelse(cases_per_1000 > quantile(cases_per_1000, 0.75), 1, 0)))

# We'll use the, SVI score, the HOLC grade, and maybe the city as predictors.
# Create a version of the dataframe that can be used for learning (no
# geometry).
modeling_df = data.frame(high_cases = input_df$high_cases,
                         RPL_THEMES = input_df$RPL_THEMES,
                         holc_grade = as.factor(input_df$holc_grade))

# Use an 80/20 split for training and testing.
#
set.seed(48744)
splitter <- createDataPartition(modeling_df$high_cases, p = 0.8, list = FALSE)
train_df <- modeling_df[splitter, ]
test_df <- modeling_df[-splitter, ]

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
#          0 37 12
#          1  0  0
#
#                Accuracy : 0.7551
#                  95% CI : (0.6113, 0.8666)
#     No Information Rate : 0.7551
#     P-Value [Acc > NIR] : 0.576719
#
#                   Kappa : 0
#
#  Mcnemar's Test P-Value : 0.001496
#
#             Sensitivity : 1.0000
#             Specificity : 0.0000
#          Pos Pred Value : 0.7551
#          Neg Pred Value :    NaN
#              Prevalence : 0.7551
#          Detection Rate : 0.7551
#    Detection Prevalence : 1.0000
#       Balanced Accuracy : 0.5000
#
#        'Positive' Class : 0

# Calculate the RMSE.
#
rmse <- sqrt(mean((pred - test_df$high_cases)^2))

# Calculate the correlation between the predicted and actual values.
#
rf_cor <- cor(pred, test_df$high_cases)

# Calculate the R-squared value.
#
rf_rsq <- 1 - sum((pred - test_df$high_cases)^2) / sum((test_df$high_cases - mean(test_df$high_cases))^2)

# Calculate the MAE and MAPE
#
rf_mae <- mean(abs(pred - test_df$high_cases))
rf_mape <- mean(abs((pred - test_df$high_cases)/test_df$high_cases))

# Plot the predicted vs. actual values for the model.
#
rf_plot <- plot(pred, test_df$high_cases, main = "Random Forest Predicted vs. Actual", xlab = "Predicted", ylab = "Actual")

# Plot the variable importance for the random forest model.
#
rf_var_imp <- varImp(fit, conditional = TRUE)
rf_imp_plot <- plot(rf_var_imp, main = "Random Forest Variable Importance")
