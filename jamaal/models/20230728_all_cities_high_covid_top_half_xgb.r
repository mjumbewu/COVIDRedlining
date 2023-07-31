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
  mutate(high_cases = as.factor(ifelse(cases_per_1000 > quantile(cases_per_1000, 0.5), 1, 0)))

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
