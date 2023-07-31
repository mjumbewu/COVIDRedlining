if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf, randomForest, caret)

# Using the combined file, run a random forest model to predict the covid_cases
# based on the SVI scores (RPL_THEMES) and the HOLC grade.
#
input_df <- read_sf("jamaal/data/covid_redling_combined_files/baltimore_nyc_chicago_combined.gpkg")
input_df <- input_df %>%
  filter(zcta != "60666") %>%
  filter(!is.na(holc_grade)) %>%
  filter(!is.na(RPL_THEMES)) %>%
  filter(city != "NYC")

# We'll use the, SVI score, the HOLC grade, and maybe the city as predictors.
# Create a version of the dataframe that can be used for learning (no
# geometry).
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
                method = "rf",
                ntree = 1000,
                importance = TRUE)

# Predict the cases_per_1000 for the test set.
#
rf_pred <- predict(rf_fit, test_df)

# Calculate the RMSE.
#
rf_rmse <- sqrt(mean((rf_pred - test_df$cases_per_1000)^2))

# Calculate the correlation between the predicted and actual values.
#
rf_cor <- cor(rf_pred, test_df$cases_per_1000)

# Calculate the R-squared value.
#
rf_rsq <- 1 - sum((rf_pred - test_df$cases_per_1000)^2) / sum((test_df$cases_per_1000 - mean(test_df$cases_per_1000))^2)

# Calculate the MAE and MAPE
#
rf_mae <- mean(abs(rf_pred - test_df$cases_per_1000))
rf_mape <- mean(abs((rf_pred - test_df$cases_per_1000)/test_df$cases_per_1000))

# Plot the predicted vs. actual values for the model.
#
rf_plot <- plot(rf_pred, test_df$cases_per_1000, main = "Random Forest Predicted vs. Actual", xlab = "Predicted", ylab = "Actual")

# Plot the variable importance for the random forest model.
#
rf_var_imp <- varImp(rf_fit, conditional = TRUE)
rf_imp_plot <- plot(rf_var_imp, main = "Random Forest Variable Importance")
