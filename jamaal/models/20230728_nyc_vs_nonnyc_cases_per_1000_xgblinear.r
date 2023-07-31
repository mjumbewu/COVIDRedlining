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

# We'll use the, SVI score, the HOLC grade, and maybe the city as predictors.
# Create a version of the dataframe that can be used for learning (no
# geometry).
# modeling_df = data.frame(cases_per_1000 = input_df$cases_per_1000,
#                          RPL_THEMES = input_df$RPL_THEMES,
#                          holc_grade = as.factor(input_df$holc_grade),
#                          city = as.factor(input_df$city))
modeling_df = data.frame(cases_per_1000 = input_df$cases_per_1000,
                         RPL_THEMES = input_df$RPL_THEMES,
                         holc_grade = as.factor(input_df$holc_grade),
                         is_nyc = as.factor(ifelse(input_df$city == "NYC", 1, 0)))

# Use an 80/20 split for training and testing.
#
set.seed(48744)
splitter <- createDataPartition(modeling_df$cases_per_1000, p = 0.8, list = FALSE)
train_df <- modeling_df[splitter, ]
test_df <- modeling_df[-splitter, ]

# Train the model. This is a regression problem, so we'll use the randomForest package.
#
fit <- train(cases_per_1000 ~ .,
                data = train_df,
                method = "xgbLinear")

# Predict the cases_per_1000 for the test set.
#
pred <- predict(fit, test_df)

# Calculate the RMSE.
#
rmse <- sqrt(mean((pred - test_df$cases_per_1000)^2))
rmse

# Calculate the correlation between the predicted and actual values.
#
cor <- cor(pred, test_df$cases_per_1000)
cor

# Calculate the R-squared value.
#
rsq <- 1 - sum((pred - test_df$cases_per_1000)^2) / sum((test_df$cases_per_1000 - mean(test_df$cases_per_1000))^2)
rsq

# Calculate the R-squared value for NYC and non-NYC observations separately.
#
nyc_test_df <- test_df %>%
  filter(is_nyc == 1)
nonnyc_test_df <- test_df %>%
  filter(is_nyc == 0)

nyc_pred <- pred[which(test_df$is_nyc == 1)]
nonnyc_pred <- pred[which(test_df$is_nyc == 0)]

nyc_rsq <- 1 - sum((nyc_pred - nyc_test_df$cases_per_1000)^2) / sum((nyc_test_df$cases_per_1000 - mean(nyc_test_df$cases_per_1000))^2)
nyc_rsq
nonnyc_rsq <- 1 - sum((nonnyc_pred - nonnyc_test_df$cases_per_1000)^2) / sum((nonnyc_test_df$cases_per_1000 - mean(nonnyc_test_df$cases_per_1000))^2)
nonnyc_rsq

# Calculate the MAE and MAPE
#
mae <- mean(abs(pred - test_df$cases_per_1000))
mae
mape <- mean(abs((pred - test_df$cases_per_1000)/test_df$cases_per_1000))
mape

# Plot the predicted vs. actual values for the model.
#
png(file="jamaal/data/nyc_v_nonnyc_cases_per_1000_xgb.png",
    width = 10, height = 10, res = 600, units = "in")
pred_v_actual_plot <- plot(pred, test_df$cases_per_1000,
                           main = "XGBoost Predicted vs. Actual, NYC (red) and non-NYC (black)",
                           xlab = "Predicted",
                           xlim = c(min(pred, test_df$cases_per_1000), max(pred, test_df$cases_per_1000)),
                           ylim = c(min(pred, test_df$cases_per_1000), max(pred, test_df$cases_per_1000)),
                           ylab = "Actual",
                           col = test_df$is_nyc)
dev.off()

# Plot the variable importance for the random forest model.
#
var_imp <- varImp(fit, conditional = TRUE)
var_imp
imp_plot <- plot(var_imp, main = "Variable Importance")
imp_plot
