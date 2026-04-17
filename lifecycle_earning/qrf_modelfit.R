library(haven)
library(tidyverse)
library(arrow)
library(e1071)
library(ipumsr)
library(readr)
library(purrr)
library(survey)
library(forecast)
library(grf)

# install.packages("grf")

# Load the data for QRF Model
psid_hdinc_model <- readRDS("psid_hdinc_model_v2.RDS")

# The Quantile Random Forest Model
set.seed(12351)

# What the QRF Model is doing:
# It is generating the conditional distribution of a person's income in t+1 year, based on:
# (1) the person's income in t year; (2) age and age square; (3) sex

# Notes:
# In the original version, it is:
# income for t+1 year ~ income for t year + age + age_sq + sex + age:sex + cohort_5yr
# age:sex: an interaction term between age and sex;
# cohort_5yr" the 5-year cohort information
# However, we decided to:
# Drop 5 year cohort: it is not application in the micro simulation data sample
# Drop age:sex: because of the variabe importance test


# split by person_id
person_ids <- unique(psid_hdinc_model$person_id)

train_ids <- sample(
  person_ids,
  size = floor(0.8 * length(person_ids)),
  replace = FALSE
)

df_train <- psid_hdinc_model %>%
  filter(person_id %in% train_ids)

df_test <- psid_hdinc_model %>%
  filter(!person_id %in% train_ids)

# model matrix
x_formula <- ~ age + age_sq + sex + income_real_a

X_train <- model.matrix(x_formula, data = df_train)[, -1]
Y_train <- df_train$income_real_next

X_test <- model.matrix(x_formula, data = df_test)[, -1]
Y_test <- df_test$income_real_next

# fit the QRF model
qrf_model <- quantile_forest(
  X_train,
  Y_train,
  num.trees = 500,
  seed = 12315
)

# predict & test
pred_mean <- predict(qrf_model, X_test)$predictions
pred_mean_train <- predict(qrf_model, X_train)$predictions

pred_q <- predict(
  qrf_model,
  X_test,
  quantiles = seq(0.05, 1, by = c(0.01))
)$predictions


# Evaluate the predictions
mae <- mean(abs(pred_mean - Y_test))
rmse <- sqrt(mean((pred_mean - Y_test)^2))
corr <- cor(pred_mean, Y_test)

print(mae)
print(rmse)
print(corr)

quantile(Y_train, c(.5, .9, .95, .99))
quantile(pred_mean_train, c(.5, .9, .95, .99))

quantile(Y_test, c(.5, .9, .95, .99))
quantile(pred_mean, c(.5, .9, .95, .99))

# Check the variable importance
var_imp <- variable_importance(qrf_model)

importance_df <- data.frame(
  Variable = colnames(X_train),
  Importance = var_imp
) %>%
  arrange(desc(Importance))
importance_df

# save the rds model
saveRDS(qrf_model, "qrf_model3.rds")