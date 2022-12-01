### Packages

library(tidyverse)
library(GGally)
library(ggplot2)
library(tidyr)
library(tidymodels)
library(vip)
library(caTools)
library(xgboost)
library(discrim)
library(glmnet)


### Gathering Data

setwd("/Users/dlaposata/Documents/GitHub/project-7")
train <- read.csv("training.csv")
test <- read.csv("test.csv")

train |>
  select(-id) -> train

test |>
  select(-id) -> test

train$diagnosis <- as.factor(train$diagnosis)
test$diagnosis <- as.factor(test$diagnosis)


### xGBoost Model
boost_spec <- boost_tree(trees = 5000, tree_depth = 2, learn_rate = 0.01) |>
  set_engine("xgboost") |>
  set_mode("classification")

boost_fit <- boost_spec |>
  fit(diagnosis ~ ., data = train)

boost_fit |>
  augment(new_data = test) |>
  accuracy(truth = diagnosis, estimate = .pred_class)

boost_fit |>
  augment(new_data = test) |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> boost.cm

autoplot(boost.cm, type = "heatmap")

vip(boost_fit)

### KNN 5 Model

knn5_spec <- nearest_neighbor(mode = "classification", neighbors = 5)

knn5_spec |>
  fit(diagnosis ~ ., data = train) -> knn5.fit

knn5.fit |>
  augment(new_data = test) -> knn5.test_res

knn5.test_res |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> knn5.cm

autoplot(knn5.cm, type = "heatmap")

knn5.test_res |>
  accuracy(truth = diagnosis, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

### Random Forest Model

rf_spec <- rand_forest(mtry = sqrt(.cols())) |>
  set_engine("randomForest", importance = TRUE) |>
  set_mode("classification")

rf_fit <- rf_spec |>
  fit(diagnosis ~ ., data = train)

vip(rf_fit)

rf_fit |>
  augment(new_data = test) |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> rf.cm

autoplot(rf.cm, type = "heatmap")

rf_fit |>
  augment(new_data = test) |>
  accuracy(truth = diagnosis, estimate = .pred_class)

### QDA Model

qda_spec <- discrim_quad()

qda_spec |>
  fit(diagnosis ~ ., data = train) -> qda.fit

qda.fit$fit$prior

qda.fit |>
  augment(new_data = test) -> qda.test_res

qda.test_res |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> qda.cm

autoplot(qda.cm, type = "heatmap")

qda.test_res |>
  accuracy(truth = diagnosis, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

### Lasso Classification Model

cancer_10foldcv <- vfold_cv(train, v = 10)

lambda <- lambda <- 10^seq(-2, 1, length.out = 10)
tune_df <- data.frame(lambda = lambda)

prep_data <- recipe(diagnosis ~ ., data = train) |>
  step_normalize(all_predictors())

lasso_spec <- logistic_reg(mixture = 1, penalty = tune("lambda")) |>
  set_mode("classification") |>
  set_engine("glmnet")

workflow() |>
  add_model(lasso_spec) |>
  add_recipe(prep_data) -> lasso_wf

lasso_wf |>
  tune_grid(resamples = cancer_10foldcv, grid = tune_df) -> lasso_tune

lasso_tune |>
  collect_metrics() |>
  select(lambda, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  ggplot() +
  geom_line(aes(lambda, 1 - accuracy)) +
  geom_point(aes(lambda, 1- accuracy)) +
  coord_trans(x = "log10")

show_best(lasso_tune, metric = "accuracy", n = 1)

lasso_final <- finalize_workflow(lasso_wf, select_best(lasso_tune, metric = "accuracy"))
lasso_final_fit <- fit(lasso_final, data = train)

View(lasso_final_fit  |>
  augment(new_data = test)) |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> lasso.cm

autoplot(lasso.cm, type = "heatmap")
