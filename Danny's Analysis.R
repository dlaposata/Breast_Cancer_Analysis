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
library(kableExtra)

ggplot()

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

### EDA

train |>
  group_by(diagnosis) |>
  summarise(radius = mean(radius_mean),
            concavity = mean(concavity_mean),
            perimeter = mean(perimeter_mean)) |>
  kable(digits = 3) |>
  kable_material(lightable_options = "striped")

ggplot(train) +
  geom_point(aes(x = concavity_mean, y = radius_mean, col = diagnosis))+
  labs(x = "Concavity Mean", y = "Radius Mean", title = "Severity of Concavity vs. Radius of Tumor")

train[,1:11] |>
  ggpairs()

train[,c(1,12:21)] |>
  ggpairs()

train[,c(1,22:31)] |>
  ggpairs()

train[,c(1,2,12,22)] |>
  ggpairs()

### Logistic Classification

logistic_spec <- logistic_reg()

logistic_spec |>
  fit(diagnosis ~ ., data = train, family = "binomial") -> log.fit

log.fit |>
  pluck("fit") |>
  summary()

log.fit |>
  augment(new_data = test) |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> log.cm

autoplot(log.cm, type = "heatmap")

log.fit |>
  augment(new_data = test) |>
  accuracy(truth = diagnosis, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

vip(log.fit)

### KNN Model

cancer_10foldcv <- vfold_cv(train, v = 10)
neighbors_df <- data.frame(neighbors = c(1,3,5,7,10))

knn_spec <- the_model <- nearest_neighbor(neighbors = tune("neighbors")) %>%
  set_engine("kknn") %>% 
  set_mode("classification") 

knn_rec <- recipe(diagnosis ~ ., data = train)

workflow() |>
  add_model(knn_spec) |>
  add_recipe(knn_rec) -> knn_wf

knn_wf |>
  tune_grid(resamples = cancer_10foldcv, grid = neighbors_df) -> knn_tune

autoplot(knn_tune)

knn_final <- finalize_workflow(knn_wf, select_best(knn_tune, metric = "accuracy"))
knn_final_fit <- fit(knn_final, data = train)

knn_final_fit |>
  augment(new_data = test) -> knn_test_res
  
knn_test_res |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> knn.cm

autoplot(knn.cm, type = "heatmap")

knn.test_res |>
  accuracy(truth = diagnosis, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

### QDA Model

qda_spec <- discrim_quad()

qda_rec <- recipe(diagnosis ~ ., data = train)

workflow() |>
  add_model(qda_spec) |>
  add_recipe(qda_rec) -> qda.wf

qda.wf |>
  fit(data = train) -> qda.fit
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

autoplot(lasso_final_fit)

View(lasso_final_fit |>
  pull_workflow_fit() |>
  tidy())

lasso_final_fit  |>
  augment(new_data = test) |>
  conf_mat(truth = diagnosis, estimate = .pred_class) -> lasso.cm

autoplot(lasso.cm, type = "heatmap")

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

### SVM

df_cost <- grid_regular(cost(), levels = 10)

svm_linear_tune_spec <- svm_poly(degree = 1, cost = tune("cost")) %>%
     set_mode("classification") %>%
     set_engine("kernlab", scaled = FALSE)
                        
svm_linear_rec <- recipe(diagnosis ~ ., data = train)
                        
svm_linear_wf <- workflow() |>
     add_model(svm_linear_tune_spec) |>
     add_recipe(svm_linear_rec)
                        
tune_fit <- svm_linear_wf |>
    tune_grid(resamples = cancer_10foldcv, grid = df_cost)
                        
autoplot(tune_fit)
show_best(tune_fit, metric = "accuracy", n = 1)
                        
 svm_linear_final <- finalize_workflow(svm_linear_wf, select_best(tune_fit, metric = "accuracy"))
 svm_linear_final |>
     fit(data = train) -> svm_linear_final_fit
                        
svm_linear_final_fit |>
      extract_fit_engine() -> svm_linear_final_fit_engine
                        
svm_linear_final_fit |>
      augment(new_data = test) |>
      conf_mat(truth = diagnosis, estimate = .pred_class) -> svm.cm

autoplot(svm.cm, type = "heatmap")

svm_linear_final_fit |>
  augment(new_data = test) |>
      ggplot() +
      geom_tile(aes(radius_mean, concavity_mean, fill = .pred_class), alpha = .5) +
      geom_point(aes(radius_mean, concavity_mean, colour = diagnosis, shape = support_vector, size = support_vector), 
                                     data = train |> mutate(support_vector = 1:n() %in% svm_linear_final_fit_engine@alphaindex[[1]]))


