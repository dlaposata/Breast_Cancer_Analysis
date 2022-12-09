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

### Gathering Data

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
  labs(x = "Concavity Mean", y = "Radius Mean", title = "Severity of Concavity vs. Radius of Tumor") -> plot1

train[,1:11] |>
  ggpairs()

train[,c(1,12:21)] |>
  ggpairs()

train[,c(1,22:31)] |>
  ggpairs()

train[,c(1,2,12,22)] |>
  ggpairs() -> plot2

### Models

### SVC

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
  mutate(pred_lower_cutoff = factor(ifelse(.pred_M > 0.25, "M", "B"))) |>
  conf_mat(truth = diagnosis, estimate = pred_lower_cutoff) -> svm.cm

autoplot(svm.cm, type = "heatmap") -> plot3

svm_linear_final_fit |>
  augment(new_data = test) |>
  mutate(pred_lower_cutoff = factor(ifelse(.pred_M > 0.25, "M", "B"))) |>
  accuracy(truth = diagnosis, estimate = pred_lower_cutoff) |>
  mutate(error = 1 - .estimate) |>
  pull(error) -> svm.err

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
  mutate(pred_lower_cutoff = factor(ifelse(.pred_M > 0.25, "M", "B"))) |>
  conf_mat(truth = diagnosis, estimate = pred_lower_cutoff) -> knn.cm

autoplot(knn.cm, type = "heatmap") -> plot4

knn_test_res |>
  mutate(pred_lower_cutoff = factor(ifelse(.pred_M > 0.25, "M", "B"))) |>
  accuracy(truth = diagnosis, estimate = pred_lower_cutoff) |>
  mutate(error = 1 - .estimate) |>
  pull(error) -> knn.err

### RF Model

rf_spec <- rand_forest(mtry = sqrt(.cols())) |>
  set_engine("randomForest", importance = TRUE) |>
  set_mode("classification")

rf_fit <- rf_spec |>
  fit(diagnosis ~ ., data = train)

vip(rf_fit)

rf_fit |>
  augment(new_data = test) |>
  mutate(pred_lower_cutoff = factor(ifelse(.pred_M > 0.25, "M", "B"))) |>
  conf_mat(truth = diagnosis, estimate = pred_lower_cutoff) -> rf.cm

rf.cm$table
autoplot(rf.cm, type = "heatmap") -> plot5

rf_fit |>
  augment(new_data = test) |>
  mutate(pred_lower_cutoff = factor(ifelse(.pred_M > 0.25, "M", "B"))) |>
  accuracy(truth = diagnosis, estimate = pred_lower_cutoff) |>
  mutate(error = 1 - .estimate) |>
  pull(error) -> rf.err

### Further Assessment

round(data.frame("SVC" = svm.err, "KNN" = knn.err, "RF" = rf.err),4)
