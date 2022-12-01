### Packages

library(tidyverse)
library(GGally)
library(ggplot2)
library(tidyr)
library(tidymodels)
library(vip)
library(caTools)
library(xgboost)


### Data Acquiring and Cleaning

setwd("/Users/dlaposata/Downloads")
cancer <- read.csv("data.csv")
View(cancer)

cancer |>
  select(-X) -> cancer

cancer$diagnosis <- as.factor(cancer$diagnosis)

### Split into Training

sampler <- sample.split(cancer, .6)
train <- cancer[sampler == T,]
test <- cancer[sampler == F,]

### EDA Plots

ggplot(cancer) +
  geom_point(aes(radius_mean, concavity_mean)) +
  geom_boxplot(aes(radius_mean, concavity_mean))

cancer |>
  select(concavity_mean, concavity_se, concavity_worst) |>
  ggpairs()


### xGBoost Model
boost_spec <- boost_tree(trees = 5000, tree_depth = 2, learn_rate = 0.01) |>
  set_engine("xgboost") |>
  set_mode("classification")

boost_fit <- boost_spec |>
  fit(diagnosis ~ ., data = train[,-1])

boost_fit |>
  augment(new_data = test) |>
  accuracy(truth = diagnosis, estimate = .pred_class)

boost_fit |>
  augment(new_data = test) |>
  conf_mat(truth = diagnosis, estimate = .pred_class)

vip(boost_fit)

### KNN Models

knn5_spec <- nearest_neighbor(mode = "classification", neighbors = 5)

knn5_spec |>
  fit(diagnosis ~ ., data = train) -> knn5.fit

knn5.fit |>
  augment(new_data = test) -> knn5.test_res

knn5.test_res |>
  conf_mat(truth = diagnosis, estimate = .pred_class)

knn5.test_res |>
  accuracy(truth = diagnosis, estimate = .pred_class) |>
  mutate(error = 1 - .estimate) |>
  pull(error)

### Random Forest Models

rf_spec <- rand_forest(mtry = sqrt(.cols())) |>
  set_engine("randomForest", importance = TRUE) |>
  set_mode("classification")

rf_fit <- rf_spec |>
  fit(diagnosis ~ ., data = train)

vip(rf_fit) ## still Price and Shelf location are the most important

rf_fit |>
  augment(new_data = test) |>
  conf_mat(truth = diagnosis, estimate = .pred_class)

rf_fit |>
  augment(new_data = test) |>
  accuracy(truth = diagnosis, estimate = .pred_class)
