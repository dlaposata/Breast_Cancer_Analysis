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
  labs(x = "Concavity Mean", y = "Radius Mean", title = "Severity of Concavity vs. Radius of Tumor")

train[,1:11] |>
  ggpairs()

train[,c(1,12:21)] |>
  ggpairs()

train[,c(1,22:31)] |>
  ggpairs()

train[,c(1,2,12,22)] |>
  ggpairs() -> measure_pairs
