library(tidyverse)
library(GGally)
library(ggplot2)


#setwd("/Users/dlaposata/Documents/GitHub/project-7")
data <- read.csv("breast_cancer_diagnostic_data.csv")
train <- read.csv("training.csv")
test <- read.csv("test.csv")

dataE <- data
corr_df <- dataE[,c('area_worst', 'area_mean', 'concave.points_mean', 'radius_worst')]
#head(corr_df)
ggpairs(corr_df)

train |>
  select(-id) -> train

test |>
  select(-id) -> test

train$diagnosis <- as.factor(train$diagnosis)
test$diagnosis <- as.factor(test$diagnosis)

train[,c(1,2,12,22)] |>
  ggpairs()

ggplot(train) +
  geom_point(aes(x = concavity_mean, y = radius_mean, col = diagnosis))+
  labs(x = "Concavity Mean", y = "Radius Mean", title = "Severity of Concavity vs. Radius of Tumor")