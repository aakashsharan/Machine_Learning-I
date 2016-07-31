library(randomForest)
library(caret)
library(mice)

train <- read.csv('train2016.csv', na.strings=c("NA","", " "))
test <- read.csv('test2016.csv', na.strings=c("NA","", " "))

train_new <- train
test_new <- test

train[, 8:108][train[, 8:108] == 'Yes' & train[, 8:108] != 'NA'] = 1
train[, 8:108][train[, 8:108] == 'No'] = -1

