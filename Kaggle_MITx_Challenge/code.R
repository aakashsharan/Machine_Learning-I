library(randomForest)

set.seed(121)

train <- read.csv('train2016.csv')
test <- read.csv('test2016.csv')

train_2 <- train
test_2 <- test

train_2$YOB[is.na(train_2$YOB)] <- median(train_2$YOB, na.rm = TRUE)
test_2$YOB[is.na(test_2$YOB)] <- median(test_2$YOB, na.rm = TRUE)


rf <- randomForest(Party ~. -USER_ID, data = train_2, mtry = 11, ntree = 500, importance = TRUE)
predict_rf <- predict(rf, test_2, type = 'response')

submission <- data.frame(USER_ID = test_2$USER_ID)
submission$Predictions <- predict(rf, test_2)
write.csv(submission, file='v1_show_of_hands.csv', row.names = FALSE)



rf_2 <- randomForest(Party ~. -USER_ID, data = train_2, ntree = 500, importance = TRUE)
predict_rf <- predict(rf_2, test_2, type = 'response')

submission2 <- data.frame(USER_ID = test_2$USER_ID)
submission2$Predictions <- predict(rf_2, test_2)
write.csv(submission, file='v2_show_of_hands.csv', row.names = FALSE)


