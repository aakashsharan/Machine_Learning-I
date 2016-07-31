library(randomForest)
library(caret)

train <- read.csv('train2016.csv')
test <- read.csv('test2016.csv')

train_new <- train
test_new <- test

library(mice)

# Multiple imputation
simple_tr = train_new[c("USER_ID", "YOB")]
simple_te = test_new[c("USER_ID", "YOB")]
set.seed(144)
imputed_tr = complete(mice(simple_tr))
imputed_te = complete(mice(simple_te))

train_new$YOB = imputed_tr$YOB
test_new$YOB = imputed_te$YOB



rf <- randomForest(Party ~. -USER_ID, data = train_new, ntree = 500, importance = TRUE)

# most improved submission.

submission <- data.frame(USER_ID = test_new$USER_ID)
submission$Predictions <- predict(rf, test_new)
write.csv(submission, file='11_show_of_hands.csv', row.names = FALSE)


# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(144)
mtry <- sqrt(ncol(train_new))
rf_random <- train(Party ~. -USER_ID, data=train_new, method="rf",tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

