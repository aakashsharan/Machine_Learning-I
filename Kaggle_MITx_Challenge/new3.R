library(randomForest)
library(mice)
set.seed(121)

train <- read.csv('train2016.csv')
test <- read.csv('test2016.csv')

train_other <- subset(train, YOB <=2003)
test_other <- test

# Test data imputation
simple_te = test_other[c("USER_ID", "YOB")]
set.seed(121)
imputed_te = complete(mice(simple_te))

test_other$YOB = imputed_te$YOB
summary(test_other)

rf <- randomForest(Party ~. -USER_ID, data = train_other, ntree = 500, importance = TRUE)

submission <- data.frame(USER_ID = test_other$USER_ID)
submission$Predictions <- predict(rf, test_other)
write.csv(submission, file='new3_show_of_hands.csv', row.names = FALSE)
