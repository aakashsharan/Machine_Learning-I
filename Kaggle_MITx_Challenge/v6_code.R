library(randomForest)
library(caret)
library(mice)

train <- read.csv('train2016.csv')
test <- read.csv('test2016.csv')

train_new <- train
test_new <- test


# Multiple imputation
simple_tr = train_new[c("USER_ID", "YOB")]
simple_te = test_new[c("USER_ID", "YOB")]
set.seed(144)
imputed_tr = complete(mice(simple_tr))
imputed_te = complete(mice(simple_te))

train_new$YOB = imputed_tr$YOB
test_new$YOB = imputed_te$YOB

rf <- randomForest(Party ~ Gender + Income + HouseholdStatus + EducationLevel + Q121699 + Q121700 + Q99716 + Q123464 + Q122120 + Q122769 + Q121011 + Q119334 +  Q118892 + Q116441 + Q116197 + Q112270 +  Q110740 + Q107491 + Q105655 + Q103293 + Q101596, data = train_new, ntree = 500, importance = TRUE)

# most improved submission.

submission <- data.frame(USER_ID = test_new$USER_ID)
submission$Predictions <- predict(rf, test_new)
write.csv(submission, file='v6_show_of_hands.csv', row.names = FALSE)
