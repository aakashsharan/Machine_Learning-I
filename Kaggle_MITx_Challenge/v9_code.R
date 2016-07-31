library(randomForest)
library(xlsx)
library(caret)
library(mice)
set.seed(121)

ques <- read.xlsx('Questions.xlsx', 1)

train <- read.csv('train2016.csv')
test <- read.csv('test2016.csv')

train_new <- train
test_new <- test

simple_tr = train_new[c("USER_ID", "YOB")]
simple_te = test_new[c("USER_ID", "YOB")]
set.seed(121)
imputed_tr = complete(mice(simple_tr))
imputed_te = complete(mice(simple_te))

train_new$YOB = imputed_tr$YOB
test_new$YOB = imputed_te$YOB

reg1 <- glm(Party ~. -USER_ID, data = train_new, family = binomial())
summary(reg1)

reg2 <- glm(Party ~ YOB + Income + HouseholdStatus + Q109244 + Q115611 + Q98197 + Q113181 + Q110740 + Q116881 + Q118232, data = train_new, family = binomial())
summary(reg2)

rf <- randomForest(Party ~ YOB + Income + HouseholdStatus + Q121699 + Q121700 + Q120194 + Q118232 + Q116197 + Q115611 + Q114517 + Q113181 + Q112478 + Q112270 + Q108950 + Q109244 + Q101596 + Q100689 + Q98869 + Q98578, data = train_new, ntree = 500, importance = TRUE)

# only 2,3 starred important variables.
rf_2 <- randomForest(Party ~ HouseholdStatus + Q121699  + Q118232 + Q115611 + Q112478 + Q109244 + Q101596 + Q100689 + Q98869 + Q98578, data = train_new, ntree = 500, importance = TRUE)

# from least to most important variables.

rf_3 <- randomForest(Party ~ YOB + EducationLevel + HouseholdStatus + Q124742 + Q121699 + Q121700 + Q120194 + Q118232 + Q116197 + Q115611 + Q114517 + Q113181 + Q112478 + Q112270 + Q108950 + Q109244 + Q124742 + Q102687 + Q101596 + Q100689 + Q99716 + Q98869 + Q98578 + Q98197, data = train_new, ntree = 500, importance = TRUE)


rf_4 <-  randomForest(Party ~ YOB + Income + HouseholdStatus + Q109244 + Q115611 + Q98197 + Q113181 + Q110740 + Q116881 + Q118232, data = train_new, ntree = 500, importance = TRUE)

rf_5 <-  randomForest(Party ~ YOB + HouseholdStatus + Q109244 + Q115611 + Q98197 + Q113181 + Q116881 + Q118232, data = train_new, ntree = 500, importance = TRUE)

# most improved submission. 5/29/2016

submission <- data.frame(USER_ID = test_new$USER_ID)
submission$Predictions <- predict(rf, test_new)
write.csv(submission, file='v9_show_of_hands.csv', row.names = FALSE)

# most improved submission. 5/29/2016, 0.65374
submission2 <- data.frame(USER_ID = test_new$USER_ID)
submission2$Predictions <- predict(rf_4, test_new)
write.csv(submission2, file='v9.1_show_of_hands.csv', row.names = FALSE)


