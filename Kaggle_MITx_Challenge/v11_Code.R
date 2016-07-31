library(randomForest)
library(caret)
library(mice)
library(xlsx)
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

reg2 <- glm(Party ~ YOB + HouseholdStatus + Q118232  + Q115611  + Q112478 + Q109244 + Q98869 + Q114517 + Q112270 + Q101596 + Q100689, data = train_new, family = binomial())
summary(reg2)

set.seed(121)
rf <- randomForest(Party ~ YOB + Income + HouseholdStatus + Q118232 + Q116197 + Q115611 + Q114517 + Q112478 + Q112270 + Q109244 + Q101596 + Q100689 + Q98869 + Q98578 + Q98197, data = train_new, ntree = 500, importance = TRUE)

rf_4 <-  randomForest(Party ~ YOB + Income + HouseholdStatus + Q109244 + Q115611 + Q98197 + Q113181 + Q110740 + Q116881 + Q118232, data = train_new, ntree = 500, importance = TRUE)

set.seed(121)
rf_5 <- randomForest(Party ~ YOB + HouseholdStatus + Q118232  + Q115611  + Q112478 + Q109244 + Q98869 + Q114517 + Q112270 + Q101596 + Q100689, data = train_new, ntree = 500, importance = TRUE)

# my best entry till today, 0.66667
submission <- data.frame(USER_ID = test_new$USER_ID)
submission$Predictions <- predict(rf, test_new)
write.csv(submission, file='v11_show_of_hands.csv', row.names = FALSE)

submission2 <- data.frame(USER_ID = test_new$USER_ID)
submission2$Predictions <- predict(rf_5, test_new)
write.csv(submission2, file='v11.1_show_of_hands.csv', row.names = FALSE)


