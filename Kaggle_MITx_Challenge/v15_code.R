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


rf_4 <-  randomForest(Party ~ YOB + Income + HouseholdStatus + Q121699 + Q120194 + Q109244 + Q115611 + Q98197 + Q113181 + Q110740 + Q116881 + Q118232 + Q112478 + Q109244 + Q101596 + Q98578 + Q102687 + Q114517 + Q112270 + Q98869 + Q99716 + Q100689 + Q116197, data = train_new, ntree = 500, importance = TRUE)


submission <- data.frame(USER_ID = test_new$USER_ID)
submission$Predictions <- predict(rf_4, test_new)
write.csv(submission, file='v15_show_of_hands.csv', row.names = FALSE)


