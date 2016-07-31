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
set.seed(121)
reg1 <- glm(Party ~. -USER_ID, data = train_new, family = binomial())
summary(reg1)

reg2 <- glm(Party ~ YOB + HouseholdStatus + Q120194 + Q118232 + Q116197 + Q115611 + Q114517 + Q113181 + Q112478 + Q112270 + Q109244 + Q102687 + Q101596 + Q100689 + Q98197 + Q118233 + Q116953 + Q116601 + Q107491 + Q102289, data = train_new, family = binomial())
summary(reg2)


rf <- randomForest(Party ~ YOB + Income + HouseholdStatus + Q124742 + Q121699 + Q121700 + Q120194 + Q118232 + Q116197 + Q115611 + Q114517 + Q113181 + Q112478 + Q112270 + Q108950 + Q109244 + Q108342 + Q102687 + Q101596 + Q100689 + Q99716 + Q98869 + Q98578 + Q98197, data = train_new, ntree = 500, importance = TRUE)


set.seed(121)
rf_5 <- randomForest(Party ~ YOB + HouseholdStatus + Q120194 + Q118232 + Q116197 + Q115611 + Q114517 + Q113181 + Q112478 + Q112270 + Q109244 + Q102687 + Q101596 + Q100689 + Q98197 + Q118233 + Q116953 + Q116601 + Q107491 + Q102289, data = train_new, ntree = 500, importance = TRUE)

#65086
submission2 <- data.frame(USER_ID = test_new$USER_ID)
submission2$Predictions <- predict(rf_5, test_new)
write.csv(submission2, file='v12_show_of_hands.csv', row.names = FALSE)


