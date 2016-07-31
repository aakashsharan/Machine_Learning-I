library(xlsx)
library(randomForest)
library(mice)
library(caTools)
set.seed(121)

ques <- read.xlsx('Questions.xlsx', 1)
#summary(ques)

train <- read.csv('train2016.csv', na.strings = c('', ' ', 'NA'))
test <- read.csv('test2016.csv')

train_new <- train

rf_imputed <- rfImpute(Party ~ ., train_new)
save(rf_imputed, file='rf_train_imputed.rda')

spl <- sample.split(rf_imputed$Party, SplitRatio = 0.7)
train_final <- subset(rf_imputed, spl == TRUE)
test_final <- subset(rf_imputed, spl == FALSE)


# OOB estimate of  error rate:  38.22%
rf <- randomForest(Party ~ ., data = train_final, importance = TRUE, proximity = TRUE)

#tuning 

# OOB estimate of  error rate: 38.17%
rf_tune1 <- randomForest(Party ~ ., data = train_final, ntree = 500, mtry = 10.3, importance = TRUE, proximity = TRUE)

# OOB estimate of  error rate: 38.25%
rf_tune2 <- randomForest(Party ~ ., data = train_final, ntree = 400, mtry = 10.3, importance = TRUE, proximity = TRUE)

# OOB estimate of  error rate: 38.12%
rf_tune3 <- randomForest(Party ~ ., data = train_final, ntree = 300, mtry = 10.3, importance = TRUE, proximity = TRUE)

# worst submission.

submission <- data.frame(USER_ID = test_final$USER_ID)
submission$Predictions <- predict(rf_tune3, test_final)
submission <- submission[1:1392 ,]
write.csv(submission, file='v5_code_show_of_hands.csv', row.names = FALSE)
