library(xlsx)
library(randomForest)
library(caret)
library(mice)
set.seed(121)

ques <- read.xlsx('Questions.xlsx', 1)
#summary(ques)

train <- read.csv('train2016.csv', na.strings = c('', ' ', 'NA'))
test <- read.csv('test2016.csv', na.strings = c('', ' ', 'NA'))
#summary(train)

imputeFeatures <- function(data) {
  data_imp <- data
  data_imp$Party <- NULL
  features <- names(data_imp)
  vec_fea <- c(features)
  
  simple_tr = data_imp[vec_fea]
  set.seed(121)
  imputed_tr = complete(mice(simple_tr))
  
  for (m_cols in features){
        print(data_imp$m_cols)
        data_imp$m_cols <- imputed_tr$m_cols
      }
  return(data_imp)
}

dats <- train[0:50, ]
summary(dat)
View(dat)

imput_dat <- imputeFeatures(dats)

train_imputed <- imputeFeatures(train)
test_imputed <- imputeFeatures(test)

rf <- randomForest(train_imputed, as.factor(train$Party), ntree = 500, importance = TRUE)



submission <- data.frame(USER_ID = test_other$USER_ID)
submission$Predictions <- predict(rf, test_other)
write.csv(submission, file='new3_show_of_hands.csv', row.names = FALSE)






rf <- randomForest(Party ~. -USER_ID, data = train_new, ntree = 500, importance = TRUE)

