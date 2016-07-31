library(xlsx)
library(randomForest)
library(caret)
library(mice)
set.seed(121)

ques <- read.xlsx('Questions.xlsx', 1)
#summary(ques)

train <- read.csv('train2016.csv', na.strings = c('', ' ', 'NA'))
test <- read.csv('test2016.csv', na.strings = c('', ' ', 'NA'))

simple_tr <- train[c("YOB","Gender","Income","HouseholdStatus","EducationLevel","Party","Q124742", "Q124122", "Q123464", "Q123621", "Q122769", "Q122770", "Q122771", "Q122120", "Q121699", "Q121700", "Q120978", "Q121011", "Q120379", "Q120650", "Q120472", "Q120194", "Q120012", "Q120014", "Q119334", "Q119851", "Q119650", "Q118892", "Q118117", "Q118232", "Q118233", "Q118237", "Q117186", "Q117193", "Q116797", "Q116881", "Q116953", "Q116601", "Q116441", "Q116448", "Q116197", "Q115602", "Q115777", "Q115610", "Q115611", "Q115899", "Q115390", "Q114961", "Q114748", "Q115195", "Q114517", "Q114386", "Q113992", "Q114152", "Q113583", "Q113584", "Q113181", "Q112478", "Q112512", "Q112270", "Q111848", "Q111580", "Q111220", "Q110740", "Q109367", "Q108950", "Q109244", "Q108855", "Q108617", "Q108856", "Q108754", "Q108342", "Q108343", "Q107869", "Q107491", "Q106993", "Q106997", "Q106272", "Q106388", "Q106389", "Q106042", "Q105840", "Q105655", "Q104996", "Q103293", "Q102906", "Q102674", "Q102687", "Q102289", "Q102089", "Q101162", "Q101163", "Q101596", "Q100689", "Q100680", "Q100562", "Q99982","Q100010", "Q99716",  "Q99581",  "Q99480",  "Q98869",  "Q98578",  "Q98059", "Q98078","Q98197","Q96024")]

simple_tst <- test[c("YOB","Gender","Income","HouseholdStatus","EducationLevel","Q124742", "Q124122", "Q123464", "Q123621", "Q122769", "Q122770", "Q122771", "Q122120", "Q121699", "Q121700", "Q120978", "Q121011", "Q120379", "Q120650", "Q120472", "Q120194", "Q120012", "Q120014", "Q119334", "Q119851", "Q119650", "Q118892", "Q118117", "Q118232", "Q118233", "Q118237", "Q117186", "Q117193", "Q116797", "Q116881", "Q116953", "Q116601", "Q116441", "Q116448", "Q116197", "Q115602", "Q115777", "Q115610", "Q115611", "Q115899", "Q115390", "Q114961", "Q114748", "Q115195", "Q114517", "Q114386", "Q113992", "Q114152", "Q113583", "Q113584", "Q113181", "Q112478", "Q112512", "Q112270", "Q111848", "Q111580", "Q111220", "Q110740", "Q109367", "Q108950", "Q109244", "Q108855", "Q108617", "Q108856", "Q108754", "Q108342", "Q108343", "Q107869", "Q107491", "Q106993", "Q106997", "Q106272", "Q106388", "Q106389", "Q106042", "Q105840", "Q105655", "Q104996", "Q103293", "Q102906", "Q102674", "Q102687", "Q102289", "Q102089", "Q101162", "Q101163", "Q101596", "Q100689", "Q100680", "Q100562", "Q99982","Q100010", "Q99716",  "Q99581",  "Q99480",  "Q98869",  "Q98578",  "Q98059", "Q98078","Q98197","Q96024")]

set.seed(121)
imputed_train <- complete(mice(simple_tr))
imputed_test <- complete(mice(simple_tst))

# OOB estimate of  error rate: 34.07%
rf <- randomForest(Party ~. , data = imputed_train, ntree = 500, importance = TRUE)

# OOB estimate of  error rate: 33.1%
rf_2 <- randomForest(Party ~. , data = imputed_train, ntree = 500, mtry = 10.34408, importance = TRUE, proximity = TRUE)

# OOB estimate of  error rate: 33.73%
rf_3 <- randomForest(Party ~. , data = imputed_train, ntree = 400, mtry = 10.34408, importance = TRUE, proximity = TRUE)

# OOB estimate of  error rate: 34.27%
rf_4 <- randomForest(Party ~. , data = imputed_train, ntree = 300, mtry = 10.34408, importance = TRUE, proximity = TRUE)


submission <- data.frame(USER_ID = test$USER_ID)
submission$Predictions <- predict(rf, imputed_test)
write.csv(submission, file='v8_show_of_hands.csv', row.names = FALSE)

