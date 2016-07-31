require(xgboost)
require(method)
require(data.table)
require(magrittr)


train <- fread('imputed_train.csv', header = T, stringsAsFactors = F)
test <- fread('imputed_test.csv', header = T, stringsAsFactors = F)

train[, USER_ID:= NULL]
test[, USER_ID:= NULL]

nameLabel <- 'Party'

train$Party[train$Party == 'Democrat'] <- 'C_1'
train$Party[train$Party == 'Republican'] <- 'C_2'

y <- train[, nameLabel, with = F][[1]] %>% gsub('[C_]+', '',.) %>% {as.integer(.) -1}

train[, nameLabel:=NULL, with = F]
train

set.seed(121)
trainMatrix <- train[, lapply(.SD, as.numeric)] %>% as.matrix
testMatrix <- test[, lapply(.SD, as.numeric)] %>% as.matrix

numberOfClasses <- max(y) + 1
param <- list('objective' = 'multi:softprob', 'eval_metric' = 'mlogloss', 'num_class' = numberOfClasses)

cv.nround <- 5
cv.nfold <- 3
bst.cv = xgb.cv(param = param, data = trainMatrix, label = y, nfold = cv.nfold, nrounds = cv.nround)














