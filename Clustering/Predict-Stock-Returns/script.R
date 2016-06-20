# we will first do clustering to identify clusters of stocks with similar returns over time and then we will use logistic regression to predict whether or not the stocks will have positive returns.

library(caTools)
library(caret)
library(flexclust)

# lets read the dataset
stocks <- read.csv('StocksCluster.csv')
nrow(stocks)
str(stocks)
summary(stocks)

# observations that have positive returns
table(stocks$PositiveDec == 1)

#correlation between independent variables
cor(stocks)

# lets train a logistic regression model to predict PositiveDec.
set.seed(144)

spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)

StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = binomial())
# overall accuracy on the training set, using a threshold of 0.5
predict_model <- predict(StocksModel, type = 'response')
table(stocksTrain$PositiveDec, predict_model >= 0.5)
(990 + 3640)/nrow(stocksTrain) #0.5711818

# overall accuracy of the model on the test, using a threshold of 0.5
predict_test <- predict(StocksModel, newdata = stocksTest, type = 'response')
table(stocksTest$PositiveDec, predict_test >= 0.5)
(417 + 1553)/nrow(stocksTest) #0.5670697

# accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)
table(stocksTest$PositiveDec)
1897/(nrow(stocksTest))

# lets cluster the stocks
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
# lets normalize the data
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
# check if we had done the right stuff.
summary(normTrain$ReturnJan)
summary(normTest$ReturnJan)

set.seed(144)
k <- 3
km <- kmeans(normTrain, k)
km_cluster <- km$cluster
km_clust <- split(normTrain, km_cluster)

km.kcca <- as.kcca(km, normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata = normTest)
table(clusterTest)

# lets build for training and testing data frames for each clusters.
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

# lets check the value of dependent variable for each train models.
mean(stocksTrain1$PositiveDec) 
mean(stocksTrain2$PositiveDec) 
mean(stocksTrain3$PositiveDec)

# now we build logistic regression models to predict PositiveDec
StocksModel1 <- glm(PositiveDec ~., data = stocksTrain1, family = binomial())
StocksModel2 <- glm(PositiveDec ~., data = stocksTrain2, family = binomial())
StocksModel3 <- glm(PositiveDec ~., data = stocksTrain3, family = binomial())

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

# lets do test-set predictions and compute the accuracy for each regression model.
PredictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = 'response')
PredictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = 'response')
PredictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = 'response')

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(30 + 774)/nrow(stocksTest1) #0.6194145
(388 + 757)/nrow(stocksTest2) #0.5504808
(49 + 13)/nrow(stocksTest3) #0.6458333

# To compute the overall test-set accuracy of the cluster-then-predict approach, we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector.
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)
(467 + 1544)/(467 + 1110 + 353 + 1544) #0.5788716
