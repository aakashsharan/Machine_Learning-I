# we are going to use census information about an individual to predict how much a person earns in particular, whether the person earns more than $50,000 per year

library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)

#lets read the data
census <- read.csv("census.csv")
str(census)

#now lets create a simple logistic regression model.
#we will split the data so that the training set contains 60% of the observations, while the testing set contains 40% of the observations.
set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio = 0.6)
train_model1 <- subset(census, spl == TRUE)
test_model1 <- subset(census, spl == FALSE)

reg_model1 <- glm(over50k ~ ., data = train_model1, family = binomial)
summary(reg_model1)

#lets find the accuracy of the model on the testing data with a threshold of 0.5
predictions_reg_model1 <- predict(reg_model1, newdata = test_model1, type= "response")
table(test_model1$over50k, predict_reg_model1 > 0.5)
(9051+1888)/nrow(test_model1) #0.8552107


#baseline accuracy
(9051+662)/nrow(test_model1) #0.7593621

#lets calculate the area-under-the-curve(AUC) for the model on the test set.
ROCR_pred <- prediction(predict_reg_model1, test_model1$over50k)
as.numeric(performance(ROCR_pred, "auc")@y.values) #0.9061598


#lets build a classification tree to predict.
cart_model1 <- rpart(over50k ~ ., data = train_model1, method = "class")
#lets plot the tree to see the splits.
prp(cart_model1)

#lets find the accuracy of the model on the testing data with a threshold of 0.5
predict_cart <- predict(cart_model1, newdata = test_model1, type = "class")
table(test_model1$over50k, predict_cart)
(9243 + 1596)/nrow(test_model1) #0.8473927

#lets plot ROC curve for the CART model.
predict_roc <- predict(cart_model1, newdata = test_model1)
head(predict_roc)

pred <- prediction(predict_roc[, 2], test_model1$over50k)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

#AUC of the CART model on the test set
predict_cart2 <- predict(cart_model1, newdata = test_model1)
predict_cart2 <- predict_cart2[, 2]
ROCR_pred2 <- prediction(predict_cart2, test_model1$over50k)
as.numeric(performance(ROCR_pred2, "auc")@y.values)

#we will define a new training set to be used when building our random forest model, that contains 2000 randomly selected obervations from the original training set
set.seed(1)
trainSmall = train_model1[sample(nrow(train_model1), 2000), ]

set.seed(1)
small_forest = randomForest(over50k ~ ., data = trainSmall)

#lets calculate accuracy of the model.
predict_small_forest = predict(small_forest, newdata = test_model1)
table(test_model1$over50k, predict_small_forest)
(9586 + 1093)/nrow(test_model1) #0.8348839


#lets plot a chart showing for each variable measures the number of times that variable was selected for splitting.
vu = varUsed(small_forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(small_forest$forest$xlevels[vusorted$ix]))

# plot showing which variables is important in terms of mean reduction in impurity
varImpPlot(small_forest)

#Let us select the cp parameter for our CART model using k-fold cross validation, with k = 10 folds.
set.seed(2)
num_folds = trainControl(method = 'cv', number = 10)
cp_grid = expand.grid(.cp = seq(0.002, 0.1, 0.002))

train(over50k ~ ., method = 'rpart', data = train_model1, trControl = num_folds, tuneGrid = cp_grid)
#max value is at cp = 0.002

#Lets fit a CART model to the training data using this value of cp.
CART_tree = rpart(over50k ~ ., data = train_model1, method = 'class', cp = 0.002)
prp(CART_tree)
#prediction accuracy on the test set
predict_cart_tree = predict(CART_tree, type = 'class', newdata = test_model1)
table(test_model1$over50k, predict_cart_tree)
#accuracy
(9178 + 1838)/nrow(test_model1) #0.8612306

#Compared to the original accuracy using the default value of cp, this new CART model is an improvement, and so we should clearly favor this new model.
#By tuning cp, we improved our accuracy by over 1%, but our tree became significantly more complicated.
prp(CART_tree)

