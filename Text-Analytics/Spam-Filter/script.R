# we are going to build and evaluate a spam filter using Enron email dataset provided by MIT.

library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(randomForest)
library(rpart.plot)
library(ROCR)

#lets read the data.
emails <- read.csv('emails.csv', stringsAsFactors = FALSE)
nrow(emails)
str(emails)
table(emails$spam)

#lets create a corpus
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)

#now lets build a DocumentTermMatrix
dtm <- DocumentTermMatrix(corpus)
dtm

#dtm contains a lot of terms, lets obtain a more reasonable number of terms by limiting dtm to contain terms appearing in at least 5% of documents
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm
#now lets build a data frame
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
#lets add a variable spam to emailsSparse by copying from the original data frame.
emailsSparse$spam <- emails$spam
#lets view the most appeared word stem
sort(colSums(subset(emailsSparse, spam == 0)))

#we will create now machine learning models to predict spam.
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)

#create training and test data models.
spl <-  sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, spl == TRUE)
test <- subset(emailsSparse, spl == FALSE)

#logistic regression model. this is going to overfit.
spamLog <- glm(spam ~ ., data = train, family = binomial())
#classification tree model
spamCART <- rpart(spam ~., data = train, method = 'class')
set.seed(123)
#random forest model
spamRF <- randomForest(spam ~ ., data = train)

#lets calculate prediction on the training set.
predict_log <- predict(spamLog, type = 'response')
# predicted probabilities from spamLog are less than 0.00001 for training set.
tick1 <- predict_log < 0.00001
table(tick1)
#predicted probabilities from spamLog are more than 0.99999 for training set.
tick2 <- predict_log > 0.99999
table(tick2)
#predicted probabilities from spamLog are between 0.00001 and 0.99999 for training set.
tick3 <- (predict_log > 0.00001 & predict_log < 0.99999)
table(tick3)
# prediction for cart and rf model.
predict_cart <- predict(spamCART)[, 2]
predict_rf <- predict(spamRF, type='prob')[, 2]

#lets plot cart tree.
prp(spamCART)

#training set accuracy of spamLog, using a threshold of 0.5 for predictions
table(train$spam, predict_log >= 0.5)
(3052 + 954)/(3052 + 0 + 4 + 954) #0.9990025

#training set AUC of spamLog
pred_ROCR <- prediction(predict_log, train$spam)
as.numeric(performance(pred_ROCR, 'auc')@y.values) #0.9999959

#training set accuracy of spamCART, using a threshold of 0.5 for predictions
table(train$spam, predict_cart >= 0.5)
(2892 + 889)/nrow(train)

#training set AUC of spamCART
pred_ROCR_cart <- prediction(predict_cart, train$spam)
as.numeric(performance(pred_ROCR_cart, 'auc')@y.values)

#training set accuracy of spamRF, using a threshold of 0.5 for predictions
table(train$spam, predict_rf > 0.5)
(3024 + 903)/nrow(train)
#training set AUC of spamRF
pred_ROCR_RF <- prediction(predict_rf, train$spam)
as.numeric(performance(pred_ROCR_RF, 'auc')@y.values)

#testing set accuracy of spamLog, using a threshold of 0.5 for predictions
predict_log2 <- predict(spamLog, type = 'response', newdata = test)
predict_cart2 <- predict(spamCART, newdata = test)[, 2]
predict_rf2 <- predict(spamRF, type='prob', newdata = test)[, 2]
table(test$spam, predict_log2 >= 0.5)
(1243 + 370)/nrow(test)
#testing set AUC of spamLog
pred_ROCR_test <- prediction(predict_log2, test$spam)
as.numeric(performance(pred_ROCR_test, 'auc')@y.values)

#testing set accuracy of spamCART, using a threshold of 0.5 for predictions
table(test$spam, predict_cart2 >= 0.5)
(1238 + 376)/nrow(test)
#testing set AUC of spamCART
pred_ROCR_cart_test <- prediction(predict_cart, test$spam)
as.numeric(performance(pred_ROCR_cart_test, 'auc')@y.values)

#testing set accuracy of spamRF, using a threshold of 0.5 for predictions
table(test$spam, predict_rf2 >= 0.5)
(1297 + 384)/nrow(test)
#testing set AUC of spamRF
pred_ROCR_RF_test <- prediction(predict_rf, test$spam)
as.numeric(performance(pred_ROCR_RF_test, 'auc')@y.values)


# ok, so lets create some wordclouds for better understanding.
# the most common word that appears at least in 5% of the emails

emailsCloud <- as.data.frame(as.matrix(spdtm))
wordcloud(colnames(emailsCloud), colSums(emailsCloud), scale = c(4, 0.5))
# well we expect to see "enron" to appear in the most.

# now lets style our graph in various different ways to tell the stories.
wordcloud(colnames(emailsCloud), colSums(emailsCloud), scale = c(4, 0.5), random.order = FALSE, colors=brewer.pal(9, "Blues"))
wordcloud(colnames(emailsCloud), colSums(emailsCloud), scale = c(4, 0.5), random.order = FALSE, colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])

