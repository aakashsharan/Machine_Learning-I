# we are going to build a logistic regression model to construct US Presedential election predictions.
library(mice)
library(ggplot2)
library(ggmap)

# Read in the data
polling = read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)


# As some of the data have NA's, lets do multiple imputation using mice package.
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# Subset data into training set and test set
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# Smart Baseline
table(Train$Republican)
table(sign(Train$Rasmussen))
table(Train$Republican, sign(Train$Rasmussen))

# Lets view Multicollinearity
str(Train)
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# Lets build a Logistic Regression Model using PropR as the only independent variable.
mod1 = glm(Republican~PropR, data=Train, family="binomial")
summary(mod1)

# Training set predictions
pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >= 0.5)

# Now Lets build a Logistic Regression Model using SurveyUSA and DiffCount as the independent variables.
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >= 0.5)
summary(mod2)

# Smart baseline accuracy
table(Test$Republican, sign(Test$Rasmussen))

# Test set predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5)

# Analyze mistake
subset(Test, TestPrediction >= 0.5 & Republican == 0)

# lets create some infographics to understand forecasting better.
states_map <- map_data('state')
str(states_map)
# map of the United States.
ggplot(states_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = 'white', color = 'black')
# let's create a vector of Republican/Democrat predictions
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
# put the predictions and state labels in a data.frame so that we can use ggplot
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

# now we merge "predictionDataFrame" with the map data "statesMap"
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(states_map, predictionDataFrame, by = "region")
# lets make sure the observations are in order so that the map is drawn properly
predictionMap = predictionMap[order(predictionMap$order),]

# lets color the states according to our binary predictions. light blue represent republican.
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = 'black')
# Let's replot the map with discrete outcomes
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

