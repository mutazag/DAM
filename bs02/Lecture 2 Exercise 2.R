
###############################################################################
## Lecture 2 Exercise 2
#  Logistic Regression
###############################################################################

# This exercise involves training a logistic regression model
# We will us the OJ data set from the ISLR package - Orange Juice sales
# We will also use the caret package to make the process easier

install.packages("caret")
library(caret)

# Let's start by summarising our data
str(OJ)
summary(OJ)

###########################
# Paritioning
###########################

# We want to partition our data into 70% for training, 30% for testing

# create data partition row list
set.seed(42)  # setting a random seed ensures we get the same result each time
# We will use the function 'createDataPartition' from the caret package
?createDataPartition
train = createDataPartition(y = OJ$Purchase, p = 0.7, list = F)
# partition OJ data into two sets 
training = OJ[train, ]
testing = OJ[-train, ]
str(training)
str(testing)

###########################
# Variable selection
###########################

# In this section, we will select which variables we want to include in our model
# We'll do this by backwards selection - start with everything and remove one by one

# let's start by throwing all the variables into the logistic regression
OJ.glm = glm(formula = Purchase ~ .,
             data = training,
             family = "binomial")
summary(OJ.glm)
# AIC ~ 607

# It's clear that we can remove some variables. This should drop the AIC
OJ.glm = glm(formula = Purchase ~ WeekofPurchase + StoreID + PriceCH + PriceMM + DiscMM + SpecialCH + SpecialMM + LoyalCH + 
               PctDiscMM + PctDiscCH,
             data = training,
             family = "binomial")
summary(OJ.glm)
# AIC ~ 604

# We can probably remove a few more
OJ.glm = glm(formula = Purchase ~ StoreID + PriceCH + PriceMM + DiscMM + LoyalCH + PctDiscMM + PctDiscCH,
             data = training,
             family = "binomial")
summary(OJ.glm)
# AIC ~ 598

# Let's stick with this last model

# we may have multi-collinearity present.  Use the 'pairs' plot function to check
pairs(OJ[, c("StoreID","PriceCH","PriceMM","DiscMM","LoyalCH","PctDiscMM","PctDiscCH")])

# QUESTION - are any predictors collinear?

###########################
# Create probabilities and predictions
###########################

# add the probabilities to the testing data
testing$probability = predict(OJ.glm, newdata = testing, type = "response")

# assume that the optimum probability threshold is 0.5
# Create the class prediction - our target is the "MM" class
testing$prediction = "CH"
testing[testing$probability >= 0.5, "prediction"] = "MM"

# Have a look at the data
head(testing)

###########################
# Evaluation

# Create a confusion matrix (along with other measures) using the 
# function 'confusionMatrix' from the caret package
confusionMatrix(data = testing$prediction, testing$Purchase)


