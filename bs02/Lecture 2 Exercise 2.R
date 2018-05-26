
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
library(ISLR)
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
# AIC ~ 581.8  

# It's clear that we can remove some variables. This should drop the AIC
# dropped 
# SalePriceMM            NA         NA      NA       NA    
# SalePriceCH            NA         NA      NA       NA    
# PriceDiff              NA         NA      NA       NA   
# 
# ListPriceDiff          NA         NA      NA       NA    
# STORE                  NA         NA      NA       NA   

# the following with very high p-values 
# Store7Yes        0.206458   0.886578   0.233  0.81586    
# DiscCH           5.398878  22.778150   0.237  0.81264   

OJ.glm = glm(formula = Purchase ~ WeekofPurchase + StoreID + PriceCH + PriceMM + DiscMM + SpecialCH + SpecialMM + LoyalCH + 
               PctDiscMM + PctDiscCH,
             data = training,
             family = "binomial")
summary(OJ.glm)
# AIC ~ 577.91  dropped from 581.8

# We can probably remove a few more
# WeekofPurchase  -0.007648   0.012010  -0.637 0.524272 
# SpecialCH        0.442230   0.408437   1.083 0.278926    
# SpecialMM        0.603260   0.332925   1.812 0.069986 .  

OJ.glm = glm(formula = Purchase ~                 StoreID + PriceCH + PriceMM + DiscMM +                        LoyalCH + 
               PctDiscMM + PctDiscCH,
             data = training,
             family = "binomial")

# AIC ~  575.71  dropped from 577.91


## try to remove PctDiscMM
# PctDiscMM   -41.1702    22.7686  -1.808 0.070575 .  
OJ.glmxx = glm(formula = Purchase ~                 StoreID + PriceCH + PriceMM + DiscMM +                        LoyalCH + 
               PctDiscCH,
             data = training,
             family = "binomial")
summary(OJ.glmxx)
# AIC 576.96 went up from 575.71, will go back to the previous one 
summary(OJ.glm)


# Let's stick with this last model

# we may have multi-collinearity present.  Use the 'pairs' plot function to check
pairs(OJ[, c("StoreID","PriceCH","PriceMM","DiscMM","LoyalCH","PctDiscMM","PctDiscCH")])

# QUESTION - are any predictors collinear?

## collinearitiy is observerd with the following: 
# PriceCH and Price MM
# DiscMM and PctDiscMM
library(dplyr)
OJSelect <- OJ %>% select(one_of(c("StoreID","PriceCH","PriceMM","DiscMM","LoyalCH","PctDiscMM","PctDiscCH")))

corrs <- round(cor(OJSelect),2)
library(corrplot)
corrplot(corrs, type="upper", order="hclust", tl.col="black", tl.srt=45)
# corr plot is showing strong DiscMM/PctDiscMM   and   PricCH/PriceMM   and StoreID/PctDiscCH
plot(OJSelect$StoreID, OJSelect$PctDiscCH) ## Storid is categorical, colinearity is misleading 

###########################
# Create probabilities and predictions
###########################

# add the probabilities to the testing data
testing$probability = predict(OJ.glm, newdata = testing, type = "response")



## which way to cut? 
plot(training$Purchase, OJ.glm$fitted.values)
plot(OJ.glm$fitted.values, training$Purchase )
plot(OJ.glm$fitted.values, OJ.glm$y)
unique(data.frame(OJ.glm$data$Purchase, OJ.glm$y))

# assume that the optimum probability threshold is 0.5
# Create the class prediction - our target is the "MM" class
testing$prediction = "CH"
testing[testing$probability >= 0.5, "prediction"] = "MM"

testing$prediction <- as.factor(testing$prediction)
# Have a look at the data
head(testing)

###########################
# Evaluation

# Create a confusion matrix (along with other measures) using the 
# function 'confusionMatrix' from the caret package

#install.packages("e1071")
confusionMatrix(data = testing$prediction, testing$Purchase)

#             Reference
# Prediction  CH  MM
#         CH 169  33
#         MM  26  92
# Pos Pred Value : 0.8366          
# Neg Pred Value : 0.7797   
##
##
##  now try by removing StoreID 


OJ.glmNoStoreID = glm(formula = Purchase ~                 PriceCH + PriceMM + DiscMM +                        LoyalCH + 
               PctDiscMM + PctDiscCH,
             data = training,
             family = "binomial")
testing$probability2 = predict(OJ.glmNoStoreID, newdata = testing, type = "response")
testing$prediction2 = "CH"
testing[testing$probability2 >= 0.5, "prediction2"] = "MM"
testing$prediction2 <- as.factor(testing$prediction2)
confusionMatrix(data = testing$prediction2, testing$Purchase)


#           Reference
# Prediction  CH  MM
#         CH 173  33
#         MM  22  92
# Pos Pred Value : 0.8398          
# Neg Pred Value : 0.8070    

