
###############################################################################
## Lecture 4 Exercise 4
#  Boosted Models
###############################################################################

# This exercise involves building a gbm model 
# We will use the same Glass data set from the previous exercise
# install.packages("gbm")
# install.packages("caret")


setwd("C:/mdsi/dam/bs03")

library(gbm)
library(caret)
library(ISLR)

# recall the OJ data set for classification modelling.  Purchase is our target
# create data partition row list
set.seed(42)
train = createDataPartition(y = OJ$Purchase, p = 0.7, list = F)


# partition default data - remove the variable Store7
training = OJ[train, ]
testing = OJ[-train, ]

training$Purchase_binary = 0
training[training$Purchase == "MM", "Purchase_binary"] = 1

testing$Purchase_binary = 0
testing[testing$Purchase == "MM", "Purchase_binary"] = 1


#######################################
## train the gbm model
#######################################

# defining some parameters
gbm_depth = 5 #maximum nodes per tree
gbm_n.min = 5 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.001 #learning rate
# cores_num = 4 #number of cores
gbm_cv.folds=5 #number of cross-validation folds to perform
num_trees = 5000

# fit initial model
gbm_fit = gbm(Purchase_binary~.,
                  data=training[, -1],
                  distribution='bernoulli', 
                  n.trees=num_trees, #the number of GBM interaction
                  interaction.depth= gbm_depth,
                  n.minobsinnode = gbm_n.min, 
                  shrinkage=gbm_shrinkage, 
                  cv.folds=gbm_cv.folds, 
                  verbose = T #print the preliminary output
                  # n.cores = cores_num
)

summary(gbm_fit)


best.iter = gbm.perf(gbm_fit, method = "cv")
###How many trees should we use?

testing$probability = predict(gbm_fit, testing, n.trees = best.iter, type = "response")

testing$prediction = 0

# Modify the probability threshold to see if you can get a better accuracy
testing[testing$probability >= 0.5, "prediction"] = 1
testing$Purchase_binary = as.factor(testing$Purchase_binary)
testing$prediction = as.factor(testing$prediction)
confusionMatrix(testing$prediction, testing$Purchase_binary)

