#Some general tips before you proceed:
# 1. Install the following packages before you proceed: randomForest
# 2. Download documentation for rpart from CRAN: 
#    https://cran.r-project.org/web/packages/rpart/rpart.pdf 
# 3. Run the code line by line and examine the results.
# 4. Understand what each step does. Check out the environment panel (on the right
#    in RStudio) to see more about the variables created in each step.
# 5. Check the randomForest documentation and understand what each parameter does (homework)
# 

#set working directory if needed (modify path as needed)
#setwd("C:/Users/Kailash/Documents/dam_lecture_3")
#load required libraries - rpart for classification and regression trees
library(rpart)
#mlbench for Glass dataset
library(mlbench)
#load Glass
data("Glass")
#set seed to ensure reproducible results
set.seed(42)
#split into training and test sets
Glass[,"train"] <- ifelse(runif(nrow(Glass))<0.8,1,0)
#separate training and test sets
trainGlass <- Glass[Glass$train==1,]
testGlass <- Glass[Glass$train==0,]
#get column index of train flag
trainColNum <- grep("train",names(trainGlass))
#remove train flag column from train and test sets
trainGlass <- trainGlass[,-trainColNum]
testGlass <- testGlass[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep("Type",names(Glass))
#build model
rpart_model <- rpart(Type ~.,data = trainGlass, method="class")
#plot tree
prp(rpart_model)
#...and the moment of reckoning
rpart_predict <- predict(rpart_model,testGlass[,-typeColNum],type="class")
mean(rpart_predict==testGlass$Type)


library(randomForest)
data("Glass")
set.seed(42)
Glass[,"train"] <- ifelse(runif(nrow(Glass))<0.8,1,0)
#write dataframe to disk to check
#write.csv(Glass,"Glass.csv")
#separate training and test sets
trainGlass <- Glass[Glass$train==1,]
testGlass <- Glass[Glass$train==0,]
trainColNum <- grep("train",names(trainGlass))
typeColNum <- grep("Type",names(Glass))
trainGlass <- trainGlass[,-trainColNum]
testGlass <- testGlass[,-trainColNum]
#Build random forest model
Glass.rf <- randomForest(Type ~.,data = trainGlass, 
                       importance=TRUE, xtest=testGlass[,-typeColNum],ntree=1000)
#model summary
summary(Glass.rf)
#variables contained in model 
names(Glass.rf)

#predictions for test set
test_predictions_rf <- data.frame(testGlass,Glass.rf$test$predicted)
write.csv(test_predictions_rf,file="test_predictions_rf.csv")

#accuracy for test set
mean(Glass.rf$test$predicted==testGlass$Type)
#confusion matrix
table(Glass.rf$test$predicted,testGlass$Type)


#quantitative measure of variable importance
importance(Glass.rf)
#sorted plot of importance
varImpPlot(Glass.rf)

