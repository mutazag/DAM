#Some general tips before you proceed:
# 1. Install the following packages before you proceed: rpart, rpart.plot, mlbench
# 2. Download documentation for rpart from CRAN: 
#    https://cran.r-project.org/web/packages/rpart/rpart.pdf 
# 3. Run the code line by line and examine the results.
# 4. Understand what each step does. Check out the environment panel (on the right
#    in RStudio) to see more about the variables created in each step.
# 5. Check the rpart documentation and understand what each parameter does (homework)
# 
##Instructions
# - create a folder called "decisiontrees" in your workarea for this subject


library (rpart)
library(rpart.plot)
library(mlbench)
#setwd("C:/Users/Kailash/Documents/decisiontrees")

setwd("c:/mdsi/dam/bs03")
#Load Ionosphere  dataset (classification problem)
data("Ionosphere")
#Explore dataset
names(Ionosphere)
nrow(Ionosphere)
ncol(Ionosphere)
summary(Ionosphere)
#create training and test sets
set.seed(42)
Ionosphere[,"train"] <- ifelse(runif(nrow(Ionosphere))<0.8,1,0) 
## or use CARET package to create training data and k-folds is part of that.


#write dataframe to disk to check
write.csv(Ionosphere,"Ionosphere.csv")
#separate training and test sets
trainset <- Ionosphere[Ionosphere$train==1,]
testset <- Ionosphere[Ionosphere$train==0,]
# this is just to get the columnnumber for the train flag so it can be removed
# later on form the train and test adata sets to avoid using it in the model
# fitting
trainColNum <- grep("train",names(trainset))
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

#predicted variable is Class -- same like train column, this is to grab the
#column niumber for column called "CLASS"
typeColNum <- grep("Class",names(Ionosphere))






#build tree

#default params. This is a classification problem so set method="class"
rpart_model <- rpart(Class~.,data = trainset, method="class")


#plot tree - SAVE PLOT for comparison later
#plot(rpart_model);text(rpart_model)


#prp from rpart.plot produces nicer plots
prp(rpart_model)
#summary
summary(rpart_model)
#predict on test data
rpart_predict <- predict(rpart_model,testset[,-typeColNum],type="class")

#write predictions to disk
test_predictions_dt1 <- data.frame(testset,rpart_predict)
write.csv(test_predictions_dt1,file="test_predictions_dt.csv")

#accuracy
mean(rpart_predict==testset$Class)
#confusion matrix
table(pred=rpart_predict,true=testset$Class)
#Now rerun for different values of seed (say 53 and 1)  and re-plot tree, calculate
#accuracy and confusion matrix for each. Comment on the plots.
