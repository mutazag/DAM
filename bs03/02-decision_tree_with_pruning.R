#Part 2 - pruning
#load rpart, rpart.plot and mlbench if you haven't done so
library (rpart)
library(rpart.plot)
library(mlbench)


#setwd("C:/Users/Kailash/Documents/dam_lecture_3")


#Load Ionosphere  dataset (classification problem)
data("Ionosphere")
#Explore dataset
#names(Ionosphere)
#nrow(Ionosphere)
#ncol(Ionosphere)
#summary(Ionosphere)
#create training and test sets
set.seed(1)
Ionosphere[,"train"] <- ifelse(runif(nrow(Ionosphere))<0.8,1,0)
#write dataframe to disk to check
write.csv(Ionosphere,"Ionosphere.csv")
#separate training and test sets
trainset <- Ionosphere[Ionosphere$train==1,]
testset <- Ionosphere[Ionosphere$train==0,]
trainColNum <- grep("train",names(trainset))
#predicted variable is Class
typeColNum <- grep("Class",names(Ionosphere))
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
#build tree
#default params. This is a classification problem so set method="class"
rpart_model <- rpart(Class~.,data = trainset, method="class")
#plot tree (UNPRUNED - same as exercise 1)
prp(rpart_model)
#Unpruned model (same as exercise 1)
rpart_predict <- predict(rpart_model,testset[,-typeColNum],type="class")
mean(rpart_predict==testset$Class)
#confusion matrix (same as exercise 1)
table(pred=rpart_predict,true=testset$Class)

#cost complexity pruning
#cost-complexity plot - can you see the minimum in the plot?
plotcp(rpart_model)
#find minimum
opt <- which.min(rpart_model$cptable[,"xerror"])
cp <- rpart_model$cptable[opt, "CP"]
pruned_model <- prune(rpart_model,cp)
#plot tree (PRUNED)
prp(pruned_model)
#PRUNED model
rpart_pruned_predict <- predict(pruned_model,testset[,-typeColNum],type="class")

#write predictions to disk
test_predictions_dt2 <- data.frame(testset,rpart_pruned_predict)
write.csv(test_predictions_dt2,file="test_predictions_dt2.csv")

#Accuracy of pruned model
mean(rpart_pruned_predict==testset$Class)
#confusion matrix (PRUNED model)
table(pred=rpart_pruned_predict,true=testset$Class)