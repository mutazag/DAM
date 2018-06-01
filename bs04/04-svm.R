library(mlbench)
#load e1071 if not loaded
#library(e1071)
data(Sonar)
str(Sonar)
set.seed(42)
Sonar[,"train"] <- ifelse(runif(nrow(Sonar))<0.8,1,0)
#write dataframe to disk to check
write.csv(Sonar,"Sonar.csv")
#separate training and test sets
trainset <- Sonar[Sonar$train==1,]
testset <- Sonar[Sonar$train==0,]
trainColNum <- grep("train",names(trainset))
typeColNum <- grep("Class",names(Sonar))
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

#Try linear kernel...
svm_model<- svm(Class~ ., data=trainset, method="C-classification", kernel="linear")
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$Class)
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$Class)
#Comment on your results

#Try radial kernel with default settings
svm_model<- svm(Class~ ., data=trainset, method="C-classification", kernel="radial")
svm_model
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$Class)
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$Class)
#comment on your results

#Tune gamma - patience is a virtue...
tune_out <- tune.svm(x=trainset[,-typeColNum],y=trainset[,typeColNum],gamma=10^(-3:3),cost=c(0.01,0.1,1,10,100,1000),kernel="radial")
#Examine best parameters
tune_out$best.parameters$cost
tune_out$best.parameters$gamma
svm_model<- svm(Class~ ., data=trainset, method="C-classification", kernel="radial",cost=tune_out$best.parameters$cost,gamma=tune_out$best.parameters$gamma)
svm_model
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$Class)
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$Class)
