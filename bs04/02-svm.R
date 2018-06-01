#clear environment
rm(list=ls())

#load data partitioned earlier
trainset <- read.csv(file="linear_svm_trainset.csv")
trainset$y <- as.factor(trainset$y)
testset <- read.csv(file="linear_svm_testset.csv")
testset$y <- as.factor(testset$y)

library(e1071)

svm_model<- 
  svm(y ~ ., data=trainset, type="C-classification", kernel="linear", cost=100, scale=FALSE)

#training accuracy
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$y)

#test accuracy
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$y)

#Visualise using ggplot

#visualise training data, distinguish classes using colour

p1 <- ggplot(data=trainset, aes(x=x1,y=x2,colour=y)) + 
  geom_point()+ scale_colour_manual(values=c("red","blue"))

#identify support vectors 
df_sv <- trainset[svm_model$index,]

#mark out support vectors in plot
p1 <- p1 + geom_point(data=df_sv,aes(x=x1,y=x2),colour="purple",size = 4,alpha=0.5)
p1

#Explanation- we first plot all points in the training set. Then we overlay a purple circle
#on support vectors.

#We know the decision boundary is a straight line (why?)

#OK, so the next step is to extract the slope and intercept of the straight line from the
#svm object. This, unfortunately, needs some work...

#We use coefs and the support vectors to build the what's called the weight vector.
#The weight vector is the product of the coefs matrix with the matrix containing the SVs.
#Note, it makes sense that only the SVs play a role in defining the decision boundary. Why?

#build weight vector
w <- t(svm_model$coefs) %*% svm_model$SV #weight vector


#slope = -w[1]/w[2], intercept = rho/w[2]. Note that the intercept <> 0 and the slope
#is slightly less than 1. We know this is incorrect since we have designed the dataset
#to have a boundary of slope 1 and intercept 0. We'll see how to do better shortly

#calculate slope and save it to a variable
slope_1 <- -w[1]/w[2]

#calculate intercept and save it to a variable
intercept_1 <- svm_model$rho/w[2]

#plot decision boundary based on  calculated slope and intercept
p1 <- p1 + geom_abline(slope=slope_1,intercept = intercept_1)



#Notice that the boundary is "supported" by roughly the same number of support vectors
#on either side. This makes intuitive sense.

#Margins have the same slope (parallel to decision boundary) and lie +-1/w[2] on either
#side of the boundary

#add margins to plot
p1 <- p1 + 
  geom_abline(slope=slope_1,intercept = intercept_1-1/w[2], linetype="dashed")+
  geom_abline(slope=slope_1,intercept = intercept_1+1/w[2], linetype="dashed")

#display plot
p1
