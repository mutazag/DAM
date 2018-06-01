#generating a radially separable dataset,

#set number of variables and seed
n <- 200
set.seed(42)

#Generate dataframe with 2 uniformly distributed predictors lying between -1 and 1.
#Call the 2 predictors are x1 and x2

df <- data.frame(x1=runif(n,min=-1,max=1),x2=runif(n,min=-1,max=1))


#We want a circular boundary. Set boundary radius 
radius <- 0.7
radius_squared <- radius^2

#create dependent categorical variable, y, with value -1 or 1 depending on whether it lies
#within or outside the circle.

df$y <- factor(ifelse(df$x1^2+df$x2^2<radius_squared,-1,1),levels=c(-1,1))

#save for later use
write.csv(df,file="radially_separable_no_margin.csv",row.names = FALSE)

#plot data with separation line
#differentiate class by colour
#suppress legend

library(ggplot2)
p <- ggplot(data=df, aes(x=x1,y=x2,colour=y)) + 
  geom_point() +
  scale_colour_manual(values=c("red","blue"))
p


#create function that returns a dataframe of npoint points that lie on a circle of 
#radius r centered at (x1_center,x2_center).
circle <- function(x1_center, x2_center, r, npoint = 100){
  #generate x, y coordinates for points on circle with an angular spacing of 2*pi/npoint
  theta <- seq(0,2*pi,length.out = npoint)
  x1_circ <- x1_center + r * cos(theta)
  x2_circ <- x2_center + r * sin(theta)
  return(data.frame(x1c = x1_circ, x2c = x2_circ))
}

#generate boundary
boundary <- circle(x1_center=0,x2_center=0,r=radius)

p <- p + geom_path(data=boundary,aes(x=x1c,y=x2c),inherit.aes = FALSE)
p

#Solve using linear kernel and polynomial kernel of degree 2.

library(e1071)

#read in dataset
df3 <- read.csv(file = "radially_separable_no_margin.csv")
df3$y <- as.factor(df3$y)

#split into train and test


set.seed(10)
df3[,"train"] <- ifelse(runif(nrow(df3))<0.8,1,0)
names(df3)
#separate training and test sets
trainset <- df3[df3$train==1,]
testset <- df3[df3$train==0,]
trainColNum <- grep("train",names(trainset))
typeColNum <- grep("y",names(df3))
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

# Let's try a linear kernel (start with default value of cost and then increase to 1000)

svm_model<- 
  svm(y ~ ., data=trainset, type="C-classification", 
      cost=1,
      kernel="linear")

#model summary
summary(svm_model)


#predictions
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$y)
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$y)

plot(svm_model,trainset)

#average accuracy
accuracy <- rep(NA,100)
set.seed(10)
for (i in 1:100){
  df3[,"train"] <- ifelse(runif(nrow(df3))<0.8,1,0)
  trainset <- df3[df3$train==1,]
  testset <- df3[df3$train==0,]
  trainColNum <- grep("train",names(trainset))
  trainset <- trainset[,-trainColNum]
  testset <- testset[,-trainColNum]
  svm_model<- 
    svm(y ~ ., data=trainset, 
        type="C-classification", 
        cost=1,
        kernel="linear")
  pred_test <- predict(svm_model,testset)
  accuracy[i] <- mean(pred_test==testset$y)
}
mean(accuracy)
sd(accuracy)


# train model - polynomial kernel, degree=2 with default settings for params cost, coef0 and gamma (1,0 and 0.5)
# don't scale to allow comparison to original
svm_model<- 
  svm(y ~ ., data=trainset, type="C-classification", 
      kernel="polynomial", degree=2)
#model summary
summary(svm_model)


#predictions
pred_train <- predict(svm_model,trainset)
mean(pred_train==trainset$y)
pred_test <- predict(svm_model,testset)
mean(pred_test==testset$y)

plot(svm_model,trainset)
#Not bad hey? Shows importance of picking the right kernel...but how do we do that when
#we don't know the shape of the decision boundary upfront???


