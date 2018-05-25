
###############################################################################
## Lecture 2 Exercise 4
#  Principal Components Analysis and Regression
###############################################################################

# In this exercise we are going to run a Principal Components Analysis (PCA), 
# and train a Principal Components Regression (PCR)
# We will use two new data sets, USArrests and Hitters

library(ISLR)
# we will use the pls package for Principal Components Regression
# install.packages("pls")
library(pls)

###########################################
# Principal Components Analysis exercise
###########################################

# We will look at the USArrests data set
head(USArrests)
# prcomp is a built in function that does a PCA 
# have a look at the documentation
?prcomp
pr_out = prcomp(USArrests, scale = T)
# The model has 5 output objects
names(pr_out)
# The 'rotation' output shows a view of the 'coefficients' of the principal components 
pr_out$rotation
# biplot outputs a plot of the first two principal components with the variables plotted
biplot(pr_out, scale=0)
 
# We will now create the scree plot by converting the standard deviation to a variance
pr_var = pr_out$sdev ^ 2
pve = pr_var / sum(pr_var)
# Scree plot
plot(pve, type = "b", main = "Scree Plot", 
     ylab = "Proportion of Variance Explained",
     xlab = "Principal Component")
# Plot of cumulative variance explained
plot(cumsum(pve), type = 'b', main = "Cumulative Variance Explained",
     xlab = "Cumulative Proportion of Variance Explained",
     ylab = "Number of Components")

# shows that PCA 2 to 3 explain between 90 to 95 of variance
###########################################
# Principal Components Regression exercise
###########################################

# The pcr function in the pls package does a Principal Components Regression
# Have a look at the documentation
?pcr
set.seed(1)
# train the model
pcr_fit = pcr(Salary ~ ., data = Hitters, scale = T, validation = "CV")
# The summary gives the % of variance explained, and a variation of MSE
summary(pcr_fit)
# Scree plot
validationplot(pcr_fit, val.type = "MSEP")
validationplot(pcr_fit, val.type = "RMSEP")
validationplot(pcr_fit, val.type = "R2")

summary(pcr_fit)
# To choose the number of components, we analyse the validation plot, 
# as well as the % variance explained in summary(pcr.fit)


## scree plot and cumulative variace on this data set 
# first calc the principle components for the set 
#### convert factors to numirics 
factor_cols <- Hitters[,sapply(Hitters, is.factor)]
sapply(Hitters, is.factor)
Hitters$League <- as.numeric(Hitters$League)
Hitters$Division <- as.numeric(Hitters$League)
Hitters$NewLeague <- as.numeric(Hitters$League)

hitters_prout <- prcomp(~. , data=Hitters, scale = T, na.action = na.omit)
pr_var = hitters_prout$sdev ^ 2
pve = pr_var / sum(pr_var)
# Scree plot
plot(pve, type = "b", main = "Scree Plot", 
     ylab = "Proportion of Variance Explained",
     xlab = "Principal Component")
# Plot of cumulative variance explained
plot(cumsum(pve), type = 'b', main = "Cumulative Variance Explained",
     xlab = "Cumulative Proportion of Variance Explained",
     ylab = "Number of Components")
# cumsum plot shows that 10 PCAs will explain around 95% of the variablity, more than enough 
# choose a number for the number of components
ncomp =  7
# fit model to predict salar 
pcr_fit_hitters <- pcr(Salary ~ ., data = Hitters, scale = T, validatiob = "CV")

# output predictions
pcr_pred_hitters = predict(pcr_fit_hitters, Hitters, ncomp = ncomp)  ## <- here apply nPCA

# Have a look at the relationship between predicted and actual
plot(Hitters$Salary, pcr_pred_hitters)

# scree plots 
validationplot(pcr_fit_hitters, val.type = "MSEP")
validationplot(pcr_fit_hitters, val.type = "RMSEP")
validationplot(pcr_fit_hitters, val.type = "R2")


# clac RMSE on prediciotion ncomp =7
caret::RMSE(pred = as.vector(pcr_pred_hitters), as.vector(Hitters$Salary), na.rm = T)
caret::R2(pred = as.vector(pcr_pred_hitters), as.vector(Hitters$Salary), na.rm = T)

# try again with low ncomp 
pcr_pred_hitters2 = predict(pcr_fit_hitters, Hitters, ncomp = 1) 
caret::RMSE(pred = as.vector(pcr_pred_hitters2), as.vector(Hitters$Salary), na.rm = T)
caret::R2(pred = as.vector(pcr_pred_hitters2), as.vector(Hitters$Salary), na.rm = T)

# with high pca 
# try again with low ncomp 
pcr_pred_hitters3 = predict(pcr_fit_hitters, Hitters, ncomp = 13) 
caret::RMSE(pred = as.vector(pcr_pred_hitters3), as.vector(Hitters$Salary), na.rm = T)
caret::R2(pred = as.vector(pcr_pred_hitters3), as.vector(Hitters$Salary), na.rm = T)
