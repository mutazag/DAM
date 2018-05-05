
###############################################################################
## Lecture 2 Exercise 1
#  Linear Regression
###############################################################################

# This exercise involves training a linear regression model
# We will use the Carseats data set in the ISLR package

# Load the ISLR package to memory.  If you haven't installed already, you need to run:
# install.packages("ISLR)
library(ISLR)

# Assign the Carseats data to a data frame called 'data' for convenience
data = Carseats
# look at the structure of the dataset
str(data)

# plot all the variables against data, four plots in a grid
# You might want to take note of this code, it can come in handy!
par(mfrow = c(2,2))
counter = 0
for (variable in colnames(data[, -1])) {
  plot(data[, variable], data$Sales, main = variable, ylab = "sales", xlab = variable)
  counter = counter + 1
  if (counter %% 4 == 0) {
    readline(prompt = "Hit ENTER to show more plots")
  }
}

# Train a linear regression model on all the predictors (using '.' in the formula does this)
data.lm = lm(formula = Sales ~., data = data)
# Analyse the model output, take note of the p-values and Adjusted R-squared
summary(data.lm)
# plot the regression diagnostics.  Note the red line on two left plots is not straight
plot(data.lm)

###############################################################################
# TASK:  Try and improve the model data.lm by removing some variables and adding interaction terms
###############################################################################

# GOAL:    It's possible to get to 0.8706 Adjusted R-squared
# HINT 1:  You create an interaction term in lm, you can add a term to the formula: X1:X2
# HINT 2:  Sometimes you can keep a predictor if it has p-value > 0.05 if it raises the R^2
# HINT:    The variable 'Population' has a high p-value.  Dropping it improves the model.  
#          You can start from here:

data.lm2 = lm(formula = Sales ~ CompPrice + Income + Advertising + Price +
                ShelveLoc + Age + Education + Urban + US, 
              data = data)
summary(data.lm2)
plot(data.lm2)


