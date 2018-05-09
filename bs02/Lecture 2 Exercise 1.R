
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
# par(mfrow = c(3,3))
counter = 0
for (variable in colnames(data[, -1])) {
  plot(data[, variable], data$Sales, main = variable, ylab = "sales", xlab = variable)
  counter = counter + 1
  if (counter %% 9 == 0) {
    readline(prompt = "Hit ENTER to show more plots")
  }
}




######
####
###
#
# modelling and plotting the modells
#
##
###
####

# par(mfrow = c(2,4))
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



# 1 took popuulation out, increast Adj R-sq to 0.8701 
data.lm2 = lm(formula = Sales ~ CompPrice + Income + Advertising + Price +
                ShelveLoc + Age + Education + Urban + US, 
              data = data)
summary(data.lm2)
plot(data.lm2)


# 2 discover interaction terms by checking corrlation between different terms
## http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software



library(dplyr)

drop_cols <- c("Urban", "US","ShelveLoc")
my_data <- data %>% select(-one_of(drop_cols)) %>% select(-one_of("Sales"))
corrs <- round(cor(my_data),2)


# install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))
corrplot(corrs, type="upper", order="hclust", tl.col="black", tl.srt=45)

# another way t look at importance is to use the verImp in caret library 
library(caret)

varImp(data.lm2, scale=FALSE)
# the higher the value the more impact it has Overall 1 14.45215 this will
# probably make more sense when doing multiplw linear regresion


# > corrs
#              CompPrice Income Advertising Population Price   Age Education
# CompPrice        1.00  -0.08       -0.02      -0.09  0.58 -0.10      0.03
# Income          -0.08   1.00        0.06      -0.01 -0.06  0.00     -0.06
# Advertising     -0.02   0.06        1.00       0.27  0.04  0.00     -0.03
# Population      -0.09  -0.01        0.27       1.00 -0.01 -0.04     -0.11
# Price            0.58  -0.06        0.04      -0.01  1.00 -0.10      0.01
# Age             -0.10   0.00        0.00      -0.04 -0.10  1.00      0.01
# Education        0.03  -0.06       -0.03      -0.11  0.01  0.01      1.00
#
# interaction terms: 
# price and comPrice : 0.58
# population and advertising <- to a lesser degree 0.27
# Age and Comprice <- -0.1, also Age and Price -0.1
# Education and population <- - 0.11

# try the interaction term of price and com price 

#create the interaction variable
# CompPrice
# Price charged by competitor at each location
# Price
# Price company charges for car seats at each site

#step 1: center the input variables
attach(data)
PRICEc <- Price - mean(Price)
COMPPRICEc <- CompPrice - mean(CompPrice)
#step 2: multiply the input variables
PriceComPrice <- Price * CompPrice

detach(data)
data3 <- data %>% mutate(PriceComPrice)

data.lm3.price_compprice = lm(formula = Sales ~ Income + Advertising + 
                Age + Price + CompPrice +  ShelveLoc, 
              data = data3)
summary(data.lm3.price_compprice)
plot(data.lm3.price_compprice)




# Adj R-sqr dropped to 0.87 <- not a good move 