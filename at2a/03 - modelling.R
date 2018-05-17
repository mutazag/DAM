## 36106 - Data Algorithms and Meaning 
## Assignment 2 Part A: Linear Regression
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## linear model for location 1 industry 1

## Library
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(scales)
library(RcppRoll) # used to calculate rolling mean
library(broom)
library(caret)

setwd("c:/mdsi/dam/at2a")


#### Task 3 - lm #### 

# For	industry =	1	and	location	=	1,	train a	linear	regression	model
# with	monthly_amount as	the	target.
# Remember	that	time	is	very	important	in	this	model,
# so	be	sure	to	include	a	variable	for	the	time	sequence	(this	can	simply	be	a	1	for	the
# first	month,	2	for	the	second	month,	etc.)



# Note: this code file represents trials and final outcome after experminting with
# different options for feature engineering, fit forumla

#### 1. feature engineering: load featurised data set ####

# load the prepared  feature engineered transaction file 
df_features <- read_csv("./transactions_features.csv", 
                        col_types = list(
                          readr::col_date(format=""), 
                          readr::col_factor(levels = NULL),
                          readr::col_factor(levels = NULL), 
                          readr::col_double(), 
                          readr::col_integer(), 
                          readr::col_factor(levels=NULL), 
                          readr::col_integer(), 
                          readr::col_double(), 
                          readr::col_double()
                        ))



#### functions ####


# print RMSE, RSE and AdjR2 for model
model_summary <- function(mod){
  mod_summary <- list()
  
  mod_summary$r2 <- summary(mod)$adj.r.squared
  mod_summary$rse <- summary(mod)$sigma
  mod_summary$aug <- mod %>% augment()
  # calculate RMSE
  mod_summary$RMSE <- sqrt(mean(mod_summary$aug$.resid ^ 2))
  
  #inspect RSE, Adj R-squared
  # 
  # sprintf("RMSE: %0.3f", mod_summary$RMSE)
  # sprintf("RSE: %0.4f", mod_summary$rse)
  # sprintf("Adj R-sqr: %0.4f", mod_summary$r2) 
  print(paste0("Adj R-sqr: ", mod_summary$r2))
  print(paste0("RMSE: ", mod_summary$RMSE)) # or RMSE(pred = mod1$fitted.values, obs = mod1$model$monthly_mean)
  print(paste0("RSE: ", mod_summary$rse))
  
  
}


# fit model for based on formula for a given industry amd location 
fit_model <- function (df, formula, ind=1, loc=1){
  df_subset <- df %>% filter(industry==ind, location==loc)
  mod <- lm (data = df_subset, formula = formula)
  
  
  print(formula)
  model_summary(mod)
  return(mod)
}


# cross validate model with out-ofsample and print average out-of-sample RMSE 
fit_model_cv <- function (df, formula, ind=1, loc=1){
  df_subset <- df %>% filter(industry==ind, location==loc)
  trControl <- trainControl(method = "cv",number = 15, verboseIter = FALSE)
  mod <- train(formula, df_subset, method = "lm", trControl = trControl)
  
  print(formula)
  print("cross validation")
  print(mod$results)
  print("final model")
  model_summary(mod$finalModel)
  return(mod)
}


#### 3. create the model using lm() (evaluate different features) ####

# model and compare with different features/formulas ####

df_features %>% filter(industry==1, location==1) %>% summarise(mean = mean(monthly_mean))
# mod1: year and month (categorical)
fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month) -> mod1

#mod2: year and monthn (numerical)
fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + monthn) -> mod2

#mod3: year, month and lagged fetures (m3 and m6)
fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month + m3+m6) -> mod3


#### cross validation ####
#dropped mod2 and only cross validaing mod1 and mod3 formulas, with and without lagged features
fit_model_cv(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month) -> mod1.cv
fit_model_cv(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month + m3+m6) -> mod3.cv


#### predict december 2016
#create a prediciton out of sample
predict(mod1, 
        data.frame(year=2016, month=factor(12)))


