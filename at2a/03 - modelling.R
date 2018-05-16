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

setwd("c:/mdsi/dam/at2a")


#### Task 3 - lm #### 

# For	industry =	1	and	location	=	1,	train a	linear	regression	model
# with	monthly_amount as	the	target.
# Remember	that	time	is	very	important	in	this	model,
# so	be	sure	to	include	a	variable	for	the	time	sequence	(this	can	simply	be	a	1	for	the
# first	month,	2	for	the	second	month,	etc.)


# Process:
# 1. select features and feature engineer
# 2. split the data into training and tesing data 
# 3. create the model using lm() (evaluate different features)
# 4. predict out of sample outcome 

# Note: this code file represents the final outcome after experminting with
# different options for feature engineering, fit forumla, modeling functoins (lm
# and glment).

#### 1. feature engineering ####

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


#### 2. data split for training and testing ####


#### 3. create the model using lm() (evaluate different features) ####

fit_model <- function (df, formula, ind=1, loc=1){
  df_subset <- df %>% filter(industry==ind, location==loc)
  
  mod <- lm (data = df_subset, formula = formula)
  mod.r2 <- summary(mod)$adj.r.squared
  mod.rse <- summary(mod)$sigma
  mod.Ymean <- mod %>% augment()
  #inspect RSE, Adj R-squared
  print(formula)
  print(paste("RSE:", mod.rse))
  print(paste("Adj R-sqr:", mod.r2)) 
  return(mod)
}

#### mod1: year + month ####
fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month)


fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + monthn)

fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month + m3+m6) -> mod
fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + monthn + m3+m6) -> mod
summary(mod)$coefficients
summary(mod)
# Residual standard error: 6844 on 34 degrees of freedom
# Multiple R-squared:  0.8443,	Adjusted R-squared:  0.7893 

# mean of Y = 166867
df_features %>% filter(location==1, industry==1) %>% summarise(ymean = mean(monthly_mean))

#RSE meadured in units Y indicates that prediction is likely to be off by about
#$6844, this is ~ 4% of mean of Y ($166,867)
#Risduals vs fitted plot shows a curve of the error in fitted values

mod1.r2 <- summary(mod1)$r.sq
mod1.rse <- summary(mod1)$sigma

# names(mod1)


#### 4. predict out of sample outcome ####
# create a prediciton out of sample
predict(mod1, 
        data.frame(year=2016, month=factor(12)), 
        interval = "confidence")

predict(mod1, 
        data.frame(year=2016, month=factor(12)), 
        interval = "prediction")
