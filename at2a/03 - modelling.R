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


## load the summarised transactions file 
df_agg <- read_csv("./transactions_agg.csv", 
                   col_types = list(
                     readr::col_date(format=""), 
                     readr::col_factor(levels = NULL),
                     readr::col_factor(levels = NULL), 
                     readr::col_double()))



#### Task 3 - lm #### 

# For	industry =	1	and	location	=	1,	train a	linear	regression	model
# with	monthly_amount as	the	target.
# Remember	that	time	is	very	important	in	this	model,
# so	be	sure	to	include	a	variable	for	the	time	sequence	(this	can	simply	be	a	1	for	the
# first	month,	2	for	the	second	month,	etc.)


# process to following
# 1. select features and feature engineer
# 2. split the data into training and tesing data 
# 3. create the model using lm() (evaluate different features)
# 4. predict out of sample outcome 


#### 1. feature engineering ####

# features: 
# month(categorical and categorical), 
# year(numerical) 
# lagged features for mean of past 3 and 6 months


df_features <- df_agg %>% 
  mutate( year = year(date), 
          month = factor(month(date)),
          monthn = month(date)
          )



#### model testing with different features ####

fit_model <- function (df, formula, ind=1, loc=1){
  df_subset <- df %>% filter(industry==1, location==1)
  
  mod <- lm (data = df_subset, formula = formula)
  mod.r2 <- summary(mod)$r.sq
  mod.rse <- summary(mod)$sigma
  mod.Ymean <- mod %>% augment()
  #inspect RSE, Adj R-squared
  print(paste("RSE:", mod.rse))
  print(paste("Adj R-sqr:", mod.r2)) 
}

#### mod1: year + month ####
fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month)


fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + monthn)


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

# create a prediciton out of sample
predict(mod1, 
        data.frame(year=2016, month=factor(12)), 
        interval = "confidence")

predict(mod1, 
        data.frame(year=2016, month=factor(12)), 
        interval = "prediction")