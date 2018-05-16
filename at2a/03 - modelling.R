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
fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + month) -> mod


fit_model(df_features, ind=1,loc=1, formula = monthly_mean ~ year + monthn) -> mod

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

#### fitting all industry and locations ####

# model per combo
combos <- df_features %>% select(industry,location) %>% distinct()
combos <- combos  %>%filter(industry==10)
for (i in 1:nrow(combos)) {
  c <- combos[i,]
  print(c)
  mod <- fit_model(df_features, ind = c$industry, loc=c$location, 
            formula = monthly_mean ~ year + month)
  mod
}

#this has produced models where R2 is nan industry 10,locaiton 3 since there is
#no year data, this specific example is not a linear model since there is
#exactly 12 outcomes one for each month in the data set 

# other proeuced negative R2, R2 is negative only when the chosen model does not
# follow the trend of the data, so fits worse than a horizontal line. this
# indicates a poorly chosen model a sign of using wrong formula
# example is industry 10, locaiton 9 with formula monthly_mean ~ year + month
fit_model(df_features, ind = 10, loc =9, formula = monthly_mean ~ year + month)
df_features %>% filter(industry==10,location==9) %>% ggplot(aes(x=date, y=monthly_mean)) + 
  geom_line() + geom_smooth(method="lm")
#fitting the same with different formula
fit_model(df_features, ind = 10, loc =9, formula = monthly_mean ~ year )
fit_model(df_features, ind = 10, loc =9, formula = monthly_mean ~ month)
fit_model(df_features, ind = 10, loc =9, formula = monthly_mean ~ m6)






## fitting a model with i and l as predictors

lm(df_features, formula = monthly_mean ~ industry + location + monthn + year) %>% summary()
mean(df_features$monthly_mean)
# produced an acceptable R 2 of 0.68 but the RSE is 2.261 mil which is pretty
# high considering that the mean of y is 1.014 mil



#### 4. predict out of sample outcome #### 


#create a prediciton out of sample
predict(mod1, 
        data.frame(year=2016, month=factor(12)), 
        interval = "confidence")

predict(mod1, 
        data.frame(year=2016, month=factor(12)), 
        interval = "prediction")
