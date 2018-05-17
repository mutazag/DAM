## 36106 - Data Algorithms and Meaning 
## Assignment 2 Part A: Linear Regression
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Create a data set with all the features to be used in later modelling and
## testing

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
## load the summarised transactions file 
df_agg <- read_csv("./transactions_agg.csv", 
                   col_types = list(
                     readr::col_date(format=""), 
                     readr::col_factor(levels = NULL),
                     readr::col_factor(levels = NULL), 
                     readr::col_double()))
#### functions ####

# this function will calculating lagged mean for 3 and 6 months for a given observation
## x is the observation out of the features data set
## hh is full history 
calc_m3_m6 <- function (x, hh){

  # history timeseries for on location/industry combo
  hts <- df_features %>% filter(location==x$location, industry==x$industry) %>% arrange(desc(date))
  
  # find location of observation in history ts
  i <- which(hts$date == x$date) 
  
  # calc men for past 3 and 6 values 
  hts %>% filter(row_number() > i ) %>% head(3) %>% 
    summarise(m3 = mean(monthly_mean)) -> m3
  hts %>% filter(row_number() > i ) %>% head(6) %>% 
    summarise(m6 = mean(monthly_mean)) -> m6
  
  # test for NaN or NA 
  if (is.na(m3)) { x$m3 = x$monthly_mean } else {x$m3 = as.double(m3)}
  if (is.na(m6)) { x$m6 = x$monthly_mean } else {x$m6 = as.double(m6)}
  # print(x)
  return(x)
}


#### features ####
# features: 
# month(categorical), monthn(numerical), using both types to different
#                                        fitting with different options on month
# year(numerical) 
# lagged features for mean of past 3 and 6 months


#### month and year features ####
df_features <- df_agg %>% 
  mutate( year = year(date), 
          month = factor(month(date)),
          monthn = month(date)
  )

#### lagged features #### 
# initialise empty m3 and m6 features 
df_features$m3 <- NA
df_features$m6 <- NA

# calculate m3 and m6 on all df_feautres, this step will take a minute 
for(i in 1:nrow(df_features)){
  x <- df_features[i,]
  df_features[i,] <- calc_m3_m6(x, df_features)
}


df_features %>% filter(industry==1, location==1) %>% 
  ggplot(aes(x=m3, y=monthly_mean)) + geom_point() + geom_smooth(method="lm")


df_features %>% filter(industry==1, location==1) %>% 
  ggplot(aes(x=date, y=m3)) + geom_line() 

df_features %>% filter(industry==1, location==1) %>% 
  ggplot(aes(x=date, y=monthly_mean)) + geom_line() 

#### save featurised data set to file ####
# save the featurised data set for use in modelling
write_csv(df_features, "./transactions_features.csv")
