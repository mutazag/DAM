
#### header ####
setwd("c:/mdsi/DAM/at2b")

library(dplyr)
library(readr)
library(ggplot2)
library(forcats)



df <- read_csv("./data/repurchase_training.csv", 
               col_types = cols(
                 Target = readr::col_factor(levels=NULL), 
                 age_band = readr::col_factor(levels=NULL), 
                 gender = readr::col_factor(levels=NULL), 
                 car_model = readr::col_factor(levels=NULL), 
                 car_segment = readr::col_factor(levels=NULL)
               ))

glimpse(df)
####  check for missing values ####
attach(df) 
summary(Target)     ## no missing values 
summary(age_band)   ## 112,375 missing values 
summary(gender)     ## 69,308 missing values
summary(car_model)  ## no missing values but model_19 appeared twice
summary(car_segment)## Other category appear 58 times 

detach(df)


####  factorise categorical variables ####
df$Target <- factor(df$Target, levels = c(1,0))
df$age_band <- ordered(df$age_band, 
                       levels = sort(as.character( evels(df$age_band))))
df$gender <- factor(df$gender, levels = c("Male", "Female", "NULL"))

df %>% mutate( repurchase = case_when(
  Target == 1 ~ "yes", 
  Target == 0 ~ "no"
)) -> dff
#### plotting categorical variables ####
dff %>% ggplot(aes(repurchase, age_band, color=gender)) + geom_jitter()


dff %>% ggplot(aes(gender, fill=repurchase)) + geom_bar()


dff %>% ggplot(aes(car_model, fill=repurchase)) + geom_bar()
dff %>% ggplot(aes(car_segment, fill=repurchase)) + geom_bar()

#### plotting numerical variables ####


dff %>% ggplot(aes(age_of_vehicle_years, fill=fct_rev( gender))) + geom_bar()
dff %>% ggplot(aes(age_of_vehicle_years, fill=fct_rev(age_band))) + geom_bar()
dff %>% ggplot(aes(age_of_vehicle_years, fill=repurchase)) + geom_bar()




