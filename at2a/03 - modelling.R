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

