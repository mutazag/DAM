## 36106 - Data Algorithms and Meaning 
## Assignment 2 Part A: Linear Regression
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## Exploring the time series for industry 1, location 1

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


#### plotting function ####
## the following plotting functions are used to inspect the data set


# plot a line chart by month for selected industry/location combinations
plot_monthly <- function(df_agg, i=1, l=1, showlm = FALSE)
{
  print("industry ~ location plot")
  df_filter <- df_agg %>% filter(industry %in% i, location %in% l)
  
  p <- df_filter %>% 
    ggplot(aes(x=date, y=monthly_mean)) + 
    geom_line() + 
    facet_grid(industry ~ location,  labeller = label_both ) +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle=90)) + 
    labs(title = "Mean Monthly Sales over Date", 
         caption = paste0("Industry: ", paste(i, collapse = ","),
                          "\n. Location: ", paste(l, collapse = ",")),
         x = "Date", 
         y = "Mean Monthly Sales ($)")
  
  if (showlm == TRUE) {
    p <- p + geom_smooth(method = "lm", se = FALSE)
  }
  
  p
}


#### Task 2 - location 1 industry 1 line plot ####

# Create	a	line	plot	of	the	variable	monthly_amount
# for	industry	=	1	and	location	=	1.
# Note	the	seasonality	by	month	in	this	time	series


# plot line chart over time for industry,location == 1,1
df_agg %>% plot_monthly(i=1,l=1,showlm = F)


# we  notice peaks and troughts in this chart for industry = 1/location = 1.
# Can we observe a discernible patter on a month by month basis? for thati will
# plot the same filtered data set but facet by year however each peak seems to
# be slighty higher than previous ones, and similarly each trough also follows a
# similar positive trend, which implies an global positive trend through out the
# years


