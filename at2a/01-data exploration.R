## 36106 - Data Algorithms and Meaning 
## Assignment 2 Part A: Linear Regression
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## First Look at the data: 
## this code will load the data, show some plots and save
## a summarised data set to file which will be used for further analysis in
## subsequent steps

## Library
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(scales)
library(RcppRoll) # used to calculate rolling mean
library(broom)


# load the transactions file and then factorise industry,location variables, and
# convert date variable to a Date data type

df <- read_csv("transactions.csv")

df$industry <- as.factor(df$industry)
df$location <- as.factor(df$location)
df$date <- dmy(df$date)
glimpse(df)


#### plotting function ####
## the following plotting functions are used to inspect the data set

plot_location <- function (df)
{
  print("plot1")
  df %>% 
    ggplot(aes(x=location, y=monthly_mean)) + 
    geom_point(position = "jitter", aes(color=industry)) +
    scale_y_continuous(labels = scales::comma) + 
    stat_summary(fun.ymin=median, fun.ymax=median, fun.y=median, geom="crossbar") +
    labs(title = "Mean Monthly Sales Distribution", 
         subtitle ="for locations, color coded by industry", 
         caption = "... with jitter", 
         x = 'Location Code', 
         y = 'Mean Monthly Sales ($)', 
         colour = "Industry"
    )
}

plot_industry <- function (df)
{
  print ("plot2")
  df %>% 
    ggplot(aes(x=industry, y=monthly_mean)) + 
    geom_point(position = "jitter", aes(color=location)) +
    scale_y_continuous(labels = scales::comma) + 
    stat_summary(fun.ymin=median, fun.ymax=median, fun.y=median, geom="crossbar") +
    labs(title = "Mean Monthly Sales Distribution", 
         subtitle = "for industry, color coded by location", 
         caption = "... with jitter", 
         x = 'Industry Code', 
         y = 'Mean Monthly Sales ($)', 
         colour = "location"
    )
}



#### Task 1 ####

# Create	an	aggregated	data	set	of	the	fields	date, industry
# and	location,	with	a	mean of	monthly_amount

df_agg <- df %>% 
  group_by(date,industry, location) %>%
  summarise(monthly_mean = mean(monthly_amount)) %>% 
  ungroup()

# The following plots show the distribution of monthly_mean for industry and
# location. The plots highlight that there are differences in industries 6,10
# and locations 1,8 where mean monthly sales observations for the combinations
# of those industry/locations are further spread than in other industry/location
# groups and have much higher values
df_agg %>% plot_industry()
df_agg %>% plot_location()


# save the aggregates data set to file to use in later tasks
write_csv(df_agg, "./transactions_agg.csv")