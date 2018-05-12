setwd("C:/mdsi/dam/at2a")


# library(tidyverse)
# detach(pacakge:tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(scales)



df <- read_csv("transactions.csv")

df$industry <- as.factor(df$industry)
df$location <- as.factor(df$location)


#### plotting function ####

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
  group_by(date, industry, location) %>%
  summarise(monthly_mean = mean(monthly_amount))

df_agg %>% plot_industry()
df_agg %>% plot_location()
  
df_agg1 <- df_agg %>% filter(industry == 1, location == 1)

df_agg1 %>%  ggplot(aes(x=industry, y=monthly_mean)) + 
  geom_boxplot() +
  labs(title = "Mean Monthly Sales Distribution", 
       subtitle = "for industry, color coded by location", 
       caption = "... with jitter", 
       x = 'Industry Code', 
       y = 'Mean Monthly Sales ($)', 
       colour = "location")s
df_agg1 %>% plot_location()

#### Notes ####

# summary(df$monthly_amount)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0     95323    179399    395 397    375439 100000000 
# the range (skewness is pretty high) 