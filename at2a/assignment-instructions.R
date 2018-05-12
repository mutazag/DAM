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
df$date <- dmy(df$date)
glimpse(df)

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

plot_monthly <- function(df_agg, i=1, l=1, showlm = FALSE)
{
  print("industry ~ location plot")
  df_filter <- df_agg %>% filter(industry %in% i, location %in% l)
  
  p <- df_filter %>% 
    ggplot(aes(x=date, y=monthly_mean)) + 
    geom_line() + 
    facet_grid(industry ~ location,  labeller = label_both ) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Mean Monthly Sales over Date", 
         caption = paste0("Industry: ", i, ". Location: ", l),
         x = "Date", 
         y = "Mean Monthly Sales ($)")
  
  if (showlm == TRUE) {
    p <- p + geom_smooth(method = "lm", se = FALSE)
  }
  
  p
}

plot_ym <- function(df_agg_ym, i=1, l=1, showlm = FALSE)
{
  print("monthly on year by year")
  df_filter <- df_agg_ym %>% filter(industry %in% i, location %in% l)
  
  p <- df_filter %>% 
    ggplot(aes(x=m, y=monthly_mean, color=factor(y))) + 
    geom_line()  +
    scale_x_continuous(breaks = 1:12, labels = month.name, minor_breaks = NULL) +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle=90)) + 
    labs(title = "Mean Monthly Sales over Date", 
         subtitle = "Month View by Year", 
         caption = paste0("Industry: ", i, ". Location: ", l), 
         x = "Date", 
         y = "Mean Monthly Sales ($)", 
         colour = "Year")
  
  if (showlm == TRUE) {
    p <- p + geom_smooth(method = "lm", se = FALSE)
  }
  
  p
}

#### Task 1 ####

# Create	an	aggregated	data	set	of	the	fields	date, industry
# and	location,	with	a	mean of	monthly_amount

df_agg <- df %>% 
  group_by(date,industry, location) %>%
  summarise(monthly_mean = mean(monthly_amount)) %>% 
  ungroup()

df_agg %>% plot_industry()
df_agg %>% plot_location()

#### Task 2 - location 1 industry 1 line plot ####

# Create	a	line	plot	of	the	variable	monthly_amount
# for	industry	=	1	and	location	=	1.
# Note	the	seasonality	by	month	in	this	time	series

df_agg1 <- df_agg %>% filter(industry == 1, location == 1)

df_agg %>% plot_monthly(1,1,showlm = T)
# we can notice peaks and troughts in this chart for industry = 1/location = 1.
# Can we observe a discernible patter on a month by month basis? for thati will
# plot the same filtered data set but facet by year however each peak seems to
# be slighty higher than previous ones, and similarly each trough also follows a
# similar positive trend, which implies an global positive trend through out the
# years


df_agg_ym <- df_agg %>% 
  mutate(m = month(date), y = year(date))

df_agg_ym %>% plot_ym(1,1,T)
#### Task 3 - lm #### 

# For	industry =	1	and	location	=	1,	train a	linear	regression	model
# with	monthly_amount as	the	target.
# Remember	that	time	is	very	important	in	this	model,
# so	be	sure	to	include	a	variable	for	the	time	sequence	(this	can	simply	be	a	1	for	the
# first	month,	2	for	the	second	month,	etc.)

# first will create a time variable for each month 

df_agg_f <- df_agg %>% mutate(month = month(date), year = year(date))

# need to create lagged features before fitting, but will do later 

# fitting an lm model 

mod <-  lm(data = df_agg_f, formula = monthly_mean ~ year + month + industry + location)
plot(mod)


#### Notes ####

# summary(df$monthly_amount)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0     95323    179399    395 397    375439 100000000 
# the range (skewness is pretty high) 