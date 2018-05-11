# Part A involves training a regression model on a financial time series data
# set. The task is to produce accurate forecasts of future periods, accounting
# for the effects of seasonality, industry, location and trends. Time series
# techniques are not required for this task, just an understanding of linear
# regression models.
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
dim(df)
colnames(df)

glimpse(df)

# fix date data type 
df$date <- dmy(df$date)
range(df$date)
# 4464 unique customer IDs
length(unique(df$customer_id))

# industry and location are categorical variables
df$industry <- as.factor(df$industry)
df$location <- as.factor(df$location)
df %>% select(industry, location) %>% summary()

# look for number of observations per industry/location 
df %>%
  group_by(industry, location) %>%
  summarise(N = n()) %>% 
  spread(location, N) 


df %>%
  group_by(industry, location) %>%
  summarise(N = n()) %>% 
  ggplot(aes(x=location, y=industry)) + geom_point()

df %>% group_by(industry) %>% 
  summarize(industry_total = sum(monthly_amount), industry_mean = mean(monthly_amount)) %>%
  ggplot(aes(x=industry, y=industry_total)) + geom_bar(stat="identity") +
  scale_y_continuous(labels = comma) 
# labels = scientific requires scales lirbary , Possible values for labels are
# comma, percent, dollar and scientific. stat = identity to do a column plot
# separate(date, c("y", "m", "d"), remove = FALSE) %>%




df %>% group_by(location) %>% 
  summarize(location_total = sum(monthly_amount), location_mean = mean(monthly_amount)) %>%
  ggplot(aes(x=location, y=location_total)) + geom_bar(stat="identity") +
  scale_y_continuous(labels = comma) 
# labels = scientific requires scales lirbary , Possible values for labels are
# comma, percent, dollar and scientific. stat = identity to do a column plot
# separate(date, c("y", "m", "d"), remove = FALSE) %>%



df %>% group_by(location) %>% 
  summarize(industry_total = sum(monthly_amount), industry_mean = mean(monthly_amount)) %>%
  ggplot(aes(x=industry, y=industry_total)) + geom_bar(stat="identity") +
  scale_y_continuous(labels = comma) 

df_mean <- df %>%  group_by(date, industry, location) %>% summarize(monthly_mean = mean(monthly_amount)) %>% separate(date, c("y","m","d"), remove = F)
df_l1i1 <- df_mean %>% filter(location == 1 , industry == 1)
df_l1i1 %>% ggplot(aes(m, monthly_mean)) + geom_point()

head(df_l1i1, 20)
tail(df_l1i1, 20)
plot(df_l1i1$date, df_l1i1$monthly_mean)
month(df_l1i1$date)
range(df_l1i1$date)

df_l1i1

df_l1i1 %>% ggplot(aes(date, monthly_mean), group=1) + geom_line() +
  facet_grid(. ~y,scale="free", space="free", switch = "x") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  theme_minimal() + geom_smooth()
  
  theme(panel.margin = unit(0,"line"), strip.placement = "outside")
  
  
  
  
  
  
  
  # 
  # need to confirm if i need to use location and industry as features 
  # ARIMA if you only have the monthly sum but not other variables, will look into seasonality and -- ARIMA has a built in fuction to determine the number of lags to build in the features. ACF and PACF 
  # 
  # start with linear regression and then incorporate regulazation 
  # 
  # seasonality is incorporated by incorporating the month 
  # 
  # look at if we should normalise the features (0-1): z-scorting but will creating negateive and positive, or min-max will create positive normalised value 
  # 
  # missing **  lagged features - mean monthly sum for the past three months, on month by montg, but can also do lagged feature for same month last year 
  # 
  # 
  # 
  # other linear model like lasso (L1) 
  # 
  # trend: there is a montly pattern - that can be expressed using month features for that i will use lagged features but also there a global trend that we be observed with looking at the year, so will featurise the year -- not enough years to do lagged features for year. 
  # 
  # for boundary cases, like feb - where there is no three months before, i could repeat the values. 
  # 
  # 
  # 
  # 
  # Solution: Build an ARIMA model on the historic sales data, then use the learned model to generate forecasts. Model building consists of the following steps:
  #   1.      Calculate the ACF and PACF of the historic sales time series for a few time lags, e.g. 1, 2, 3, ..., 10. 
  # 2.      Based on the ACF and PACF values, choose candidate p, d, q values for a non-seasonal ARIMA. 
  # 3.      Estimate the parameters of the chosen ARIMA model. 
  # 4.      Examine the autocorrelation between residuals via the Ljung-Box test.  If residuals are correlated, adjust the p, d, q values, or try a seasonal ARIMA model until a satisfactory model is obtained.
  # 5.      Forecast future sales using the learned (S)ARIMA model. 
