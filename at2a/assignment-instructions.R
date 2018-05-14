setwd("C:/mdsi/dam/at2a")


# library(tidyverse)
# detach(pacakge:tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(scales)
library(RcppRoll) # used to calculate rolling mean



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
  print("month by month for each year ")
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

plot_my <- function(df_agg_ym, i=1, l=1, showlm = FALSE)
{
  print("monthly on year by year")
  df_filter <- df_agg_ym %>% filter(industry %in% i, location %in% l, m==12)
  
  p <- df_filter %>% 
    ggplot(aes(x=y, y=monthly_mean, color=factor(m))) + 
    geom_line() +
    scale_x_continuous(breaks = 1:12, minor_breaks = NULL) +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle=90)) + 
    labs(title = "Mean Monthly Sales over Date", 
         subtitle = "Month YoY", 
         caption = paste0("Industry: ", i, ". Location: ", l), 
         x = "Year", 
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

# two plots here not required by task 
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

df_agg_ym %>% plot_ym(1,1,F)
df_agg_ym %>% plot_my(1,1,F)



#### Task 3 - lm #### 

# For	industry =	1	and	location	=	1,	train a	linear	regression	model
# with	monthly_amount as	the	target.
# Remember	that	time	is	very	important	in	this	model,
# so	be	sure	to	include	a	variable	for	the	time	sequence	(this	can	simply	be	a	1	for	the
# first	month,	2	for	the	second	month,	etc.)

# first will create a time variable for each month 

df_agg_f <- df_agg %>% mutate(month = as.integer(month(date)), year = as.integer(year(date)))

# need to create lagged features before fitting, but will do later 

# fitting an lm model 

# mod <-  lm(data = df_agg_f, formula = monthly_mean ~ year + month + industry + location)
# select  only industry 1 location 1 
df_agg_f <- filter(df_agg_f, industry == 1, location == 1)
mod <- lm(data= df_agg_f, formula = monthly_mean ~ month + year )
plot(mod)

# using broom::augment()
mod %>% augment() %>% filter(row_number()==1)
# this will show the outcome and fitted vaule, all features, and error measures 

# values with high cooksd have high influence! is this useful in the context
# given the the value is not an outlier!
mod %>% augment() %>% ggplot(aes(x=monthly_mean, y=.cooksd)) + geom_point()
# fitte coeefcients 
coef(mod)

# two ways to look at fitted values 
data.frame(f1=fitted.values(mod)) -> fitted1
mod %>% augment() %>% select(f2 = .fitted, .resid) -> fitted2
data.frame(fitted1, fitted2) %>% mutate(r = f2 - f1)
fitted1[1,] - fitted2[1,]
#### todo on lm model ####
# split to train and test data 

# compare prediction find error 

# quantiative assessment of error 

# predict december 2016 for this industry/location 


# the above peice of code will need to repeated for each industry/location
# combination, that is 100 times, beware of missling industry/locaiton combinations. 


#### Notes ####

# summary(df$monthly_amount)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0     95323    179399    395 397    375439 100000000 
# the range (skewness is pretty high) 


#### lagged mean example ####
d <- data_frame(x=c(23,332,12,123,22,1231))
d <- data_frame(x = d$x, x1 = roll_meanr(d$x, n=3))
d %>% filter(is.na(x1)) %>% mutate(x1 = roll_meanr(x, n=2 )) -> d[is.na(d$x1),]
d %>% filter(is.na(x1)) %>% mutate(x1 = roll_meanr(x, n=1 )) -> d[is.na(d$x1),]
d

#prepare a NA variable for lagging value 
df_agg_f$mean3 <- NA 
df_agg_f %>% select(date, monthly_mean, mean3)  %>% head()

# calc lagging value for the first time using 3 points 
df_agg_f %>% 
  filter(is.na(mean3)) %>% 
  mutate(mean3 = roll_meanr(monthly_mean, n=3 )) -> df_agg_f[is.na(df_agg_f$mean3),]

# then 2 points to fill NAs
df_agg_f %>% 
  filter(is.na(mean3)) %>% 
  mutate(mean3 = roll_meanr(monthly_mean, n=2 )) -> df_agg_f[is.na(df_agg_f$mean3),]

# then 1 point for final one
df_agg_f %>% 
  filter(is.na(mean3)) %>% 
  mutate(mean3 = roll_meanr(monthly_mean, n=1 )) -> df_agg_f[is.na(df_agg_f$mean3),]

df_agg_f %>% ggplot(aes(x=mean3, y=monthly_mean)) + geom_point() + geom_smooth(method="lm")
df_agg_f %>% ggplot(aes(x=date, y=mean3)) + geom_line()  + geom_smooth(method="lm")
df_agg_f %>% ggplot(aes(x=date, y=monthly_mean)) + geom_line()  + geom_smooth(method="lm")


# lm with mean 3 (with year as numerical and then as factor)
mod2 <- lm(data= df_agg_f, formula = monthly_mean ~ month + year + mean3)
df_agg_f$yf <- as.factor(df_agg_f$year)
mod3 <- lm(data= df_agg_f, formula = monthly_mean ~ month + yf + mean3)


# functionalise the process of creating lagged features
# this will create a feature for mean over 6 previous months
df_agg_f$mean6 <- NA 
for (i in 6:1) {
  df_agg_f %>% 
    filter(is.na(mean6)) %>% 
    mutate(mean6 = roll_meanr(monthly_mean, n=i )) -> df_agg_f[is.na(df_agg_f$mean6),]
}
df_agg_f %>% select(date, monthly_mean, mean3, mean6)  %>% head(20)
df_agg_f[1,] %>%  summarise(mean(monthly_mean))

mod4 <- lm(data= df_agg_f, formula = monthly_mean ~  month + yf + mean3 + mean6)
mod5 <- lm(data= df_agg_f, formula = monthly_mean ~  month + year + mean3 + mean6)

# the following lines show if there is a linear relation between predictor and targets 
df_agg_f %>% ggplot(aes(x=mean6, y=monthly_mean)) + geom_point() + geom_smooth(method="lm")
df_agg_f %>% ggplot(aes(x=mean3, y=monthly_mean)) + geom_point() + geom_smooth(method="lm")
df_agg_f %>% ggplot(aes(x=month, y=monthly_mean)) + geom_point() + geom_smooth(method="lm")

# the follwing two lines show less variablity arond the mean for mean6 over
# monthly mean, but using mean6 as an outome means that we will need to umean
# the result to get the monthly value
df_agg_f %>% ggplot(aes(x=date, y=mean6)) + geom_line()  + geom_smooth(method="lm")
df_agg_f %>% ggplot(aes(x=date, y=monthly_mean)) + geom_line()  + geom_smooth(method="lm")

# shall we use dummies for categorical variables like year 
library(dummies)
cbind(df_agg_f, dummy(df_agg_f$yf)) -> df_dummies
df_dummies %>% select( -date, -industry, -location, -yf, -year) %>%  
  lm(formula = monthly_mean ~ .)  -> mod4_dummies 
# mod4 dummies R2adj -> 0.6499
# mod4 R2adj -> 0.6499
# no difference as lm takes care of factor predictors 

## test for coliearity 
pairs(df_agg_f) # we see colinearity between mean3 and mean5, and 
cor(df_agg_f%>% select(mean3, mean6), method="pearson")

#           mean3     mean6
# mean3 1.0000000 0.9392523
# mean6 0.9392523 1.0000000
cor(df_agg_f%>% select(mean3, mean6), method="spearman")
#           mean3     mean6
# mean3 1.0000000 0.9118871
# mean6 0.9118871 1.0000000

# high colinearity between mean3 and mean6 -> regulisation is required , or try lasso regression 
#

#### lasso #### lasso (least absolute shrinkage and selection operator) (also
#Lasso or LASSO) is a regression analysis method that performs both variable
#selection and regularization in order to enhance the prediction accuracy and
#interpretability of the statistical model it produces.
# https://en.wikipedia.org/wiki/Lasso_(statistics)
# https://www.r-bloggers.com/ridge-regression-and-the-lasso/

library(glmnet)
library(caret)

# create training sub set -- other method is k-folds
set.seed(42)
train <- createDataPartition(y = df_agg_f$monthly_mean, p = 0.7, list = F)
training <- df_agg_f[train, ] %>% select(-date, -industry, -location, -year)
testing = df_agg_f[-train, ] %>% select(-date, -industry, -location, -year)
# prepare x and y for glment 
x = model.matrix(~ ., training %>% select(-monthly_mean))
y = training$monthly_mean

# set alpha to 1 for lasso
# cv.glmnet will fit based on best lambda by cross validation (as opposed to just glmnet)
cv.fit_lasso = cv.glmnet(x, y,  alpha = 1) # alpha 1 == lasso 
# glmnet(x, y,  alpha = 1) -> gg
# predict(gg, xtest, s = min(gg$lambda))

#look at results of lasso
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min # best lambda
cv.fit_lasso$lambda.1se

coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min) 

# it shows that it reduces mean6 coeff when compare with mod4

# no predict on testing data and calculate MSE = mean( (ypred - ytest)^2 )
xtest <- model.matrix(~ ., testing %>% select(-monthly_mean))
ytest <- testing$monthly_mean
ypred <- predict(cv.fit_lasso$glmnet.fit, newx = xtest, s = cv.fit_lasso$lambda.min)
ypred <- predict(cv.fit_lasso$glmnet.fit, newx = xtest, s = 7.389056)
# predect based on test 
lasso_MSE = mean ( (ypred - ytest)^2)
lasso_MSE
#compare withmod 4
mod4 %>% augment() %>% select(ytest = monthly_mean, ypred = .fitted) -> mod4_pred
mod4_pred %>% summarise( mean( (ypred-ytest)^2)) -> mod4_MSE
mod4_MSE
lasso_MSE < mod4_MSE
# 115263837 <66247298
#### Task 4 - repeat lm process for all industry/month combinations and predict decemebt 2016 for all of them ####