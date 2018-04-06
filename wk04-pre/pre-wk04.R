# Week 4 pre excercise 

# simple funciton to square a nunber 
x_squared <- function(x) { 
  x_2 = x^2
  return(x_2)
}

numbers <- 1:10 
x_squared(numbers)

# return multiple variables in a dataframe 
moments <- function(x) 
{
  # mean of x 
  mean <- mean(x)
  
  # standard deviation of x 
  sd <- sd(x) 
  
  # rows 
  rows <- length(x) 
  
  moments <- data.frame(mean, sd, rows)
  return(moments)
}

moments(numbers)
moments(rnorm(50))

## aggregations 

# read diamonds
diamonds <- read.csv("diamonds.csv", header = TRUE)
head(diamonds)

str(diamonds)

# average carat for each cut  (left of ~ is the grouping factor) 
aggregate(formula = carat ~ cut, data = diamonds, FUN = mean)

aggregate(formula = cbind(carat, depth, price, x, y, z) ~ cut, data = diamonds, FUN = mean) 

aggr <- aggregate(formula = cbind(carat, depth, price, x, y, z) ~ cut + color + clarity, 
          data = diamonds, 
          FUN = mean) 

head(aggr)

# order the aggr
aggr_order <- order(aggr$carat, decreasing = TRUE)
head(aggr[aggr_order,])

# aggregsate by custom function 
aggregate(formula = carat ~ cut, 
          data = diamonds, 
          FUN = function(x) {
            return(c(mean = mean(x), sd=sd(x), rows = rows<-length(x)))
          })


### data visualisation

library(ggplot2)


qplot(x = carat, data = diamonds, geom = "auto", fill = clarity)

# density distribution , alpha is transperancy
qplot(x = carat, data = diamonds, geom = "density", fill = clarity, alpha= I(0.25))


# scatterplot - point diagram 
qplot(x = carat, y = price, data = diamonds, geom = "point", color = clarity, 
      main =  " plot carat against price by clarity")


# ggplot for customisations 
#   data 
#   aes - variables
#   geom - geom of the plot 

ggplot(data = diamonds, 
       aes(x = carat, y = price, color = clarity)) + geom_point()

# now overlay a trend line geom_smooth
ggplot(data = diamonds, 
       aes(x = carat, y = price, color = clarity)) + geom_point() + geom_smooth()

ggplot(data = diamonds, 
       aes(x = carat, y = price, color = clarity)) + geom_smooth()

# facet the plot by clarity, and color by cut 
ggplot(data = diamonds, 
       aes(x = carat, y = price, color = cut)) + geom_point() +
       facet_wrap( ~ clarity) + geom_smooth(method="auto") 
