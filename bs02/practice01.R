## trying different things from DAM BS02 


library (ggplot2)


n <- 10
x <- seq(0,n,0.1)  ## create variable x
set.seed(42) 
y <- 10 + 0.5 * x  ## this is a perfectly linear relationship
ggplot(data_frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method="lm", se=F, col="red")

# change Y and add some random error
y <- y + rnorm(1+ n/0.1, 0, 1.25)
ggplot(data_frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method="lm", se=F, col="red")

# create Y with quadratic relaship with X and add random error 
y <- 9.96+ 0.97*x - 0.19*(x^2) + rnorm(1+ n/0.1, 0, 1.25)
ggplot(data_frame(x,y), aes(x,y)) + geom_point() + geom_smooth(method="loess", se=F, col="red")




# https://stats.stackexchange.com/questions/232548/r-how-are-the-significance-codes-determined-when-summarizing-a-logistic-regres

fit <- lm (y~x, data=data_frame(x,y))
summary(fit)


# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   12.88105    0.35557   36.23   <2e-16 ***
#   x           -0.88785    0.06143  -14.45   <2e-16 ***
### x appears to be very significant as it has a very low p-value 

# another way t look at importance is to use the verImp in caret library 
library(caret)
varImp(fit, scale=FALSE)
# the higher the value the more impact it has Overall 1 14.45215 this will
# probably make more sense when doing multiplw linear regresion