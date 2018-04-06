# chapter 2 - excercise 10 

library(MASS)
head(Boston)

# crim  per capita crime rate by town.
# zn   proportion of residential land zoned for lots over 25,000 sq.ft.
# indus proportion of non-retail business acres per town.
# chas  Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox  nitrogen oxides concentration (parts per 10 million).
# rm    average number of rooms per dwelling.
# age  proportion of owner-occupied units built prior to 1940.
# dis    weighted mean of distances to five Boston employment centres.
# rad  index of accessibility to radial highways.
# tax full-value property-tax rate per \$10,000.
# ptratio  pupil-teacher ratio by town.
# black   1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat   lower status of the population (percent).
# medv  median value of owner-occupied homes in \$1000s.

dim(Boston)
# 506 rows, 14 cols
# houses values in suburbs of boston

# (b)
b <- Boston
attach(b)
plot ( ~ crim + rm + lstat + medv)

# (c)
plot(crim, medv)
cor(crim,medv)


# (d)
plot(~ crim + tax + ptratio )
boxplot(crim)
boxplot(tax)
boxplot(ptratio)
sd(crim)
sd(tax)
sd(ptratio)

# (e) 
nrow(b[ chas == 1,])

# (f) 
mean(ptratio)

#(g)
boxplot(medv)
mean(medv)
range(medv)

# (h)
nrow(b[rm > 7, ])
nrow(b[rm > 8, ])

b_8 <- b[rm> 8,]
plot(b_8)
boxplot(b_8)
plot( ~ medv +black, b_8)
