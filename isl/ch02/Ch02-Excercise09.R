# Ch02 Excercise 09 - Auto data set

# load data set
# download from ISL
Auto <- read.table("http://www-bcf.usc.edu/~gareth/ISL/Auto.data", header = TRUE, na.strings = "?")
Auto <- na.omit(Auto)
nrow(Auto)


# (a) 
str(Auto)
quantitative <- c("mpg", "displacement", "horsepower", "weight", "acceleration", "year")
qualititave <- c("cylinders", "origin", "name")


Auto[,qualititave]<- lapply(Auto[,qualititave], as.factor)

# (b) 
# str(Auto)
lapply(Auto[,quantitative], range) # range for quantitative params

# (c) mean and standard deviation of quantitative params 

lapply(quantitative, function(col) { paste( col, "mean", mean(Auto[,col]), "- sd", sd(Auto[,col]))})

# (d) remove rows 10:85 and calc range, mean and sd
Auto_subset <- Auto[-c(10:85),]
lapply(Auto_subset[,quantitative], range) # range for quantitative params
lapply(quantitative, function(col) { paste( col, "mean", mean(Auto_subset[,col]), "- sd", sd(Auto_subset[,col]))})

# (e) scatterplot of quantitiative params
attach(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration + year)
detach(Auto)
# (f) predict mpg using displacement horsepower, weight and acceleration 
attach(Auto)
par(mfrow = c(3,2))
plot(mpg, displacement, main = paste("cor", cor(mpg, displacement)))
plot(mpg, horsepower, main = paste("cor", cor(mpg, horsepower)))
plot(mpg, weight, main = paste("cor", cor(mpg, weight)))
plot(mpg, acceleration, main = paste("cor", cor(mpg, acceleration))) # week postivie cor
# plot(as.factor(year), mpg, main = paste("cor", cor(mpg, year)))
boxplot(mpg ~ year, main = paste("cor", cor(mpg, year)))
boxplot(mpg ~ origin, main = paste("cor", cor(mpg, as.numeric(origin))))
detach(Auto)