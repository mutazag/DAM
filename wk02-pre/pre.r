# Activities for Week 2 (19 Mar 2018) 

#load diamonds.csv
list.files()

diamonds <- read.csv("diamonds.csv", header = TRUE, stringsAsFactors = TRUE)

# structure of diamonds
str(diamonds)

# get the most expensie diamond 
max(diamonds$price )
#1
diamonds[order(diamonds$price, decreasing = TRUE)[1], ]
# or 2
diamonds[diamonds$price == max(diamonds$price), ]



# data packages 
head(AirPassengers)
str(AirPassengers)

# list all data packages
data(package = .packages(all.available = TRUE))

head(mtcars)
str(mtcars)
rownames(mtcars)
colnames(mtcars)
summary(mtcars)


# data types
data <- mtcars
str(data)
data$cyl <- as.factor(data$cyl)
data$cyl

data$vs <- as.factor(data$vs)
data$am <- as.factor(data$am)
data$gear <- as.factor(data$gear)
data$carb <-as.factor(data$carb)
str(data)

# create a column for car names (based on row names) 
data$carname <- rownames(data)
data

# subsetting 
head(data[,1:3])
head(data[, c("mpg", "cyl", "disp")])


# visualisation 

hist(data$qsec, 
     main =  "Histogram of 1/4 mile per second variable", 
     xlab = "1/4 mile per second"
) 
summary(data$qsec)


# ploting variables against each other 
 ?plot
plot(
  x = data$disp, 
  y = data$mpg, 
  main = "Miles per gallon against displacement", 
  xlab = "displacement", 
  ylab = "mpg"
)


# created a faceted plot , add the third dimension to an x-y scatter plot 
?coplot
coplot( 
  formula = disp ~ mpg | as.factor(cyl), 
  data = data, 
  panel = panel.smooth, 
  rows =1
  )


# pairs -- create scatter plots for pair-wise combinations 
? pairs
col_pairs <- c("mpg", "cyl", "disp", "qsec")
pairs(data[,col_pairs], main = "mtcars data")



## plotting diamonds 

diamonds_colnames <- colnames(diamonds)
coplot(
  formula = price ~ x | as.factor(clarity), 
  data= diamonds, 
  panel = panel.smooth, 
  rows=1
  
)


pairs(diamonds[, diamonds_colnames[c(6,8,9,10,11)]])

hist(diamonds$price)
