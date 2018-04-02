## Ch 2.3


# ch 2.3.1 basic commands

x<- c(1,6,2)
x 

y<- c(1,4,3)


length(x)
length(y) 

x + y


ls() 
rm(x,y)
ls()


rm(list=ls())


# matrix 

x <- matrix (data = c(1,2,3,4), nrow =2, ncol=2) # matrix from vector by col
x

y <- matrix (data = c(1,2,3,4), nrow =2, ncol=2, byrow = TRUE) # mtraix from vector by row
y

# sqrt and power 2 on each element in the matrix
sqrt(x)
x^2

# vector of random variables using rnorm

x1 <- rnorm(50)
y1 <- x1 + rnorm(50, mean = 50, sd = .1)
cor(x1,y1) # compute correlation 

# seeding randoms
set.seed(1303) 
xs<-rnorm(50)
set.seed(1303)
ys<-rnorm(50)
cor(xs,ys) # cor == 1 as seeding was used on both random number vectors

#mean, var, sqrt, sd
mean(ys)
var(ys)
sqrt(var(ys)) == sd(ys)

# 2.3.2 Graphics 

#scatter plot 
plot(xs,ys)
plot(x1,y1, 
     xlab="this is the x-axis", 
     ylab = "this is the y-axis",
     main="plot of X vs Y", col = "green")

# output to pdf or jpeg ?dev.off
pdf("x vs y.pdf")
plot(x1,y1, 
     xlab="this is the x-axis", 
     ylab = "this is the y-axis",
     main="plot of X vs Y", col = "green")
dev.off()
list.files()



# sequence function 

xseq <- seq(1,10)
xseq2 <- 1:10
xseq == xseq2
piseq <- seq(-pi, pi, length=50)
plot(piseq)


# plot countor 
x1 <- seq(100)
y1 <- seq(100)
f = outer(x1,y1,function(x1,y1){cos(y1)/(1+x1^2)})
contour(x1,y1,f, nlevels = 45, add = T)
fa = (f-t(f))/2
contour(x1,y1,fa, nlevels=15)
image(x1,y1,fa, theta = 30) #heatmap
plot(x1,y1)


# 2.3.3 indexing data 

A <- matrix(1:16, 4,4)
A[2,3]

A[c(1,3), c(2,4)] # subsetting rows 1,3 and col 2,4 

A[1:2,] # rows 1,2

A[,1:2] # cols 1,2

A[1,] # one observation (row)

A[-c(2,3), ] # omit rows 2,3

dim(A) # dimensions of A


# 2.3.4 loading data 
# load auto data 

# download from ISL
download.file(url="http://www-bcf.usc.edu/~gareth/ISL/Auto.data", 
              destfile = file.path("Auto.data"))
list.files()
#load data
Auto <- read.table("Auto.data")
#display data in a sheet
fix(Auto)

#load with headers
Auto <- read.table("Auto.data", header = TRUE, na.strings = "?")
fix(Auto)
dim(Auto) # 397 obs 9 cols

# omit missing data 
Auto <- na.omit(Auto)
dim(Auto) # 5 rows with missing data were omitted
names(Auto) # variables (columns) names


# 2.3.5 graphics and stats 

plot(Auto$cylinders, Auto$mpg)

# attach data set before plotting
attach(Auto)
plot(cylinders, mpg) # ploting cyl as numeric -> scatter plot
cylinders <- as.factor(cylinders)
plot(cylinders,
     mpg,
     col = "red", 
     varwidth=TRUE, 
     xlab = "cyliders", 
     ylab = "mpg", 
     main = "plot cyl vs mpg") # plotting cyl as factor -> boxplot


#hist plot 
attach(Auto)
hist(mpg)
hist(mpg, col=2, breaks = 15)

# scatter plot matrix using pair 
pairs(Auto)
pairs( ~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower, mpg)

identify(horsepower, mpg, name) # iteractive plot to identify observations 



# stats 
# summary
summary(Auto)
summary(mpg)
