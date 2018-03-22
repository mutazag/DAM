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
