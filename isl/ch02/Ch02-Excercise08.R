## Excercise 8 

## College data set: http://www-bcf.usc.edu/~gareth/ISL/College.csv 


download.file("http://www-bcf.usc.edu/~gareth/ISL/College.csv", file.path("college.csv"))


# (a) read the college data set

college <- read.csv("college.csv")

# (b) examine using fix
fix(college)
# name rows after college name in col 1 and get rid of first col containing college names
rownames(college) <- college[,1] #select the first col
college <- college[,-1] 
head(college)


# (c)  

# (i) summary of college
summary(college)

# (ii) scatter plot of the firt 10 colns 
pairs(college[,1:10])

# (iii) side by side boxplots of Outstate and Private 
attach(college) 
plot(Private, 
     Outstate,
     xlab= "Private", 
     ylab= "Outstate", 
     main= "Private vs Outstate")
detach(college)

# (iv) create Elite

Elite <- rep("No", nrow(college))
Elite[college$Top10perc >50]="Yes"
Elite <- as.factor(Elite)
college$Elite <- Elite # same as college <- data.frame(college,Elite)

summary(college$Elite)
attach(college)
plot(Elite, 
     Outstate,
     xlab= "Elite", 
     ylab= "Outstate", 
     main= "Elite vs Outstate")
detach(college)


# (v)  hist of 4 quantitiatve params

attach(college) 
par(mfrow =c(2,2))
hist(Apps)
hist(F.Undergrad)
hist(P.Undergrad)
hist(Grad.Rate)

par(mfrow = c(1,1))
plot(Elite, Grad.Rate)

detach(college)
