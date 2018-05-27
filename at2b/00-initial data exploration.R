# load and explore the raw data set before attempting any modelling. this file
# will generate a featurised data set that can be used later on for different
# modeling excercises, this featurised data set will contain all variables and
# any additional features, modeling steps later on can decide which features to
# keep or leave out


#### header ####
setwd("c:/mdsi/DAM/at2b")

library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(caret)
library(corrplot)

repurchase_levels <- c(0,1)
repurchase_labels <- c("no", "yes")
gender_levels <- c("Male", "Female", "NULL")
age_band_levels <- c("1. <25","2. 25 to 34","3. 35 to 44",
                     "4. 45 to 54","5. 55 to 64","6. 65 to 74",
                     "7. 75+","NULL")

df <- read_csv("./data/repurchase_training.csv", 
               col_types = cols(
                 Target = readr::col_logical(), 
                 age_band = readr::col_factor(levels=NULL), 
                 gender = readr::col_factor(levels=NULL), 
                 car_model = readr::col_factor(levels=NULL), 
                 car_segment = readr::col_factor(levels=NULL)
               ))


# d2f <- read_csv("./data/repurchase_training.csv")

glimpse(df)

####  check for missing values ####
attach(df) 
summary(Target)     ## no missing values 
summary(age_band)   ## 112,375 missing values 
summary(gender)     ## 69,308 missing values
summary(car_model)  ## no missing values but model_19 appeared twice
summary(car_segment)## Other category appear 58 times 

detach(df)


####  prepare featured data set ####
dff <- df

# re-ordering levels for factors 

# create a renamed outcome variable called repurchase with yes or no as values 
# dff$repurchase <- factor(dff$Target, levels = repurchase_levels, labels = repurchase_labels)
dff$repurchase <- dff$Target
str(dff$repurchase)

## age_band_levels <- sort(as.character(levels(dff$age_band)))
dff$age_band <- ordered(dff$age_band, 
                       levels = age_band_levels)

dff$gender <- factor(dff$gender, levels = gender_levels)

# drop ID and Target, the variables are not needed as ID is unique to
# observation and target is replaced with repurchase
dff <- dff %>% select(-ID, -Target)


#### plotting categorical variables ####
# dff %>% ggplot(aes(repurchase, age_band, color=gender)) + geom_jitter()



dff %>% ggplot(aes(x=dff$age_band, fill=repurchase)) + geom_bar()
dff %>% ggplot(aes(x=dff$age_band, fill=repurchase)) + geom_bar()

dff %>% ggplot(aes(x=dff$gender, fill=repurchase)) + geom_bar()
dff %>% filter(repurchase == T) %>%  ggplot(aes(gender)) + geom_bar()

dff %>% ggplot(aes(car_model, fill=repurchase)) + geom_bar()
dff %>% filter(repurchase == T) %>%  ggplot(aes(car_model)) + geom_bar()

dff %>% ggplot(aes(car_segment, fill=repurchase)) + geom_bar()
dff %>% filter(repurchase == T) %>% ggplot(aes(car_segment)) + geom_bar()

#### plotting numerical variables ####

plot_numerical <- function(dff,variable, label) {
  
  dff %>% ggplot(aes(variable, fill=repurchase)) + geom_bar() + labs(y=label)
  dff %>% ggplot(aes(x = repurchase, y=variable, fill=repurchase)) + geom_boxplot() + labs(y=label)
}

plot_numerical(dff, dff$age_of_vehicle_years, "Age of Vehicle")
plot_numerical(dff, dff$sched_serv_warr, "sched_serv_warr")
plot_numerical(dff, dff$non_sched_serv_warr, "non_sched_serv_warr")

plot_numerical(dff, dff$sched_serv_paid, "sched_serv_paid")
plot_numerical(dff, dff$non_sched_serv_paid, "non_sched_serv_paid")

plot_numerical(dff, dff$total_paid_services, "total_paid_services")
plot_numerical(dff, dff$total_services, "total_services")

plot_numerical(dff, dff$mth_since_last_serv, "mth_since_last_serv")
plot_numerical(dff, dff$annualised_mileage, "annualised_mileage")

plot_numerical(dff, dff$num_dealers_visited, "num_dealers_visited")
plot_numerical(dff, dff$num_serv_dealer_purchased, "num_serv_dealer_purchased")



#### correlation of numeric variables, corrplot, correlogram
numeric_variables <- names(dplyr::select_if(dff, is.numeric))
dff_corr <- cor(dff[, numeric_variables])

png(filename="./corrplot.png")
corrplot(dff_corr, 
         method="number", 
         type="upper")
dev.off()

#### write dff to file ####

write_csv(dff, "./dff.csv")







#### cv.glmnet - cross validation ####



# split 
train <- createDataPartition(y = dff$repurchase, p = .7, list = F)
dff_train.cv <- dff[train, ]
dff_test.cv <- dff[-train, ]

# prepare x, y for cv.glmnet
x = model.matrix(~ ., dff_train.cv %>% select(-repurchase))
y = as.numeric(dff_train.cv$repurchase)

# fit model with cv 
set.seed(42)
repurchase.glm.cv <- cv.glmnet(x, y, family = 'binomial', alpha = 0)

# plot cv mode 
plot(repurchase.glm.cv)
coef(repurchase.glm.cv, repurchase.glm.cv$lambda.min)
coef(repurchase.glm.cv, repurchase.glm.cv$lambda.1se)


dff_test.cv$pred_class = predict(repurchase.glm.cv$glmnet.fit, 
                           newx = model.matrix(~ ., dff_test.cv %>% select(-repurchase)), 
                           type = "class",
                           s = repurchase.glm.cv$lambda.1se)

# dff_test.cv$pred_propability = predict(repurchase.glm.cv$glmnet.fit, 
#                                  newx = model.matrix(~ ., dff_test.cv %>% select(-repurchase)), 
#                                  type = "response",
#                                  s = repurchase.glm.cv$lambda.1se)

dff_test.cv %>% mutate(r1 = as.character(as.integer(repurchase))) %>% select(r1, pred_class) -> ddd
ddd$r1 <- factor(ddd$r1)
ddd$pred_class <- factor(ddd$pred_class)
confusionMatrix(date = ddd$pred_class, ddd$r1)
