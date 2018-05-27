#### header ####
setwd("c:/mdsi/DAM/at2b")

library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(caret)

repurchase_levels <- c(0,1)
repurchase_labels <- c("no", "yes")
gender_levels <- c("Male", "Female", "NULL")
age_band_levels <- c("1. <25","2. 25 to 34","3. 35 to 44",
                     "4. 45 to 54","5. 55 to 64","6. 65 to 74",
                     "7. 75+","NULL")


dff <- read_csv("./dff.csv", 
               col_types = cols(
                 repurchase = readr::col_logical(),
                 age_band = readr::col_factor(levels=age_band_levels, ordered = T), 
                 gender = readr::col_factor(levels=gender_levels), 
                 car_model = readr::col_factor(levels=NULL), 
                 car_segment = readr::col_factor(levels=NULL)
               ))


#### functions #### 

train_test_glm <- function(formula = repurchase ~ ., 
                       train = dff_train, 
                       test = dff_test, 
                       cut = .5){
  # fit model on training data 
  repurchase.glm <-  glm(
    formula = formula, 
    data = train, 
    family = "binomial"
  )
   
  ## add fitted values back into the train data set for ease of access
  train$pred_propability <- repurchase.glm$fitted.values
  train$pred_class <- ifelse(train$pred_propability >=  cut, TRUE, FALSE)
  train_confusionMatrix <- confusionMatrix(
    data = as.factor(train$pred_class), 
    as.factor(train$repurchase))
  
  
  
  # predict on test data 
  test$pred_propability <- predict(
    repurchase.glm, 
    newdata = test, 
    type = "response"
  )
  
  
  # print("Confusion Matrix on Test Data Set")
  test$pred_class <- ifelse(test$pred_propability >=  cut, TRUE, FALSE)
  test_confusionMatrix <- confusionMatrix(
    data = as.factor(test$pred_class), 
    as.factor(test$repurchase))
  
   return(list(
    glm.fitted = repurchase.glm, 
    train = train, 
    train_confusionMatrix = train_confusionMatrix, 
    test = test, 
    test_confusionMatrix = test_confusionMatrix
  ))
  
  
}


# split 70/30 split
train <- createDataPartition(y = dff$repurchase, p = .7, list = F)
dff_train <- dff[train, ]
dff_test <- dff[-train, ]

# check classes ==> no is negative, yes is positive 
# unique(data.frame(repurchase.glm$data$repurchase, repurchase.glm$y))
contrasts(dff_train$repurchase) 


test1 <- train_test_glm()

test2 <- train_test_glm(formula = repurchase ~ gender + 
                          car_model + 
                          # age_of_vehicle_years +
                          sched_serv_warr + 
                          sched_serv_paid + 
                          non_sched_serv_paid + 
                          total_paid_services + 
                          total_services + 
                          mth_since_last_serv + 
                          annualised_mileage + 
                          num_dealers_visited + 
                          num_serv_dealer_purchased)





