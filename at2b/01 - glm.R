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


# training and test using glm 
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
  
  
  # check classes ==> no is negative, yes is positive 
  unique(data.frame(repurchase.glm$data$repurchase, repurchase.glm$y))
  data.frame(repurchase.glm$data$repurchase, repurchase.glm$y, repurchase.glm$fitted.values) %>% 
    arrange(desc(repurchase.glm$fitted.values)) %>% 
    head(100)
  # contrasts(dff_train$repurchase) 
  
   
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
  
  
  # store accuracy, precision, recall and F1 from test partition 
  Accuracy <- test_confusionMatrix$overall[["Accuracy"]]
  Sensitivity <- test_confusionMatrix$byClass[["Sensitivity"]]
  Precision <- test_confusionMatrix$byClass[["Precision"]]
  Recall <- test_confusionMatrix$byClass[["Specificity"]]
  F1 <- 2 * ((Precision*Recall)/(Precision+Recall))
   
  
  return(list(
    model.type = "glm",
    fitted.model = repurchase.glm, 
    train = train, 
    train_confusionMatrix = train_confusionMatrix, 
    test = test, 
    test_confusionMatrix = test_confusionMatrix, 
    perf = data.frame(Accuracy, 
                      Sensitivity, 
                      Precision, 
                      Recall, 
                      F1)
  ))
  
  
}


# helper function to create model summary 
report_model <- function(model_result,desc =""){
  obj_name <- deparse(substitute(model_result))
  return(
    data.frame(
      name = obj_name,
      model.type = model_result$model.type, 
      desc = desc, 
      model_result$perf
    )
  )
}
#### split ####

# split 70/30 split
train <- createDataPartition(y = dff$repurchase, p = .7, list = F)
dff_train <- dff[train, ]
dff_test <- dff[-train, ]

# check classes ==> no is negative, yes is positive 
# unique(data.frame(repurchase.glm$data$repurchase, repurchase.glm$y))
contrasts(dff_train$repurchase) 



#### train_test with different model arch ####

# test with all variables 
test1 <- train_test_glm()
summary(test1$fitted.model)
coef(test1$fitted.model)
varImp(test1$fitted.model) -> test1_varimp
test1$perf
# varImp(test1$glm.fitted)
# coef(test1$glm.fitted)


# test without variables that were showing high p-values from
# previous test1
test2 <- train_test_glm(formula = repurchase ~ 
                          # age_band + 
                          gender + 
                          # car_model + 
                          # car_segment + 
                          # age_of_vehicle_years +
                          sched_serv_warr + 
                          # non_sched_serv_warr + 
                          sched_serv_paid + 
                          non_sched_serv_paid + 
                          # total_paid_services + 
                          total_services + 
                          mth_since_last_serv + 
                          annualised_mileage + 
                          num_dealers_visited + 
                          num_serv_dealer_purchased)
summary(test2$fitted.model)
test2$perf

#### remove na and test again ####
# remove missing values and fit a model again 
dff_na.rm <- dff %>% filter(gender != "NULL", age_band != "NULL")
train_na.rm <- createDataPartition(y = dff_na.rm$repurchase, p = .7, list = F)
dff_train_na.rm <- dff_na.rm[train_na.rm, ]
dff_test_na.rm <- dff_na.rm[-train_na.rm, ]


test1_na.rm <- train_test_glm(train = dff_train_na.rm, test = dff_test_na.rm)
test2_na.rm <- train_test_glm(formula = repurchase ~ 
                                            age_band +
                                            gender + 
                                            # car_model + 
                                            # car_segment + 
                                            # age_of_vehicle_years +
                                            sched_serv_warr + 
                                            # non_sched_serv_warr + 
                                            sched_serv_paid + 
                                            non_sched_serv_paid + 
                                            # total_paid_services + 
                                            total_services + 
                                            mth_since_last_serv + 
                                            annualised_mileage + 
                                            num_dealers_visited + 
                                            num_serv_dealer_purchased, 
                          train = dff_train_na.rm, 
                          test = dff_test_na.rm)


test1$perf
test2$perf
test1_na.rm$perf
test2_na.rm$perf


# based on test2_na.rm coef, will create test3

test3 <- train_test_glm(formula = repurchase ~ 
                          # age_band +
                          # gender + 
                          # car_model + 
                          # car_segment + 
                          age_of_vehicle_years +
                          # sched_serv_warr + 
                          # non_sched_serv_warr + 
                          sched_serv_paid + 
                          # non_sched_serv_paid + 
                          # total_paid_services + 
                          total_services + 
                          mth_since_last_serv + 
                          annualised_mileage + 
                          num_dealers_visited + 
                          num_serv_dealer_purchased) 

test3$perf
test3$train_confusionMatrix
test3$test_confusionMatrix
# Reference --- this is better than the cv model as it caught more true positives and minised false negatives (missd opportunity) --- higher specifity 
# Prediction FALSE  TRUE
# FALSE 38320   866
# TRUE     24   190
summary(test3$fitted.model)




# test1 with lower cut threshold (all vars)
test1_lowcut <- test1 <- train_test_glm(cut =0.2)
test1_lowcut$perf 
test1_lowcut$test_confusionMatrix

# test 3 with lower cut (less categorical variables)
test3_lowcut <- train_test_glm(formula = repurchase ~ 
                          # age_band +
                          # gender + 
                          # car_model +
                          # car_segment +
                          age_of_vehicle_years +
                          # sched_serv_warr + 
                          # non_sched_serv_warr + 
                          sched_serv_paid + 
                          # non_sched_serv_paid + 
                          # total_paid_services + 
                          total_services + 
                          mth_since_last_serv + 
                          annualised_mileage + 
                          num_dealers_visited + 
                          num_serv_dealer_purchased, 
                          cut = 0.2) 
test3_lowcut$perf 
test3_lowcut$test_confusionMatrix

# tested glm with more configuration until reached a hig er specificity using a
# lower cut and removing gender and age_band as they contain a very high number
# of missing values



#### models perf reports #####
glm_full_report <- rbind(
report_model(test1, "all predictors, 0.5 probability cut"),
report_model(test2, "Removed predictors that rank as low importance in previous test:  age_band,  car_model, car_segment, 
age_of_vehicle_years , non_sched_serv_warr,  total_paid_services"),
report_model(test1_na.rm, "Repeat test 1 after dropping records with missing values for gender and age_band"),
report_model(test2_na.rm, "Repeat test 2 after dropping records with missing values for gender and age_band"),
report_model(test3, "Removing all categorical variables, include age_band and gender and numerical predictors that ranked low in test1"),
report_model(test1_lowcut, "Same settings are test1 with 0.2 probablity cut threshold"),
report_model(test3_lowcut, "Same settings are test3 with 0.2 probablity cut threshold"))

write_csv(glm_full_report, "./glm_full_report.csv")



#### Variables Importance #### 

varImp(test1$fitted.model) -> test1_varimp
varImp(test2$fitted.model) -> test2_varimp
varImp(test3$fitted.model) -> test3_varimp

glm_varImp <- rbind(
cbind(varimp = test1_varimp, varName = row.names(test1_varimp), test="test1", model.type = "glm"),
cbind(varimp = test2_varimp, varName = row.names(test2_varimp),test="test2", model.type = "glm"),
cbind(varimp = test3_varimp, varName = row.names(test3_varimp),test="test3", model.type = "glm"))

write_csv(glm_varImp, "./glm_varimp.csv")


