#### header ####
setwd("c:/mdsi/DAM/at2b")

library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(caret)
library (rpart)
library(rpart.plot)
library(mlbench)
library(randomForest)

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


#### functions  #### 

train_test_rpart <- function(formula = repurchase ~ ., 
                       train = dff_train, 
                       test = dff_test){
  
  # fit model on training data 

  repurchase.rpart <-  rpart(
    formula = formula, 
    data = train, 
    method = "class"
  )
  
  train$pred_class <- predict(
    repurchase.rpart,
    train %>% select(-repurchase),
    type="class")
  
  # print("Confusion Matrix on TRain Data Set")
  train_confusionMatrix <- confusionMatrix(
    data = as.factor(train$pred_class), 
    as.factor(train$repurchase))
  
  
  #predict on test data - get class 
  test$pred_class <- predict(
    repurchase.rpart,
    test %>% select(-repurchase),
    type="class")
  
  
  # predict on test data -- probablity of both classes. the following two lines
  # yield similar result as pred_class which can be computed directly by calling
  # predict with type="class". this line is not necessary
  test$pred_propability <- predict(
    repurchase.rpart,
    test %>% select(-repurchase),
    type="prob")
  test$pred_class2 <- ifelse(as.vector(test$pred_propability[,"TRUE"]) >=  .5, TRUE, FALSE)
  
  
  # print("Confusion Matrix on Test Data Set")
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
    model.type = "rpart", 
    fitted.model = repurchase.rpart, 
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
test1 <- train_test_rpart()
prp(test1$fitted.model )
# varImp(test1$glm.fitted)
# coef(test1$glm.fitted)


# test without variables that were showing high p-values from
# previous test1
test2 <- train_test_rpart(formula = repurchase ~ 
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


#### remove na and test again ####
# remove missing values and fit a model again 
dff_na.rm <- dff %>% filter(gender != "NULL", age_band != "NULL")
train_na.rm <- createDataPartition(y = dff_na.rm$repurchase, p = .7, list = F)
dff_train_na.rm <- dff_na.rm[train_na.rm, ]
dff_test_na.rm <- dff_na.rm[-train_na.rm, ]


test1_na.rm <- train_test_rpart(train = dff_train_na.rm, test = dff_test_na.rm)
test2_na.rm <- train_test_rpart(formula = repurchase ~ 
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

test3 <- train_test_rpart(formula = repurchase ~ 
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



#### compare all example models ####

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


model_results <- rbind(
report_model(test1, "all variables as predictors"),
report_model(test2, "age and gender but not other categorical variables"),
report_model(test3, "no categorical variables")
)



#### change seed and run best model (test1) ####

# we will do this to test the stabilit of the tree and determine if pruning is needd

seed_split_train_test <- function(seed = 42){
  
  set.seed(seed)
  # split 70/30 split
  train <- createDataPartition(y = dff$repurchase, p = .7, list = F)
  dff_train <- dff[train, ]
  dff_test <- dff[-train, ]
  
  test1 <- train_test_rpart()
  return(test1)
}


seedtest_42 <- seed_split_train_test(42)
seedtest_14 <- seed_split_train_test(14)
seedtest_22 <- seed_split_train_test(22)

seedtest_results <- rbind(
  report_model(seedtest_42, "seedtest_42"),
  report_model(seedtest_14, "seedtest_14"),
  report_model(seedtest_22, "seedtest_22")
)


prp(seedtest_42$fitted.model)
prp(seedtest_14$fitted.model)
prp(seedtest_22$fitted.model)


#### prunning of test 1 model to avoid overfitting the tree ####

prune_model <- function(model_result){
  model <- model_result$fitted.model
  # get minimum cost complexity pruning
  #plotcp(model)
  opt <- which.min(model$cptable[,"xerror"])
  # take it one step up 
  cp <- model$cptable[(opt-2), "CP"]
  
  # prune model with select minimum cp - 1 
  pruned_model <- prune(model, cp )
  #prp(pruned_model)
  train <- model_result$train %>% select(-pred_class)
  train$pred_class <- predict(pruned_model,
                                    train %>% select(-repurchase),
                                    type="class")
  
  test <- model_result$test %>% select(-pred_class,
                                       -pred_propability,
                                       -pred_class2)
  test$pred_class <- predict(pruned_model, 
                                  test %>% select(-repurchase),
                                  type="class")
  test$pred_propability <- predict(pruned_model,
                                         test %>% select(-repurchase),
                                         type="prob")
  test$pred_class2 <- ifelse(as.vector(test$pred_propability[,"TRUE"]) >=  .5, TRUE, FALSE)
  
  # print("Confusion Matrix on TRain Data Set")
  train_confusionMatrix <- confusionMatrix(
    data = as.factor(train$pred_class), 
    as.factor(train$repurchase))
  
  # print("Confusion Matrix on Test Data Set")
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
    model.type = "rpart.pruned", 
    fitted.model = pruned_model, 
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



test3_pruned <- prune_model(test3)
prp(test3_pruned$fitted.model)
prp(test3$fitted.model)

prunning_report <- rbind(
  report_model(test3, "test3 before prunning"), 
  report_model(test3_pruned, "test3 after prunning")
)
