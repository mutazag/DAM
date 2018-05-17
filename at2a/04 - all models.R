## 36106 - Data Algorithms and Meaning 
## Assignment 2 Part A: Linear Regression
##
## Mutaz Abu Ghazaleh 
## 13184383
##
## fitting to all industry and location combinations 

## Library
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
library(scales)
library(RcppRoll) # used to calculate rolling mean
library(broom)
library(caret)

setwd("c:/mdsi/dam/at2a")
# load the prepared  feature engineered transaction file 
df_features <- read_csv("./transactions_features.csv", 
                        col_types = list(
                          readr::col_date(format=""), 
                          readr::col_factor(levels = NULL),
                          readr::col_factor(levels = NULL), 
                          readr::col_double(), 
                          readr::col_integer(), 
                          readr::col_factor(levels=NULL), 
                          readr::col_integer(), 
                          readr::col_double(), 
                          readr::col_double()
                        ))



#### plotting functions ####
#montly line 
plot_monthly <- function(df_agg, i=1, l=1, showlm = FALSE, smooth_line = "lm")
{
  print("industry ~ location plot")
  df_filter <- df_agg %>% filter(industry %in% i, location %in% l)
  
  p <- df_filter %>% 
    ggplot(aes(x=date, y=monthly_mean)) + 
    geom_line() + 
    facet_grid(industry ~ location,  labeller = label_both ) +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle=90)) + 
    labs(title = "Mean Monthly Sales over Date", 
         caption = paste0("Industry: ", paste(i, collapse = ","),
                          "\n. Location: ", paste(l, collapse = ",")),
         x = "Date", 
         y = "Mean Monthly Sales ($)")
  
  if (showlm == TRUE) {
    p <- p + geom_smooth(method = smooth_line, se = FALSE)
  }
  
  p
}

#### function ####

model_summary <- function(mod){
  mod_summary <- list()
  
  mod_summary$r2 <- summary(mod)$adj.r.squared
  mod_summary$rse <- summary(mod)$sigma
  mod_summary$aug <- mod %>% augment()
  # calculate RMSE
  mod_summary$RMSE <- sqrt(mean(mod_summary$aug$.resid ^ 2))
  
  #inspect RSE, Adj R-squared
  # 
  # sprintf("RMSE: %0.3f", mod_summary$RMSE)
  # sprintf("RSE: %0.4f", mod_summary$rse)
  # sprintf("Adj R-sqr: %0.4f", mod_summary$r2) 
  print(paste0("Adj R-sqr: ", mod_summary$r2))
  print(paste0("RMSE: ", mod_summary$RMSE)) # or RMSE(pred = mod1$fitted.values, obs = mod1$model$monthly_mean)
  print(paste0("RSE: ", mod_summary$rse))
  
  
}

# print RMSE, RSE and AdjR2 for model
# cross validate model with out-of-sample and print average out-of-sample RMSE 
fit_model_cv <- function (df, formula, ind=1, loc=1){
  df_subset <- df %>% filter(industry==ind, location==loc)
  trControl <- trainControl(method = "cv",number = 15, verboseIter = FALSE)
  mod <- train(formula, df_subset, method = "lm", trControl = trControl)
  
  print(formula)
  print("cross validation")
  print(mod$results)
  print("final model")
  model_summary(mod$finalModel)
  return(mod)
}




#### fitting all industry and locations ####

# create a model per combo, add the model to a list so it can later be used to
# predict out of sample for all combos
combos <- df_features %>% select(industry,location) %>% distinct()

# initalise objects to hold fitted model and summary performance results
mods_all <- list()
mods_df <- data_frame()

# loop to fit models for all industry locations 
for (i in 1:nrow(combos)) {
  c <- combos[i,]
  print(c)
  #cross validate the model
  mod <- fit_model_cv(df_features, ind = c$industry, loc=c$location, 
                   formula = monthly_mean ~ year + month)
  
  #add the final model to list of trained models
  mods_all[[i]] <- list(industry = c$industry,
       location = c$location, 
       model = mod, 
       RMSE = RMSE(pred = mod$finalModel$fitted.values,
                   obs = mod$finalModel$model$.outcome), 
       AdjR2 = summary(mod$finalModel)$adj.r.squared, 
       RMSE.cv = mod$results$RMSE, 
       AdjR2.cv = mod$results$Rsquared)
  
  #add stats to dataframe of all models 
  mods_df <- rbind( mods_df, 
                        data_frame(industry = c$industry,
                                   location = c$location,
                                   RMSE = RMSE(pred = mod$finalModel$fitted.values,
                                               obs = mod$finalModel$model$.outcome), 
                                   AdjR2 = summary(mod$finalModel)$adj.r.squared, 
                                   RMSE.cv = mod$results$RMSE, 
                                   AdjR2.cv = mod$results$Rsquared, 
                                   month = factor(12), 
                                   year = 2016))

}


#### inspect models ####
#inspect results, look for lowest AdjR2.cv 
mods_df %>% arrange(AdjR2.cv) %>% head(4) %>% write_csv("./worstperformance.csv")

plot_monthly(df_features,7,6, T, "auto") # fitting over m3 and m6 significantly improved the performance 
plot_monthly(df_features,3,9, T, "auto")
plot_monthly(df_features,7,4, T, "auto")
plot_monthly(df_features,5,10, T, "auto")


#### predict dec 2016 for all industries and locations ####
mods_df <- mods_df %>% mutate(monthly_mean = NA)

#genereate predictions for all models 
for (i in 1:nrow(mods_df)) {
  #retrieve model from mods list 
  mod <- mods_all[[i]]
  #prep input for prediction 
  input <- mods_df[i,] %>% select(year,month)
  #save prediction back to mods_df monthly_mean coln
  mods_df[i,]$monthly_mean <- predict(mod$model, input)

}


#### save all predictions to file ####
# Prepare a data set for all predections
pred_all <- mods_df %>% 
  mutate(date = dmy(paste("01", as.character(month), as.integer(year), sep = "-"))) %>% 
  select(date, industry, location, monthly_mean)
  


# write to file 
write_csv(pred_all, "./transactions_dec2016.csv")
