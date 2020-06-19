library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)
library(R6)
library(tidyverse)
library(readxl)


data_original <- read_csv("Analysis/update_data/data/processed/cleaned_covid_data_final.csv")
ML_pipeline_results <- readRDS(here("Analysis/update_data/data/processed/ML_pipeline_5_outcomes.RDS"))

#test perc reduced
percents <- c(0.0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90) 

outcomes <- c("FirstCaseDay",
              "CountyRelativeDay25Cases_PopScale", 
              "TotalCasesUpToDate_PopScale",
              "USRelativeDay100Deaths_PopScale",
              "TotalDeathsUpToDate_PopScale")


make_boot_dfs <- function(top_var){
  
  df <- as.data.frame(matrix(nrow = length(percents), ncol = 5))
  colnames(df) <- c(top_var,
                               'Boot Pred', 
                               'Boot Low' , 
                               'Boot High', 
                               'Init. Model Pred')
  return(df)
}

top_variables <- c("public transportation to work", "heart stuff", "general hardship")
boot_dfs <- map(.x = top_variables, .f = make_boot_dfs)

## set up bootstrap CI function
bootstrapCI <- function(target_variable, 
                        data_original, 
                        ML_pipeline_results,
                        covars,
                        outcome,
                        perc) {
  
  sl <- ML_pipeline_results$sl_obj
  
  nr <- nrow(data_original)
  data_tmp <- data_original
  resampled_data <- data_tmp[sample(1:nr, size = nr, replace = TRUE), ]
  
  resampled_data_task <- make_sl3_Task(
    data = resampled_data,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(resampled_data, 
                                fold_fun = folds_vfold, V = 2)
  )
  
  sl_fit <- sl$train(resampled_data_task)
  
  data_resampled_reduced <- data_original
  
  data_resampled_reduced[,target_variable] <- data_resampled_reduced[, target_variable] - (data_resampled_reduced[, target_variable]*perc)
  
  resampled_data_reduced_task <- make_sl3_Task(
    data = data_resampled_reduced,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(data_resampled_reduced, fold_fun = folds_vfold, V = 2)
  )
  
  sl_preds_reduced <- sl_fit$predict(resampled_data_reduced_task)

  return(sl_preds_reduced)
}


## run marginal predictions for each decrease in target variable from variable importance calculations
bootsrap_marginal_predictions <- function(target_variable, 
                                          data_original, 
                                          ML_pipeline_results,
                                          covars,
                                          outcome) {
  
  for (i in 1:length(percents)) {
    
    perc <- percents[i]
    data_temp <- data_original
    data_temp[,target_variable] <- data_original[, target_variable] - (data_original[, target_variable]*perc)
    
    task_reduced_initial_est <- make_sl3_Task(
      data = data_temp,
      covariates = covars,
      outcome = outcome,
      folds = origami::make_folds(data_temp, 
                                  fold_fun = folds_vfold, V = 2))
    
    sl_preds_reduced <- sl_fit$predict(task_reduced_initial_est)
    
    if (outcome != "FirstCaseDay") {
      total_counts <- sl_preds_reduced * data_temp$Population
    } else { total_counts <- sl_preds_reduced }
    
    boot_updates <- replicate(10, bootstrapCI(target_variable = target_variable, 
                                              data_original = data_original, 
                                              ML_pipeline_results = ML_pipeline_results,
                                              covars = covars,
                                              outcome = outcome,
                                              perc = perc))
    
    CI_boot_day25 <- quantile(boot_day25, probs = c(0.025,0.50, 0.975), na.rm = TRUE)
    
    results_day25[i,1] <- perc
    results_day25[i,2] <- CI_boot_day25[[2]]
    results_day25[i,3] <- CI_boot_day25[[1]]
    results_day25[i,4] <- CI_boot_day25[[3]]
    results_day25[i,5] <- sum_cases
}
}


