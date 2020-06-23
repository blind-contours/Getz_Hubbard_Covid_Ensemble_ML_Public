library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)
library(R6)
library(tidyverse)
library(readxl)

scale = FALSE
## read in processsed dataframe and load the ML pipeline results
data_original <- read_csv("Analysis/update_data/data/processed/cleaned_covid_data_final.csv")
ML_pipeline_results <- readRDS(here("Analysis/update_data/data/processed/ML_pipeline_5_outcomes_noscale.RDS"))


## create the per-capita outcomes
data_original$CountyRelativeDay25Cases_PopScale <- data_original$CountyRelativeDay25Cases / data_original$Population 
data_original$TotalCasesUpToDate_PopScale <- data_original$TotalCasesUpToDate / data_original$Population 
data_original$USRelativeDay100Deaths_PopScale <- data_original$USRelativeDay100Deaths / data_original$Population 
data_original$TotalDeathsUpToDate_PopScale <- data_original$TotalDeathsUpToDate / data_original$Population

# test perc reduced
percents <- c(0.0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)

all_outcomes <- c("CountyRelativeDay25Cases", 
              "TotalCasesUpToDate", 
              "USRelativeDay100Deaths" , 
              "TotalDeathsUpToDate", 
              "FirstCaseDay",
              "CountyRelativeDay25Cases_PopScale", 
              "TotalCasesUpToDate_PopScale",
              "USRelativeDay100Deaths_PopScale",
              "TotalDeathsUpToDate_PopScale")


target_outcomes <- c(
  "FirstCaseDay",
  "CountyRelativeDay25Cases_PopScale",
  "TotalCasesUpToDate_PopScale",
  "USRelativeDay100Deaths_PopScale",
  "TotalDeathsUpToDate_PopScale"
)

## set up covar features
covars <- colnames(data_original)[-which(names(data_original) %in% c(
  all_outcomes,
  "X1", 
  "FIPS",
  "FIPS.1",
  "county_names"
))]

## doing this outside the map function so it doesn't scale everytime, just need to do it once
if (scale) {
  data_original_scaled <- data_original %>% 
    mutate_at(covars, ~(scale(.)))
}
  



get_top_variables <- function(ML_results) {
  varimp_data <- ML_results$var_imp
  highest_rr_idx <- which.max(varimp_data$risk_ratio)
  top_var <- varimp_data[highest_rr_idx]$X
  return(top_var)
}

top_vars <- unlist(map(
  .x = ML_pipeline_results,
  .f = get_top_variables
))

make_boot_dfs <- function(top_var, percents) {
  df <- as.data.frame(matrix(nrow = length(percents), ncol = 5))
  colnames(df) <- c(
    top_var,
    "Boot Pred",
    "Boot Low",
    "Boot High",
    "Init. Model Pred"
  )
  return(df)
}

boot_dfs <- map(
  .x = top_vars,
  .f = make_boot_dfs, percents = percents
)

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
      fold_fun = folds_vfold, V = 2
    )
  )

  sl_fit <- sl$train(resampled_data_task)

  data_resampled_reduced <- data_original

  data_resampled_reduced[, target_variable] <- data_resampled_reduced[, target_variable] - (data_resampled_reduced[, target_variable] * perc)

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
                                          ML_pipeline_results,
                                          outcome,
                                          boot_df,
                                          data_original,
                                          covars,
                                          percents, 
                                          pop) {
  

  sl_fit <- ML_pipeline_results$fit

  for (i in 1:length(percents)) {
    perc <- percents[i]
    data_temp <- data_original
    data_temp[, target_variable] <- data_original[, target_variable] - (data_original[, target_variable] * perc)

    task_reduced_initial_est <- make_sl3_Task(
      data = data_temp,
      covariates = covars,
      outcome = outcome,
      folds = origami::make_folds(data_temp,
        fold_fun = folds_vfold, V = 2
      )
    )
    
    sl_preds_reduced <- sl_fit$predict(task_reduced_initial_est)

    boot_updates <- replicate(10, bootstrapCI(
      target_variable = target_variable,
      data_original = data_original,
      ML_pipeline_results = ML_pipeline_results,
      covars = covars,
      outcome = outcome,
      perc = perc
    ))
    
    if (outcome == "FirstCaseDay") {
      total_counts <- mean(sl_preds_reduced)
      boot_updates <- boot_updates
      
    } else {
      #browser()
      total_counts <- sum(sl_preds_reduced * pop)
      boot_updates <- boot_updates * pop
      boot_updates <- colSums(boot_updates)
    }

    CI_boot <- quantile(boot_updates, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)

    boot_df[i, 1] <- perc
    boot_df[i, 2] <- CI_boot[[2]]
    boot_df[i, 3] <- CI_boot[[1]]
    boot_df[i, 4] <- CI_boot[[3]]
    boot_df[i, 5] <- total_counts
  }
  return(boot_df)
}

boot_results <- pmap(list(
  target_variable = top_vars,
  ML_pipeline_results = ML_pipeline_results,
  outcome = target_outcomes,
  boot_df = boot_dfs),
.f = bootsrap_marginal_predictions,
data_original = data_original,
covars = covars,
percents = percents,
pop = data_original$Population
)
