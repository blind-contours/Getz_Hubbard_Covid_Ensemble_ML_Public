# switch up library path for the slurm monster
r_libdir <- Sys.getenv("R_LIBDIR")

# set user-specific package library
if (grepl("savio2", Sys.info()["nodename"])) {
  .libPaths(r_libdir)
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
}


library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)
library(R6)
library(tidyverse)
library(readxl)
library(gam)
library(data.table)
library(gbm)
library(tidyr)
library(gbm)
library(xgboost)


`%notin%` <- Negate(`%in%`)

sapply(list.files(
  path = here("Analysis/poisson_learners"),
  full.names = TRUE
), source)


scale <- FALSE
## read in processsed dataframe and load the ML pipeline results
data_original <- read_csv(here("Analysis/update_data/data/processed/cleaned_covid_data_final.csv"))
ML_pipeline_results <- readRDS(here("Analysis/update_data/data/processed/ML_pipeline_5_outcomes_noscale_july16.RDS")) ### MAKE SURE TO CHANGE THIS AS UPDATED!

## read in data dictionary for identifying subgroups of top variables to isolate the different control conditions
Data_Dictionary <- read_excel(here("Analysis/update_data/data/processed/Data_Dictionary.xlsx"))
Data_Dictionary_Used <- Data_Dictionary %>%
  filter(Keep == "Yes") %>%
  select(`Variable Name`, `Sub-Category`)
## remove from the list covariates that had too many NAs and were then dropped before analysis, FIPS, and the outcome data:

# vars_rmv_na <- colnames(data_original)[names(data_original) %notin% Data_Dictionary_Used$`Variable Name`]

vars_rmv_na <- read.csv(here("Analysis/update_data/data/processed/vars_removed_na_thresh.csv"))

vars_rmv_na <- as.vector(vars_rmv_na$x)

removing <- c(
  vars_rmv_na,
  "FIPS",
  "CountyRelativeDay25Cases",
  "TotalCasesUpToDate",
  "USRelativeDay100Deaths",
  "TotalDeathsUpToDate",
  "FirstCaseDay"
)

Data_Dictionary_Used <- Data_Dictionary_Used[-match(removing, Data_Dictionary_Used$`Variable Name`), ]
variable_list <- Data_Dictionary_Used$`Variable Name`
subcategory_list <- Data_Dictionary_Used$`Sub-Category`

## create the per-capita outcomes
data_original$CountyRelativeDay25Cases_PopScale <- data_original$CountyRelativeDay25Cases / data_original$Population
data_original$TotalCasesUpToDate_PopScale <- data_original$TotalCasesUpToDate / data_original$Population
data_original$USRelativeDay100Deaths_PopScale <- data_original$USRelativeDay100Deaths / data_original$Population
data_original$TotalDeathsUpToDate_PopScale <- data_original$TotalDeathsUpToDate / data_original$Population

# test perc reduced
percents <- c(0.0, 0.1, 0.2, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)

all_outcomes <- c(
  "CountyRelativeDay25Cases",
  "TotalCasesUpToDate",
  "USRelativeDay100Deaths",
  "TotalDeathsUpToDate",
  "FirstCaseDay",
  "CountyRelativeDay25Cases_PopScale",
  "TotalCasesUpToDate_PopScale",
  "USRelativeDay100Deaths_PopScale",
  "TotalDeathsUpToDate_PopScale"
)


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
    mutate_at(covars, ~ (scale(.)))
}


get_top_variables <- function(ML_results) {
  varimp_data <- ML_results$var_imp
  highest_rr_idx <- which.max(varimp_data$risk_ratio)
  top_var <- varimp_data[highest_rr_idx]$X
  return(top_var)
}

top_vars <- unlist(purrr::map(
  .x = ML_pipeline_results,
  .f = get_top_variables
))

## get the top variables, their subcategories, and accompanying variables in same category for marginal predictions
top_var_subgroups <- subcategory_list[match(top_vars, variable_list)]
top_var_subcat_vars <- purrr::map(.x = top_var_subgroups, ~ variable_list[subcategory_list %in% .x])

make_boot_dfs <- function(top_var, percents) {
  df <- as.data.frame(matrix(nrow = length(percents), ncol = 4))
  colnames(df) <- c(
    top_var,
    "Boot Pred",
    "Boot Low",
    "Boot High"
  )
  return(df)
}

boot_dfs_sl_full <- purrr::map(
  .x = top_vars,
  .f = make_boot_dfs, percents = percents
)

boot_dfs_sl_no_subcat <- purrr::map(
  .x = top_vars,
  .f = make_boot_dfs, percents = percents
)

boot_dfs_univar_gam <- purrr::map(
  .x = top_vars,
  .f = make_boot_dfs, percents = percents
)

## set up bootstrap CI function
bootstrapCI <- function(target_variable,
                        data_original,
                        ML_pipeline_result,
                        covars,
                        outcome,
                        perc,
                        sub_cat_vars) {
  
  sl <- ML_pipeline_result$sl_obj
  
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
  # create another SL task but remove all other variables in the target variable subcategory and only include the target variable
  resampled_data_task_no_subcat_covars <- make_sl3_Task(
    data = resampled_data,
    covariates = c(covars[covars %notin% sub_cat_vars], target_variable),
    outcome = outcome,
    folds = origami::make_folds(resampled_data,
                                fold_fun = folds_vfold, V = 2
    )
  )
  
  
  ## train a univariate gam on the resampled data
  univar_gam_model <- gam(as.formula(paste(outcome, paste("s(" , target_variable,")", sep = ""), sep = "~")),
                          data = resampled_data)
  
  
  
  # lrnr_mean <- make_learner(Lrnr_mean)
  #sl_2 <- make_learner(Lrnr_sl, sl$params$learners[c(1,4)])
  #sl_fit_full_resampled <- sl_2$train(resampled_data_task)
  #sl_fit_nosubcat_resampled  <- sl_2$train(resampled_data_task_no_subcat_covars)
  # 
  sl_fit_full_resampled <- sl$train(resampled_data_task)
  sl_fit_nosubcat_resampled <- sl$train(resampled_data_task_no_subcat_covars)
  
  
  ## get the original data and reduce the target variable by perc
  data_resampled_reduced <- data_original
  target_min <- min(data_resampled_reduced[, target_variable])
  
  for(i in 1:nrow(data_resampled_reduced)) {
    if(target_min < data_resampled_reduced[[i, target_variable]])
      data_resampled_reduced[[i, target_variable]] <- data_resampled_reduced[[i, target_variable]] - (data_resampled_reduced[[i, target_variable]] * perc)
    else {
      data_resampled_reduced[[i, target_variable]] <- target_min
    }
  }
  
  resampled_data_reduced_task <- make_sl3_Task(
    data = data_resampled_reduced,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(data_resampled_reduced, fold_fun = folds_vfold, V = 2)
  )
  
  resampled_data_reduced_task_no_subcat_covars <- make_sl3_Task(
    data = data_resampled_reduced,
    covariates = c(covars[covars %notin% sub_cat_vars], target_variable),
    outcome = outcome,
    folds = origami::make_folds(resampled_data,
                                fold_fun = folds_vfold, V = 2
    )
  )
  
  
  ## predict through superlearner for reduced data on resampled models
  sl_preds_reduced_full <- sl_fit_full_resampled$predict(resampled_data_reduced_task)
  # predict from no subcategories
  sl_preds_reduced_no_subcat <- sl_fit_nosubcat_resampled$predict(resampled_data_reduced_task_no_subcat_covars)
  # predict from univariate gam
  univariate_gam_predictions <- stats::predict(object = univar_gam_model, newdata = data_resampled_reduced)
  
  results <- data.frame(sl_preds_reduced_full, sl_preds_reduced_no_subcat, as.vector(univariate_gam_predictions))
  colnames(results) <- c("SL_full_model", "SL_no_tgt_subcat_vars", "univariate_gam")
  
  return(results)
}

## run marginal predictions for each decrease in target variable from variable importance calculations
bootstrap_marginal_predictions <- function(target_variable,
                                           ML_pipeline_result,
                                           outcome,
                                           boot_df_sf_full,
                                           boot_df_sf_no_subcat,
                                           boot_df_univar_gam,
                                           sub_cat_vars,
                                           data_original,
                                           covars,
                                           percents,
                                           pop,
                                           boot_num){
  
  initialize_data <- rep(NaN, dim(data_original)[1] * boot_num * length(percents))
  
  boot_data_array_full_sl <- as.data.frame(matrix(0, ncol = length(percents), nrow = boot_num))
  boot_data_array_full_nosubcat <- as.data.frame(matrix(0, ncol = length(percents), nrow = boot_num))
  boot_data_array_full_univar_gam <- as.data.frame(matrix(0, ncol = length(percents), nrow = boot_num))
  
  for (i in 1:length(percents)) {
    
    perc <- percents[i]
    
    boot_updates <- foreach(this_iter = seq_len(boot_num),
                            .options.multicore = list(preschedule = FALSE),
                            .errorhandling = "remove") %dopar%  { 
                              bootstrapCI(
                                target_variable = target_variable,
                                data_original = data_original,
                                ML_pipeline_result = ML_pipeline_result,
                                covars = covars,
                                outcome = outcome,
                                perc = perc,
                                sub_cat_vars = sub_cat_vars)
                            }
    
    if (outcome == "FirstCaseDay") {
      ## reformat and extract the bootstrap results
      boot_totals <- as.data.frame(t(colMeans(bind_rows(boot_updates))))
      total_counts_SL_full <- boot_totals$SL_full_model
      total_counts_SL_no_subcat <- boot_totals$SL_no_tgt_subcat_vars
      total_counts_univariate_gam <- boot_totals$univariate_gam
      
      boot_updates_SL_full <- colMeans(do.call(cbind, sapply(boot_updates, "[", 1))) ## wish I didn't have to manually index here but in a hurry
      boot_updates_SL_nosubcat <- colMeans(do.call(cbind, sapply(boot_updates, "[", 2)))
      boot_updates_SL_univar_gam <- colMeans(do.call(cbind, sapply(boot_updates, "[", 3)))
    } else {
      total_totals <- as.data.frame(t(colSums(bind_rows(boot_updates) * pop)))
      total_counts_SL_full <- total_totals$SL_full_model
      total_counts_SL_no_subcat <- total_totals$SL_no_tgt_subcat_vars
      total_counts_univariate_gam <- total_totals$univariate_gam
      
      boot_updates_SL_full <- colSums(do.call(cbind, sapply(boot_updates, "[", 1)) * pop)
      boot_updates_SL_nosubcat <- colSums(do.call(cbind, sapply(boot_updates, "[", 2)) * pop)
      boot_updates_SL_univar_gam <- colSums(do.call(cbind, sapply(boot_updates, "[", 3)) * pop)
    }
    boot_data_array_full_sl[, i] <- as.vector(boot_updates_SL_full)
    boot_data_array_full_nosubcat[, i] <- as.vector(boot_updates_SL_nosubcat)
    boot_data_array_full_univar_gam[, i] <- as.vector(boot_updates_SL_univar_gam)
    
    CI_boot_full_sl <- quantile(boot_updates_SL_full, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)
    CI_boot_full_nosubcat <- quantile(boot_updates_SL_nosubcat, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)
    CI_boot_full_univar_gam <- quantile(boot_updates_SL_univar_gam, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)
    
    ## update full sl df
    boot_df_sf_full[i, 1] <- perc
    boot_df_sf_full[i, 2] <- CI_boot_full_sl[[2]]
    boot_df_sf_full[i, 3] <- CI_boot_full_sl[[1]]
    boot_df_sf_full[i, 4] <- CI_boot_full_sl[[3]]
    
    ## update subcat sl df
    boot_df_sf_no_subcat[i, 1] <- perc
    boot_df_sf_no_subcat[i, 2] <- CI_boot_full_nosubcat[[2]]
    boot_df_sf_no_subcat[i, 3] <- CI_boot_full_nosubcat[[1]]
    boot_df_sf_no_subcat[i, 4] <- CI_boot_full_nosubcat[[3]]
    
    ## update univar gam df
    boot_df_univar_gam[i, 1] <- perc
    boot_df_univar_gam[i, 2] <- CI_boot_full_univar_gam[[2]]
    boot_df_univar_gam[i, 3] <- CI_boot_full_univar_gam[[1]]
    boot_df_univar_gam[i, 4] <- CI_boot_full_univar_gam[[3]]
  }
  
  return(list(
    "full_sl_results" = list(
      "boot_CI_df_sl_full" = boot_df_sf_full,
      "boot_sl_full_array" = boot_data_array_full_sl
    ),
    "no_subcat_sl_results" = list(
      "boot_CI_df_sl_nosubcat" = boot_df_sf_no_subcat,
      "boot_sl_nosubcat_array" = boot_data_array_full_nosubcat
    ),
    "univar_gam_results" = list(
      "boot_CI_univar_gam" = boot_df_univar_gam,
      "boot_sl_univar_gam_array" = boot_data_array_full_univar_gam
    )
  ))
}


#start_time <- Sys.time()
library(doParallel)
ncores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
ncores
cl <- makeCluster(ncores)
registerDoParallel(ncores)

#all_results <-as.list(rep(NA,5))

start_time <- Sys.time()

# boot_results <- purrr::pmap(list(
#   target_variable = top_vars,
#   ML_pipeline_result = ML_pipeline_results,
#   outcome = target_outcomes,
#   boot_df_sf_full = boot_dfs_sl_full,
#   boot_df_sf_no_subcat = boot_dfs_sl_no_subcat,
#   boot_df_univar_gam = boot_dfs_univar_gam,
#   sub_cat_vars = top_var_subcat_vars
# ),
# .f = bootstrap_marginal_predictions,
# data_original = data_original,
# covars = covars,
# percents = percents,
# pop = data_original$Population,
# boot_num = 300
# )

total_cases_by_minority_boot_results <- bootstrap_marginal_predictions(target_variable = "EPL_MINRTY", 
                               ML_pipeline_result = ML_pipeline_results[[3]],
                               outcome = target_outcomes[[3]],
                               boot_df_sf_full = boot_dfs_sl_full[[2]],
                               boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[2]],
                               boot_df_univar_gam = boot_dfs_univar_gam[[2]],
                               sub_cat_vars = top_var_subcat_vars[[3]],
                               data_original = data_original,
                               covars = covars,
                               percents = percents,
                               pop = data_original$Population,
                               boot_num = 1000)
                               

end_time <- Sys.time()
end_time - start_time

saveRDS(boot_results, here("Analysis/update_data/data/processed/total_cases_by_minority_boot.RDS"))

stopCluster(cl)


# for(it in 1:5){
#   results <- bootstrap_marginal_predictions(target_variable= top_vars[it],
#                                             ML_pipeline_result =ML_pipeline_results[[it]],
#                                             outcome = target_outcomes[it],
#                                             boot_df_sf_full = boot_dfs_sl_full[[it]],
#                                             boot_df_sf_no_subcat = boot_dfs_sl_no_subcat[[it]],
#                                             boot_df_univar_gam = boot_dfs_univar_gam[[it]],
#                                             sub_cat_vars= top_var_subcat_vars[[it]],
#                                             data_original = data_original,
#                                             covars = covars,
#                                             percents = percents,
#                                             pop = data_original$Population,
#                                             boot_num=1000)
#   
#   all_results[[it]] <- results
#   }



