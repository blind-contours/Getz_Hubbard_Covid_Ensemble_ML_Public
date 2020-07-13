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

`%notin%` <- Negate(`%in%`)


scale <- FALSE
## read in processsed dataframe and load the ML pipeline results
data_original <- read_csv("Analysis/update_data/data/processed/cleaned_covid_data_final.csv")
ML_pipeline_results <- readRDS(here("Analysis/update_data/data/processed/ML_pipeline_5_outcomes_noscale_july10.RDS"))

## read in data dictionary for identifying subgroups of top variables to isolate the different control conditions
Data_Dictionary <- read_excel("Analysis/update_data/data/processed/Data_Dictionary.xlsx")
Data_Dictionary_Used <- Data_Dictionary %>%
  filter(Keep == "Yes") %>%
  select(`Variable Name`, `Sub-Category`)
## remove from the list covariates that had too many NAs and were then dropped before analysis, FIPS, and the outcome data:

#vars_rmv_na <- colnames(data_original)[names(data_original) %notin% Data_Dictionary_Used$`Variable Name`]

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
percents <- c(0.0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)

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
    "Boot High")
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
                        ML_pipeline_results,
                        covars,
                        outcome,
                        perc,
                        sub_cat_vars) {
  
  # if (target_variable=="EPL_LIMENG") { 
  #   browser()
  #   }
  
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
  ## create another SL task but remove all other variables in the target variable subcategory and only include the target variable
  #browser()
  resampled_data_task_no_subcat_covars <- make_sl3_Task(
    data = resampled_data,
    covariates = c(covars[covars %notin% sub_cat_vars], target_variable),
    outcome = outcome,
    folds = origami::make_folds(resampled_data,
      fold_fun = folds_vfold, V = 2
    )
  )

  ## train a univariate gam on the resampled data
  univar_gam_model <- gam(as.formula(paste(outcome, target_variable, sep = "~")), data = resampled_data)

  ## train the superlearner on the resampled data
  sl_fit_full_resampled <- sl$train(resampled_data_task)
  sl_fit_nosubcat_resampled <- sl$train(resampled_data_task_no_subcat_covars)

  ## get the original data and reduce the target variable by perc
  data_resampled_reduced <- data_original
  data_resampled_reduced[, target_variable] <- data_resampled_reduced[, target_variable] - (data_resampled_reduced[, target_variable] * perc)

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
  #predict from no subcategories
  sl_preds_reduced_no_subcat <- sl_fit_nosubcat_resampled$predict(resampled_data_reduced_task_no_subcat_covars)
  #predict from univariate gam
  univariate_gam_predictions <- stats::predict(object = univar_gam_model, newdata = data_resampled_reduced)

  results <- data.frame(sl_preds_reduced_full, sl_preds_reduced_no_subcat, as.vector(univariate_gam_predictions))
  colnames(results) <- c("SL_full_model", "SL_no_tgt_subcat_vars", "univariate_gam")
  
  return(results)
}
  
#   return(list("SL_full" = sl_preds_reduced_full, 
#               "SL_no_subcat" = sl_preds_reduced_no_subcat, 
#               "univar_gam" = as.vector(univariate_gam_predictions)))
# }


## run marginal predictions for each decrease in target variable from variable importance calculations
bootsrap_marginal_predictions <- function(target_variable,
                                          ML_pipeline_results,
                                          outcome,
                                          boot_df_sf_full,
                                          boot_df_sf_no_subcat,
                                          boot_df_univar_gam,
                                          sub_cat_vars,
                                          data_original,
                                          covars,
                                          percents,
                                          pop,
                                          boot_num) {
  
  #browser()
  sl_fit <- ML_pipeline_results$fit

  initialize_data <- rep(NaN, dim(data_original)[1] * boot_num * length(percents))
  
  boot_data_array_full_sl <- array(initialize_data, c(length(percents), dim(data_original)[1], boot_num))
  boot_data_array_full_nosubcat <- array(initialize_data, c(length(percents), dim(data_original)[1], boot_num))
  boot_data_array_full_univar_gam <- array(initialize_data, c(length(percents), dim(data_original)[1], boot_num))
  

  for (i in 1:length(percents)) {
    perc <- percents[i]
    #data_temp <- data_original
    #data_temp[, target_variable] <- data_original[, target_variable] - (data_original[, target_variable] * perc)

    # task_reduced_initial_est <- make_sl3_Task(
    #   data = data_temp,
    #   covariates = covars,
    #   outcome = outcome,
    #   folds = origami::make_folds(data_temp,
    #     fold_fun = folds_vfold, V = 2
    #   )
    # )
    # 
    # sl_preds_reduced <- sl_fit$predict(task_reduced_initial_est)
    

    boot_updates <- replicate(boot_num, bootstrapCI(
      target_variable = target_variable,
      data_original = data_original,
      ML_pipeline_results = ML_pipeline_results,
      covars = covars,
      outcome = outcome,
      perc = perc,
      sub_cat_vars = sub_cat_vars
    ), simplify = FALSE
    )

    if (outcome == "FirstCaseDay") {
      ## initial model predictions ,take mean days
      #SL_full_initial_total_counts <- mean(sl_preds_reduced)
      
      ## reformat and extract the bootstrap results
      boot_totals <- as.data.frame(t(colMeans(bind_rows(boot_updates))))
      total_counts_SL_full <- boot_totals$SL_full_model
      total_counts_SL_no_subcat <- boot_totals$SL_no_tgt_subcat_vars
      total_counts_univariate_gam <- boot_totals$univariate_gam
      
      boot_updates_SL_full <- do.call(cbind,sapply(boot_updates,"[",1)) ## wish I didn't have to manually index here but in a hurry
      boot_updates_SL_nosubcat <- do.call(cbind,sapply(boot_updates,"[",2))
      boot_updates_SL_univar_gam <- do.call(cbind,sapply(boot_updates,"[",3))
      
    } else {
      #browser()
      total_totals <- as.data.frame(t(colSums(bind_rows(boot_updates) * pop)))
      total_counts_SL_full <- total_totals$SL_full_model
      total_counts_SL_no_subcat <- total_totals$SL_no_tgt_subcat_vars
      total_counts_univariate_gam <- total_totals$univariate_gam
      
      boot_updates_SL_full <- do.call(cbind,sapply(boot_updates,"[",1)) * pop  
      boot_updates_SL_nosubcat <- do.call(cbind,sapply(boot_updates,"[",2)) * pop  
      boot_updates_SL_univar_gam <- do.call(cbind,sapply(boot_updates,"[",3))* pop  
      
    }

    boot_data_array_full_sl[i, , ] <- boot_updates_SL_full
    boot_data_array_full_nosubcat[i, , ] <- boot_updates_SL_nosubcat
    boot_data_array_full_univar_gam[i, , ] <- boot_updates_SL_univar_gam

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

  return(list("full_sl_results" = list("boot_CI_df_sl_full" = boot_df_sf_full, 
                                       "boot_sl_full_array" = boot_data_array_full_sl), 
              "no_subcat_sl_results" = list("boot_CI_df_sl_nosubcat" = boot_df_sf_no_subcat, 
                                            "boot_sl_nosubcat_array" = boot_data_array_full_nosubcat),
              "univar_gam_results" = list("boot_CI_univar_gam" = boot_df_univar_gam, 
                                          "boot_sl_univar_gam_array" = boot_data_array_full_univar_gam)))
}



boot_results <- pmap(list(
  target_variable = top_vars,
  ML_pipeline_results = ML_pipeline_results,
  outcome = target_outcomes,
  boot_df_sf_full = boot_dfs_sl_full,
  boot_df_sf_no_subcat = boot_dfs_sl_no_subcat,
  boot_df_univar_gam = boot_dfs_univar_gam,
  sub_cat_vars = top_var_subcat_vars
),
.f = bootsrap_marginal_predictions,
data_original = data_original,
covars = covars,
percents = percents,
pop = data_original$Population,
boot_num = 1
)

boot_diff_results_cmpr_prev <- list(
  "FirstCaseDay_boot_change" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "CountyRelativeDay25Cases_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "TotalCasesUpToDate_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "USRelativeDay100Deaths_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "TotalDeathsUpToDate_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9))
)

boot_diff_results_cmpr_base <- list(
  "FirstCaseDay_boot_change" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "CountyRelativeDay25Cases_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "TotalCasesUpToDate_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "USRelativeDay100Deaths_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9)),
  "TotalDeathsUpToDate_PopScale" = as.data.frame(matrix(ncol = 3, nrow = 9))
)


extract_perc_change_diffs <- function(boot_results,
                                      outcome,
                                      boot_diff_results_cmpr_prev,
                                      boot_diff_results_cmpr_base,
                                      pop = data_original$Population) {
  boot_array <- boot_results[[2]]
  no_reduction_boot_data <- boot_array[1, , ]
  # browser()
  # first slice is bootstrap predictions for 0 perc reduction, second slice is for 10 percent reduction... etc.
  for (i in 2:dim(boot_array)[1]) {
    target_perc_boot_results <- boot_array[i, , ]
    frm_target_perc_boot_results <- boot_array[i - 1, , ]
    boot_change_df <- target_perc_boot_results - frm_target_perc_boot_results
    boot_change_frm_base <- target_perc_boot_results - no_reduction_boot_data

    if (outcome == "FirstCaseDay") {
      CI_boot_cmpr_prev <- quantile(boot_change_df, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)
      CI_boot_cmpr_base <- quantile(boot_change_frm_base, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)
    } else {
      boot_change_sums_df <- colSums(boot_change_df)
      boot_change_sums_frm_base_df <- colSums(boot_change_frm_base)

      CI_boot_cmpr_prev <- quantile(boot_change_sums_df, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)
      CI_boot_cmpr_base <- quantile(boot_change_sums_frm_base_df, probs = c(0.025, 0.50, 0.975), na.rm = TRUE)
    }

    boot_diff_results_cmpr_prev[i, ] <- CI_boot_cmpr_prev
    boot_diff_results_cmpr_base[i, ] <- CI_boot_cmpr_base
  }
  return(list("Compared Previous" = boot_diff_results_cmpr_prev, "Compared to Base" = boot_diff_results_cmpr_base))
}


boot_change_results <- pmap(list(
  boot_results,
  target_outcomes,
  boot_diff_results_cmpr_prev,
  boot_diff_results_cmpr_base
),
.f = extract_perc_change_diffs,
pop = data_original$Population
)



plot_perc_change_diffs <- function(boot_change_results,
                                   outcome,
                                   predictors) {
  # browser()
  cmpr_previous <- boot_change_results$`Compared Previous`[-1, ]
  cmpr_base <- boot_change_results$`Compared to Base`[-1, ]
  xlabel <- as.character(predictors)
  title <- as.character(outcome)

  x_prev_lbs <- as.factor(c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%"))
  x_base_lbs <- as.factor(c("0-10%", "0-20%", "0-30%", "0-40%", "0-50%", "0-60%", "0-70%", "0-80%", "0-90%"))

  plot_cmpr_prev <- ggplot(cmpr_previous, aes(x = x_prev_lbs, y = V2)) +
    geom_errorbar(aes(ymin = V1, ymax = V3), width = .1) +
    geom_line() +
    geom_point() +
    xlab(xlabel) +
    ylab("Predicted Difference") +
    ggtitle(title)

  plot_cmpr_base <- ggplot(cmpr_base, aes(x = x_base_lbs, y = V2)) +
    geom_errorbar(aes(ymin = V1, ymax = V3), width = .1) +
    geom_line() +
    geom_point() +
    xlab(xlabel) +
    ylab("Predicted Difference from Baseline") +
    ggtitle(title)

  return(list(plot_cmpr_prev, plot_cmpr_base))
}

plots <- pmap(list(
  boot_change_results,
  outcome = target_outcomes,
  predictors = top_vars
),
.f = plot_perc_change_diffs
)


## plot results


plot_bootstrap_results <- function(boot_results, target_outcomes) {
  browser()
  boot_results <- boot_results[[1]]
  target_variable <- names(boot_results)[1]
  xlabel <- as.character(target_variable)

  file_name <- paste(target_outcomes, "_marginal_predictions_", xlabel, ".png", sep = "")

  plot <- ggplot(boot_results, aes(x = boot_results[, target_variable], y = `Boot Pred`)) +
    geom_errorbar(aes(ymin = `Boot Low`, ymax = `Boot High`), width = .1) +
    geom_line() +
    geom_point() +
    xlab(xlabel) +
    ggtitle(target_outcomes)


  ggsave(
    filename = file_name,
    plot = plot,
    device = NULL,
    path = here("/Visulizations/marginal_predictions"),
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE
  )
}

walk2(
  .x = boot_results,
  .y = target_outcomes,
  .f = plot_bootstrap_results
)
