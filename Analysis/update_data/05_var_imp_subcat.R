library(here)
library(readxl)
library(dplyr)
library(purrr)
library(sl3)
library(tidyr)
library(data.table)
library(gbm)
library(ggplot2)
## changed the varimp function to do subcategories

varimp_subcat <- function (.x, 
                           loss, 
                           variable_list, 
                           subcategory_list,
                           fold_number = "validation", 
                           type = c("ratio", "difference")) 
{
  #browser()
  fit <- .x$fit
  type <- match.arg(type)
  task <- fit$training_task
  dat <- task$data
  X <- task$nodes$covariates
  Y <- task$Y
  subcategories <- unique(subcategory_list)
  preds <- fit$predict_fold(task, fold_number = fold_number)
  risk <- mean(loss(preds, Y))
  importance <- lapply(subcategories, function(i) {
    subcat_vars <- variable_list[which(subcategory_list %in% i)]
    scrambled_rows <- dat[sample(nrow(dat)), ]
    scrambled_rows_selection <- scrambled_rows %>% dplyr::select(!!subcat_vars)
    scrambled_col_names <- task$add_columns(scrambled_rows_selection)
    scrambled_col_task <- task$next_in_chain(column_names = scrambled_col_names)
    scrambled_sl_preds <- fit$predict_fold(scrambled_col_task, 
                                           fold_number)

    risk_scrambled <- mean(loss(scrambled_sl_preds, Y))
    if (type == "ratio") {
      varimp_metric <- risk_scrambled/risk
    }
    else if (type == "difference") {
      varimp_metric <- risk_scrambled - risk
    }
    return(varimp_metric)
  })
  #browser()
  names(importance) <- subcategories
  if (type == "ratio") {
    results <- data.table(X = names(importance), risk_ratio = unlist(importance))
    results_ordered <- results[order(-results$risk_ratio)]
  }
  else if (type == "difference") {
    results <- data.table(X = names(importance), risk_difference = unlist(importance))
    results_ordered <- results[order(-results$risk_difference)]
  }
  return(results_ordered)
}

## plotting results
plot_variable_importance_for_cat <- function(input_df, plot_label, save_label){
  var_importance <- input_df
  colnames(var_importance) <- c("County_Features", "Risk_Ratio")
  
  var_importance$County_Features <- as.factor(var_importance$County_Features)
  var_importance$County_Features <- factor(var_importance$County_Features, levels = var_importance$County_Features[order(var_importance$Risk_Ratio)])
  
  pdf(paste(here("Visulizations/var_imp/"),save_label , sep = ""))
  
  var_imp_plot <- var_importance %>%
    ggplot(aes(x = County_Features, y = Risk_Ratio)) +
    geom_dotplot(binaxis = "y", dotsize = 0.25) +
    labs(x = "County Level Feature", y = "Risk Ratio", 
         title = plot_label) + 
    coord_flip() +
    theme_bw()
  
  print(var_imp_plot)
  
  dev.off()
  
}
ML_pipeline_results <- readRDS(here("Analysis/update_data/data/processed/ML_pipeline_5_outcomes_noscale_july16.RDS"))
Data_Dictionary <- read_excel(here("Analysis/update_data/data/processed/Data_Dictionary.xlsx"))
Data_Dictionary_Used <- Data_Dictionary %>% filter(Keep == "Yes") %>% select(`Variable Name`, `Sub-Category`)

vars_rmv_na <- read.csv(here("Analysis/update_data/data/processed/vars_removed_na_thresh.csv"))
##remove from the list covariates that had too many NAs and were then dropped before analysis, FIPS, and the outcome data:
removing <- c(vars_rmv_na$x,
                "FIPS",
                "occ_all_private",
                "CountyRelativeDay25Cases",
                "TotalCasesUpToDate",
                "USRelativeDay100Deaths", 
                "TotalDeathsUpToDate",
                "FirstCaseDay")

Data_Dictionary_Used <- Data_Dictionary_Used[-match(removing , Data_Dictionary_Used$`Variable Name`),]
variable_list <-  Data_Dictionary_Used$`Variable Name`
subcategory_list <- Data_Dictionary_Used$`Sub-Category`

var_imp_by_categories <- purrr::map(.x = ML_pipeline_results, 
    .f = varimp_subcat, 
    loss = loss_squared_error, 
    variable_list = variable_list,
    subcategory_list =subcategory_list,
    fold_number = "validation", 
    type = "ratio")


plot_variable_importance_for_cat(input_df = var_imp_by_categories[[1]], plot_label = "Day First Case Outcome by Cat", save_label = "day_first_case_x_cat.pdf")
plot_variable_importance_for_cat(input_df = var_imp_by_categories[[2]], plot_label = "Cases Rate at Day 25 Outcome by Cat", save_label = "day_25_cases_x_cat.pdf")
plot_variable_importance_for_cat(input_df = var_imp_by_categories[[3]], plot_label = "Cases Rate Total To Date Outcome by Cat", save_label = "tota_cases_2date_x_cat.pdf")
plot_variable_importance_for_cat(input_df = var_imp_by_categories[[4]], plot_label = "Mortality at Day 100 Rate Outcome by Cat", save_label = "day_100_mortality_x_cat.pdf")
plot_variable_importance_for_cat(input_df = var_imp_by_categories[[5]], plot_label = "Mortality Total Rate Outcome by Cat", save_label = "total_deaths_2date_x_cat.pdf")





