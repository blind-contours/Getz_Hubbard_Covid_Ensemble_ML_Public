`%notin%` <- Negate(`%in%`)


## changed the varimp function to do subcategories
varimp_subcat <- function (fit, 
                           loss, 
                           variable_list, 
                           subcategory_list,
                           fold_number = "validation", 
                           type = c("ratio", "difference")) 
{
  type <- match.arg(type)
  task <- fit$training_task
  dat <- task$data
  X <- task$nodes$covariates
  Y <- task$Y
  subcategories <- unique(subcategory_list)
  preds <- fit$predict_fold(task, fold_number = fold_number)
  risk <- mean(loss(preds, Y))
  importance <- lapply(subcategories, function(i) {
    #browser()
    subcat_vars <- variable_list[which(subcategory_list %in% i)]
    scrambled_cols <- as.data.frame(map(.x =subcat_vars, ~data.table(sample(unlist(dat[, subcat_vars, with = FALSE]), nrow(dat)))))
    names(scrambled_cols) <- subcat_vars
    scrambled_col_names <- task$add_columns(scrambled_cols)
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
plot_variable_importance <- function(input_df, plot_label, save_label){
  
  var_importance <- as.data.frame(input_df$var_imp)
  colnames(var_importance) <- c("County_Features", "Risk_Ratio")
  
  var_importance$County_Features <- as.factor(var_importance$County_Features)
  var_importance$County_Features <- factor(var_importance$County_Features, levels = var_importance$County_Features[order(var_importance$Risk_Ratio)])
  
  pdf(paste(here("Visulizations/var_imp/"),save_label , sep = ""))
  
  var_imp_plot <- var_importance %>%
    filter(Risk_Ratio > 1.01)  %>%
    ggplot(aes(x = County_Features, y = Risk_Ratio)) +
    geom_dotplot(binaxis = "y", dotsize = 0.25) +
    labs(x = "County Level Feature", y = "Risk Ratio", 
         title = plot_label) + 
    coord_flip() +
    theme_bw()
  
  print(var_imp_plot)
  
  dev.off()
  
}



Data_Dictionary <- read_excel("Analysis/update_data/data/processed/Data_Dictionary.xlsx")
Data_Dictionary_Used <- Data_Dictionary %>% filter(Keep == "Yes") %>% select(`Variable Name`, `Sub-Category`)
##remove from the list covariates that had too many NAs and were then dropped before analysis, FIPS, and the outcome data:
removing <- c("HIV.prevalence.raw.value", 
                "prev_2017_over_65_Alzheimer.s.Disease.Dementia", 
                "seg_index",
                "occ_other_services",
                "FIPS",
                "CountyRelativeDay25Cases",
                "TotalCasesUpToDate",
                "USRelativeDay100Deaths", 
                "TotalDeathsUpToDate",
                "FirstCaseDay")

Data_Dictionary_Used <- Data_Dictionary_Used[-match(removing , Data_Dictionary_Used$`Variable Name`),]
variable_list <-  Data_Dictionary_Used$`Variable Name`
subcategory_list <- Data_Dictionary_Used$`Sub-Category`

varimp_subcat(fit = ML_pipeline_output[[1]]$fit,
              loss = loss_squared_error, 
              variable_list = variable_list,
              subcategory_list =subcategory_list,
              fold_number = "validation", 
              type = "ratio")

varimp_subcat(fit = ML_pipeline_output[[2]]$fit,
              loss = loss_squared_error, 
              variable_list = variable_list,
              subcategory_list =subcategory_list,
              fold_number = "validation", 
              type = "ratio")

