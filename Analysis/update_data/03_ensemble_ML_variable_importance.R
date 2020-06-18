library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)
library(R6)
library(tidyverse)
library(readxl)

## scale data before running ML pipeline? 
scale = TRUE

## load data
covid_data_processed <- read_csv("Analysis/update_data/data/processed/cleaned_covid_data_final.csv")

## source the custom learners built for poisson outcomes
sapply(list.files(path = here("Analysis/poisson_learners"),
                  full.names = TRUE), 
                  source)


covid_data_processed$CountyRelativeDay25Cases_PopScale <- covid_data_processed$CountyRelativeDay25Cases / covid_data_processed$Population 
covid_data_processed$TotalCasesUpToDate_PopScale <- covid_data_processed$TotalCasesUpToDate / covid_data_processed$Population 
covid_data_processed$USRelativeDay100Deaths_PopScale <- covid_data_processed$USRelativeDay100Deaths / covid_data_processed$Population 
covid_data_processed$TotalDeathsUpToDate_PopScale <- covid_data_processed$TotalDeathsUpToDate / covid_data_processed$Population


outcomes <- c("CountyRelativeDay25Cases", 
              "TotalCasesUpToDate", 
              "USRelativeDay100Deaths" , 
              "TotalDeathsUpToDate", 
              "FirstCaseDay",
              "CountyRelativeDay25Cases_PopScale", 
              "TotalCasesUpToDate_PopScale",
              "USRelativeDay100Deaths_PopScale",
              "TotalDeathsUpToDate_PopScale")


covars <- colnames(covid_data_processed)[-which(names(covid_data_processed) %in% c(
  outcomes,
  "X1", 
  "FIPS",
  "FIPS.1",
  "county_names"
))]


## use a function to apply task over multiple outcomes 
run_sl3_poisson_lrns <- function(outcome, 
                                 data, 
                                 covars, 
                                 scale = TRUE, 
                                 cv = TRUE, 
                                 outcome_type = "Gaussian", 
                                 all_outcomes = outcomes) {
  
  if (scale) {
    
    features_data_scaled <- data %>% 
      select(-c(all_outcomes,
                "X1", 
                "FIPS",
                "FIPS.1",
                "county_names")) %>% 
      scale()  %>% 
      as.data.frame()
    
    data <- cbind(data[, outcome], features_data_scaled)

  }
  
  if (cv) {
    
    task <- make_sl3_Task(
    data = data,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(data, fold_fun = folds_vfold, V = 5))
  } else{
  
  task <- make_sl3_Task(
    data = data,
    covariates = covars,
    outcome = outcome)
  }
  
  if (outcome_type == "Poisson") {
    
  
    ## set up the custom learners and some standard ones as well
    ## set up baseline mean to make sure our other learners are working better than mean
    mean_lrnr <- Lrnr_mean$new()
    ## standard poisson GLM I wrote
    Lrnr_david_pois <- make_learner(Lrnr_david_pois)
    
    ## custom xgboost for poisson outcome with varying parameters (should try grid as well)
    Lrnr_david_xgboost_pois_850 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 8,  nrounds = 50)
    Lrnr_david_xgboost_pois_5100 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 5,  nrounds = 100)
    Lrnr_david_xgboost_pois_10200 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 10,  nrounds = 200)
    
    ## custom ridge and lass from glmnet poisson
    ridge_lrnr_pois <- make_learner(Lrnr_david_glmnet_pois,alpha = 0, nfolds = 3)
    lasso_lrnr_pois <- make_learner(Lrnr_david_glmnet_pois,alpha = 1, nfolds = 3)
    
    ## custom gbm and glmnet for poisson with varying parameters
    Lrnr_david_gbm_pois <- make_learner(Lrnr_david_gbm_pois)
    Lrnr_david_glmnet_pois_25 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.25, nfolds = 3)
    Lrnr_david_glmnet_pois_50 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.50, nfolds = 3)
    Lrnr_david_glmnet_pois_75 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.75, nfolds = 3)
    
    ## custom HAL
    Lrnr_david_hal9001_pois <- make_learner(Lrnr_david_hal9001_pois)
    ## custom earth
    #Lrnr_david_earth_pois <- make_learner(Lrnr_david_earth_pois)
    
    ## create the stack of learners 
    stack <- make_learner(
      Stack,
      mean_lrnr,
      #Lrnr_david_pois,
      #Lrnr_david_xgboost_pois_850,
      Lrnr_david_xgboost_pois_5100,
      Lrnr_david_xgboost_pois_10200,
      ridge_lrnr_pois,
      lasso_lrnr_pois,
      Lrnr_david_gbm_pois,
      Lrnr_david_glmnet_pois_25,
      Lrnr_david_glmnet_pois_50,
      Lrnr_david_glmnet_pois_75) #Lrnr_david_earth_pois isn't working and leave out HAL for time restraints but should try
  } else {
  # choose base learners
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_mean <- make_learner(Lrnr_mean)
  
  lrnr_ranger10 <- make_learner(Lrnr_ranger, num.trees = 10)
  lrnr_ranger50 <- make_learner(Lrnr_ranger, num.trees = 50)
  lrnr_hal_simple <- make_learner(Lrnr_hal9001, max_degree = 2, n_folds = 2)
  lrnr_lasso <- make_learner(Lrnr_glmnet) # alpha default is 1
  lrnr_ridge <- make_learner(Lrnr_glmnet, alpha = 0)
  lrnr_elasticnet <- make_learner(Lrnr_glmnet, alpha = .5)
  
  #lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
  
  # I like to crock pot my super learners
  grid_params <- list(max_depth = c(2, 4, 6, 8, 10, 12),
                      eta = c(0.001, 0.01, 0.1, 0.2, 0.3),
                      nrounds = c(20, 50))
  grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
  params_default <- list(nthread = getOption("sl.cores.learners", 1))
  xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
    do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))})
  
  
  stack <- make_learner(
    Stack,
    lrnr_glm, 
    lrnr_mean, 
    lrnr_ridge, 
    lrnr_elasticnet, 
    lrnr_lasso, 
    xgb_learners[[31]], 
    xgb_learners[[32]], 
    xgb_learners[[33]],
    xgb_learners[[34]],
    xgb_learners[[40]],
    lrnr_ranger10, 
    xgb_learners[[50]], 
    xgb_learners[[60]]
  )
  }
  
  
  sl <- make_learner(Lrnr_sl,
                     learners = stack
  )
  ## fit the sl3 object
  sl_fit <- sl$train(task)
  
  ## cross validate across the learners in sl3 object to see how learners perform 
  CVsl <- CV_lrnr_sl(sl_fit, task, loss_squared_error)
  
  ## get variable importance from the sl3 object
  var_importance <- varimp(sl_fit, loss_squared_error, type = "ratio")
  
  return(list('fit' = sl_fit, 'cv_fit' = CVsl, 'var_imp' = var_importance))
}

ptm <- proc.time()
ML_pipeline_output <- map(.x = outcomes[5:length(outcomes)], 
                          .f = run_sl3_poisson_lrns, 
                          data = covid_data_processed, 
                          covars = covars,
                          scale = TRUE,
                          cv = TRUE,
                          outcome_type = "Gaussian",
                          all_outcomes = outcomes)
proc.time() - ptm

saveRDS(ML_pipeline_output, here("Analysis/update_data/data/processed/ML_pipeline_5_outcomes.RDS"))

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

plot_variable_importance(input_df = ML_pipeline_output[[1]], plot_label = "Day First Case Outcome", save_label = "day_first_case.pdf")
plot_variable_importance(input_df = ML_pipeline_output[[2]], plot_label = "Cases Rate at Day 25 Outcome", save_label = "day_25_cases.pdf")
plot_variable_importance(input_df = ML_pipeline_output[[3]], plot_label = "Cases Rate Total To Date Outcome", save_label = "tota_cases_2date.pdf")
plot_variable_importance(input_df = ML_pipeline_output[[4]], plot_label = "Mortality at Day 100 Rate Outcome", save_label = "day_100_mortality.pdf")
plot_variable_importance(input_df = ML_pipeline_output[[5]], plot_label = "Mortality Total Rate Outcome", save_label = "total_deaths_2date.pdf")



## if things need to be done manually, i.e. running the script within the function rather than running the whole function plotting variable importance dataframe generated 
## can be plotted here: 


#var_importance <- as.data.frame(input_df$var_imp)
colnames(var_importance) <- c("County_Features", "Risk_Ratio")

var_importance$County_Features <- as.factor(var_importance$County_Features)
var_importance$County_Features <- factor(var_importance$County_Features, levels = var_importance$County_Features[order(var_importance$Risk_Ratio)])

var_imp_plot <- var_importance %>%
  filter(Risk_Ratio > 1.01)  %>%
  ggplot(aes(x = County_Features, y = Risk_Ratio)) +
  geom_dotplot(binaxis = "y", dotsize = 0.25) +
  labs(x = "County Level Feature", y = "Risk Ratio", 
       title = "Day First Case") +  ## you will need to change the title
  coord_flip() +
  theme_bw()

print(var_imp_plot)
