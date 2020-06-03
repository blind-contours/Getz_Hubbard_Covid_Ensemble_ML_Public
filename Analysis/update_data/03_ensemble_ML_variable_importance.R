library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)
library(R6)
library(imputeTS)
library(tidyverse)
library(readxl)

## scale data before running ML pipeline? 
scale = TRUE

## load data
covid_data_processed <- read_excel(here("Analysis/update_data/data/processed/cleaned_covid_data_final.xlsx"),sheet = 1)
#test_previous_data <- readRDS('/Users/davidmccoy/Dropbox/COVID_Project_Getz-Hubbard/Data_Files/Compiled_Data/Covid_DataSet_0421.rds')

## source the custom learners built for poisson outcomes
sapply(list.files(path = here("Analysis/poisson_learners"),
                  full.names = TRUE), 
                  source)


outcomes <- c("CountyRelativeDay25Cases", "TotalCasesUpToDate", "USRelativeDay100Deaths" , "TotalDeathsUpToDate", "FirstCaseDay")

covars <- colnames(covid_data_processed)[-which(names(covid_data_processed) %in% c(outcomes, 
                                                             "X1", 
                                                             "X1_1", 
                                                             "FIPS", 
                                                             "county_names",
                                                             "Row Label",
                                                             'total.population.2018'))]
features_data <- covid_data_processed %>% 
  select(-c(outcomes, 'Row Label', 'FIPS', 'county_names'))

## use a function to apply task over multiple outcomes 
run_sl3_poisson_lrns <- function(outcome, data, covars, scale = TRUE){
  
  if (scale) {
    
    features_data_scaled <- covid_data_processed %>% 
      select(-c(outcomes, 'Row Label', 'FIPS', 'county_names', 'total.population.2018')) %>% 
      scale()  %>% 
      as.data.frame()
    
    data <- cbind(data[, outcome], features_data_scaled)

  }
  
  ## impute mean for NA to avoid funciton failing with sl3 imputation warning: 
  
  ## create task - most outcomes only have 1 or 2 NAs so setting drop outcome to true to avoid breaking training
  task_cv <- make_sl3_Task(
    data = data,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(data, fold_fun = folds_vfold, V = 5),
    drop_missing_outcome=TRUE
  )
  
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
    Lrnr_david_pois,
    Lrnr_david_xgboost_pois_850,
    Lrnr_david_xgboost_pois_5100,
    Lrnr_david_xgboost_pois_10200,
    ridge_lrnr_pois,
    lasso_lrnr_pois,
    Lrnr_david_gbm_pois,
    Lrnr_david_glmnet_pois_25,
    Lrnr_david_glmnet_pois_50,
    Lrnr_david_glmnet_pois_75) #Lrnr_david_earth_pois isn't working and leave out HAL for time restraints but should try
  
  sl <- make_learner(Lrnr_sl,
                     learners = stack
  )
  ## fit the sl3 object
  sl_fit <- sl$train(task_cv)
  
  ## cross validate across the learners in sl3 object to see how learners perform 
  CVsl <- CV_lrnr_sl(sl_fit, task_cv, loss_squared_error)
  
  ## get variable importance from the sl3 object
  var_importance <- varimp(sl_fit, loss_squared_error, type = "ratio")
  
  return(list('fit' = sl_fit, 'cv_fit' = CVsl, 'var_imp' = var_importance))
}

outcome <- "ConfirmedCasesDay25"
covars <- colnames(test_previous_data)[-which(names(test_previous_data) %in% c(outcome, 
                                                             "State", 
                                                             "FIPS", 
                                                             "Name", 
                                                             "popland", 
                                                             "GDP", 
                                                             "CommutingByPublicTransportation",
                                                             "Population"
))]


test_output <- run_sl3_poisson_lrns(outcome = outcome, data = test_previous_data, covars = covars)

ptm <- proc.time()
output1 <- run_sl3_poisson_lrns(outcome = outcomes[1], data = covid_data_processed, covars = covars)
output2 <- run_sl3_poisson_lrns(outcome = outcomes[2], data = covid_data_processed, covars = covars)
output3 <- run_sl3_poisson_lrns(outcome = outcomes[3], data = covid_data_processed, covars = covars)
output4 <- run_sl3_poisson_lrns(outcome = outcomes[4], data = covid_data_processed, covars = covars)
#output <- map(.x = outcomes, .f = run_sl3_poisson_lrns(outcome = x, data = covid_data_processed, covars = covars))
proc.time() - ptm


var_importance %>%
  mutate(name = forcats::fct_reorder(X, risk_ratio)) %>%
  ggplot(aes(x = risk_ratio, y = X)) +
  geom_dotplot(binaxis = "y", dotsize = 0.5) +
  labs(x = "Risk Ratio", y = "Covariate", 
       title = "sl3 Variable Importance for COVID Day 25 Cases")

var_importance %>%
  arrange(risk_ratio) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(X, levels=X)) %>%   # This trick update the factor levels
  ggplot( aes(x=X, y=risk_ratio)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")


                                                            
