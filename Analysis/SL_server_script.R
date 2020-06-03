library(sl3)
library(origami)
library(dplyr)
library(knitr)
library(ggplot2)
library(here)

#readRDS('/home/david_mccoy/R_scripts/covid/data/Covid_DataSet_0421.rds')
data_used <- readRDS('/Users/davidmccoy/Dropbox/COVID_Project_Getz-Hubbard/Data_Files/Compiled_Data/Covid_DataSet_0421.rds')
names(data_used) <- gsub(" ", "_", names(data_used))
names(data_used) <- gsub("%", "Perc", names(data_used))

data_used$log_pop_dense <- log(data_used$popland+1)
data_used$log_GDP <- log(data_used$GDP+1)
data_used$log_Pub_Trans <- log(data_used$CommutingByPublicTransportation + 1)
data_used$log_Pop <- log(data_used$Population + 1)


##set up node list

# specify the outcome and covariates
outcome <- "ConfirmedCasesDay25"
covars <- colnames(data_used)[-which(names(data_used) %in% c(outcome, 
                                                             "State", 
                                                             "FIPS", 
                                                             "Name", 
                                                             "popland", 
                                                             "GDP", 
                                                             "CommutingByPublicTransportation",
                                                             "Population"
                                                             ))]
Population <- "log_Pop"

# create the sl3 task
day25_cases_task <- make_sl3_Task(
  data = data_used,
  covariates = covars,
  outcome = outcome,
  offset = Population
)

day25_cases_task_cv <- make_sl3_Task(
  data = data_used,
  covariates = covars,
  outcome = outcome,
  folds = origami::make_folds(data_used, fold_fun = folds_vfold, V = 2)
)



# choose y learners 
mean_lrnr <- Lrnr_mean$new()
Lrnr_david_pois <- make_learner(Lrnr_david_pois)

Lrnr_david_xgboost_pois_850 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 8,  nrounds = 50)
Lrnr_david_xgboost_pois_5100 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 5,  nrounds = 100)
Lrnr_david_xgboost_pois_10200 <- make_learner(Lrnr_david_xgboost_pois, max_depth = 10,  nrounds = 200)

ridge_lrnr_pois <- make_learner(Lrnr_david_glmnet_pois,alpha = 0, nfolds = 3)
lasso_lrnr_pois <- make_learner(Lrnr_david_glmnet_pois,alpha = 1, nfolds = 3)


Lrnr_david_gbm_pois <- make_learner(Lrnr_david_gbm_pois)
Lrnr_david_glmnet_pois_25 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.25, nfolds = 3)
Lrnr_david_glmnet_pois_50 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.50, nfolds = 3)
Lrnr_david_glmnet_pois_75 <- make_learner(Lrnr_david_glmnet_pois, alpha = 0.75, nfolds = 3)

Lrnr_david_hal9001_pois <- make_learner(Lrnr_david_hal9001_pois)

Lrnr_david_earth_pois <- make_learner(Lrnr_david_earth_pois)

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
  Lrnr_david_glmnet_pois_75) #Lrnr_david_earth_pois isn't working

#Lrnr_screener_corP <- make_learner(Lrnr_screener_corP, minscreen = 10, minPvalue = 0.05)
# which covariates are selected on the full data?
#Lrnr_screener_corP$train(day25_cases_task_cv)

#Lrnr_screener_corP <- make_learner(Pipeline, Lrnr_screener_corP, stack)

#fancy_stack <- make_learner(Stack, Lrnr_screener_corP, stack)

sl <- make_learner(Lrnr_sl,
                   learners = stack
)

sl_fit <- sl$train(day25_cases_task_cv)
sl_preds<- sl_fit$predict()
head(sl_preds)


## set up bootstrap CI function
bootstrapCI <- function(perc, boot_pred_data) {
  nr <- nrow(boot_pred_data)
  data <- boot_pred_data
  new_data <- data[sample(1:nr, size = nr, replace = TRUE), ]
  
  day25_cases_task_cv <- make_sl3_Task(
    data = new_data,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(data_used, fold_fun = folds_vfold, V = 2)
  )
  
  sl_fit <- sl$train(day25_cases_task_cv)
  
  boot_pred_data[,"log_Pub_Trans"] <- boot_pred_data$log_Pub_Trans - (boot_pred_data$log_Pub_Trans*perc)
  
  day25_cases_task_cv <- make_sl3_Task(
    data = boot_pred_data,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(data_used, fold_fun = folds_vfold, V = 2)
  )
  
  sl_preds_reduced <- sl_fit$predict(day25_cases_task_cv_test)
  sum_cases <- sum(sl_preds_reduced)

  return(sum_cases)
}


#test perc reduced
percents <- c(0.0, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90) 

results_day25 <- as.data.frame(matrix(nrow = length(percents), ncol = 5))
colnames(results_day25) <- c('Percent Pub Trans Reduction',
                                 'Boot Pred', 
                                 'Boot Low' , 
                                 'Boot High', 
                                 'Init. Model Pred')

for (i in 1:length(percents)) {
  
  perc <- percents[i]
  data_temp <- data_used
  data_temp$log_Pub_Trans <- data_used$log_Pub_Trans - (data_used$log_Pub_Trans*perc)

  day25_cases_task_cv_test <- make_sl3_Task(
  data = data_temp,
  covariates = covars,
  outcome = outcome,
  folds = origami::make_folds(data_used, fold_fun = folds_vfold, V = 2))
  
  sl_preds_reduced <- sl_fit$predict(day25_cases_task_cv_test)
  sum_cases <- sum(sl_preds_reduced)
  
  boot_day25 <- replicate(10, bootstrapCI(perc = perc, 
                                            boot_pred_data = data_used))
  
  CI_boot_day25 <- quantile(boot_day25, probs = c(0.025,0.50, 0.975), na.rm = TRUE)
  
  results_day25[i,1] <- perc
  results_day25[i,2] <- CI_boot_day25[[2]]
  results_day25[i,3] <- CI_boot_day25[[1]]
  results_day25[i,4] <- CI_boot_day25[[3]]
  results_day25[i,5] <- sum_cases
}

results_day25_SL <- results_day25
results_day25_SL$type = 'Super Learner'
joint_data <- rbind(results_day25_SL, results_day25_pois)

ggplot(joint_data, aes(x=`Percent Pub Trans Reduction`, y=`Boot Pred`, group = type, colour=type)) + 
  geom_errorbar(aes(ymin=`Boot Low`, ymax=`Boot High`), width=.1) +
  geom_line() +
  geom_point() + 
  ylab('Number of Cases at day 25') + 
  xlab('Fraction Reduced')

ggplot(pois_SL_day25, aes(x=`Percent Pub Trans Reduction`, y=`Boot Pred`, group = type, colour=type)) + 
  geom_errorbar(aes(ymin=`Boot Low`, ymax=`Boot High`), width=.1) +
  geom_line() +
  geom_point() + 
  ylab('Number of Cases at day 25') + 
  xlab('Fraction Reduced')

ggplot(results_day25_SL, aes(x=`Percent Pub Trans Reduction`, y=`Init. Model Pred`)) + 
  geom_errorbar(aes(ymin=`Boot Low`, ymax=`Boot High`), width=.1) +
  geom_line() +
  geom_point() + 
  ylab('Number of Cases at day 25') + 
  xlab('Fraction Reduced')


#test_cases_task <- generate_task(data_temp, "ConfirmedCasesDay25")

head(sl_preds_reduced)
sl_fit_summary <- sl_fit$print()


CVsl <- CV_lrnr_sl(sl_fit, day25_cases_task_cv, loss_squared_error)


day25_varimp <- varimp(sl_fit, loss_squared_error, type = "difference")

# plot variable importance
day25_varimp %>%
  mutate(name = forcats::fct_reorder(X, risk_difference)) %>%
  ggplot(aes(x = risk_difference, y = name)) +
  geom_dotplot(binaxis = "y", dotsize = 0.5) +
  labs(x = "Risk Difference", y = "Covariate", 
       title = "sl3 Variable Importance for COVID Day 25 Cases")

