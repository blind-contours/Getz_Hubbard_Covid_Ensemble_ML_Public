library(caret)
library(rvest)
library(dplyr)
library(tidyverse)
library(here)
library(imputeTS)
`%notin%` <- Negate(`%in%`)

## thresholds
na_thresh <- 0.70
corr_thres <- 0.90
## run census variable rename? 
census_data_rename <- FALSE


# Changed to CountiesMergedData_July_10.csv
covid_data_unprocessed <- read_csv("Analysis/update_data/data/processed/CountiesMergedData_July_15.csv")

## remove character values that aren't needed
covid_data_unprocessed <- covid_data_unprocessed %>% 
  select(-c(X1, State, Name, NearestAirportName, NearestAirportOver5000000Name))

## convert to numeric
covid_data_unprocessed <- data.frame(lapply(covid_data_unprocessed, 
                                            function(x) as.numeric(as.character(x))))

## these variables still need standardization by population: 
covid_data_unprocessed$Premature.death.raw.value <- covid_data_unprocessed$Premature.death.raw.value / covid_data_unprocessed$Population 
covid_data_unprocessed$HIV.prevalence.raw.value <- covid_data_unprocessed$HIV.prevalence.raw.value / covid_data_unprocessed$Population 
covid_data_unprocessed$Sexually.transmitted.infections.raw.value <- covid_data_unprocessed$Sexually.transmitted.infections.raw.value / covid_data_unprocessed$Population 
covid_data_unprocessed$Preventable.hospital.stays.raw.value <- covid_data_unprocessed$Preventable.hospital.stays.raw.value / covid_data_unprocessed$Population 



# get data dictionary 
Data_Dictionary <- read_excel("Analysis/update_data/data/processed/Data_Dictionary.xlsx")

vars_2_keep <- Data_Dictionary %>% 
  filter(Keep == "Yes") %>% select(`Variable Name`)

covid_data_unprocessed <-covid_data_unprocessed %>% 
  select(vars_2_keep$`Variable Name`)

## remove columns with NA greater than threshold
covid_data_processed <- covid_data_unprocessed[, which(colMeans(!is.na(covid_data_unprocessed)) > na_thresh)]

vars_rmv_na <- colnames(covid_data_unprocessed)[names(covid_data_unprocessed) %notin% names(covid_data_processed)]
write.csv(vars_rmv_na, here("Analysis/update_data/data/processed/vars_removed_na_thresh.csv"))

## removing near zero variance variables
nz_idx_vector <- nearZeroVar(
  covid_data_processed,
  freqCut = 95/5,
  uniqueCut = 10,
  saveMetrics = FALSE,
  names = FALSE,
  foreach = FALSE,
  allowParallel = TRUE
)

## what variables are near nonvarying
if (length(nz_idx_vector) > 0) {
  covid_data_processed <- covid_data_processed[,-nz_idx_vector]
}


## previous version of the dataset had variable names from the census that were not readable, this small script gathers the variable names from census and renames variables
if (census_data_rename == TRUE) {
  webpage <- read_html("https://api.census.gov/data/2017/acs/acs5/variables.html")
  tbls <- html_nodes(webpage, "table")
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  
  variable_name_tbs <- as.data.frame(tbls_ls[[1]])
  variable_name_tbs <- variable_name_tbs[,-dim(variable_name_tbs)[2]]
  ## rename variables in covid preprocessed data based on census api label column which mactches name
  variable_name_tbs <- as.data.frame(variable_name_tbs)
  
  var_name_matches <- variable_name_tbs[variable_name_tbs$Name %in%  names(covid_data_processed), c("Name","Label")] 
  
  
  var_name_matches <- var_name_matches[!duplicated(var_name_matches$Label),]
  
  covid_data_processed <- covid_data_processed %>% rename_at(var_name_matches$Name, ~ var_name_matches$Label)
  
}


## check structures of data 

str(covid_data_processed)
char_vars <- names(covid_data_processed[, sapply(covid_data_processed, class) == 'character'])


outcomes <- c("CountyRelativeDay25Cases", 
              "TotalCasesUpToDate", 
              "USRelativeDay100Deaths", 
              "TotalDeathsUpToDate", 
              "FirstCaseDay")

outcome_data <- covid_data_processed[,c(outcomes, "FIPS")]

## check for na in outcome data
list_na <- colnames(outcome_data)[ apply(outcome_data, 2, anyNA) ]

number_na <- colSums(is.na(outcome_data))
number_na

## only a few, we will remove these at the last step

## remove outcomes
covid_data_processed_features <- covid_data_processed %>% 
  select(-outcomes)


## impute the mean for NA values in the numeric dataset (we already filtered for NA columns greater than 75%)
covid_data_processed_features_numeric_imputed<- na_mean(covid_data_processed_features, option = "mean")

## identifying and removing highly correlated features
descrCor <-  cor(covid_data_processed_features_numeric_imputed)
descrCor[upper.tri(descrCor)] <- 0
diag(descrCor) <- 0

names(covid_data_processed_features_numeric_imputed[,apply(descrCor,2,function(x) any(x > corr_thres))])
covid_data_processed_features_numeric_imputed_corr_rem <- covid_data_processed_features_numeric_imputed[,!apply(descrCor,2,function(x) any(x > corr_thres))]

covid_data_processed_features_numeric_imputed_corr_rem$occ_total_all_industries <- covid_data_processed_features_numeric_imputed$occ_total_all_industries

final_covid_processed <- covid_data_processed_features_numeric_imputed_corr_rem


## add the outcome back in and let's just remove the rows with NAs for outcome

## quick function to remove rows with any NA from selected columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

final_covid_processed <- merge(outcome_data, final_covid_processed, by = "FIPS")
final_covid_processed <- completeFun(final_covid_processed, outcomes)

## Column bind the outcome data and write the final dataset
write.csv(final_covid_processed, file = here("Analysis/update_data/data/processed/cleaned_covid_data_final.csv"))






  