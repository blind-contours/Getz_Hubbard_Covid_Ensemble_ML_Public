library(caret)
library(rvest)
library(dplyr)
library(tidyverse)
library(here)


## NA threshold
na_thresh <- 0.75

## run census variable rename? 
census_data_rename <- FALSE

## load data
covid_data_unprocessed <- read_csv("Analysis/update_data/data/processed/CountiesMergedData20200517.csv")

## remove columns with NA greater than threshold
covid_data_processed <- covid_data_unprocessed[, which(colMeans(!is.na(covid_data_unprocessed)) > na_thresh)]


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

## looks like just three variables are characters, nearest airport name, nearest airport over X and county name - will likely leave out later two variables as probably not useful

## pull outcome data

CountyRelativeDay25Cases <- covid_data_processed %>% pull(-CountyRelativeDay25Cases) 
TotalCasesUpToDate <- covid_data_processed %>% pull(-TotalCasesUpToDate)
USRelativeDay100Deaths <- covid_data_processed %>% pull(-USRelativeDay100Deaths)
TotalDeathsUpToDate <- covid_data_processed %>% pull(-TotalDeathsUpToDate)
FirstCaseDay <- covid_data_processed %>% pull(-FirstCaseDay)


## check for na in outcome data
outcome_data <- cbind(CountyRelativeDay25Cases, TotalCasesUpToDate, USRelativeDay100Deaths, TotalDeathsUpToDate, FirstCaseDay)
list_na <- colnames(outcome_data)[ apply(outcome_data, 2, anyNA) ]

number_na <- colSums(is.na(outcome_data))
number_na

## only a few, we will remove these at the last step

outcomes <- c("CountyRelativeDay25Cases", "TotalCasesUpToDate", "USRelativeDay100Deaths" , "TotalDeathsUpToDate", "FirstCaseDay")

## remove outcomes
covid_data_processed_features <- covid_data_processed %>% 
  select(-outcomes)

## remove character variables
covid_data_processed_features_numeric <- covid_data_processed_features %>% 
  select(-char_vars)

## impute the mean for NA values in the numeric dataset (we already filtered for NA columns greater than 75%)
covid_data_processed_features_numeric_imputed<- na_mean(covid_data_processed_features_numeric, option = "mean")


## identifying and removing highly correlated features
descrCor <-  cor(covid_data_processed_features_numeric_imputed[,2:length(covid_data_processed_features_numeric_imputed)], use = "pairwise.complete.obs")
highlyCorDescr  <- findCorrelation(descrCor, cutoff = 0.99)
covid_data_processed_features_numeric_imputed_high_corr_rm <- covid_data_processed_features_numeric_imputed[,-highlyCorDescr]

## just add back in the name factor variable because I don't think that nearest airport type etc. is useful (could be wrong)
county_names <- covid_data_processed$Name
final_covid_processed <- cbind(county_names, covid_data_processed_features_numeric_imputed_high_corr_rm)

final_covid_processed <- final_covid_processed %>%
  select(-c("X1"))

## add the outcome back in and let's just remove the rows with NAs for outcome

## quick function to remove rows with any NA from selected columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

final_covid_processed <- cbind(outcome_data, final_covid_processed)
final_covid_processed <- completeFun(final_covid_processed, outcomes)

## Column bind the outcome data and write the final dataset
write.csv(final_covid_processed, file = here("Analysis/update_data/data/processed/cleaned_covid_data_final.csv"))






  