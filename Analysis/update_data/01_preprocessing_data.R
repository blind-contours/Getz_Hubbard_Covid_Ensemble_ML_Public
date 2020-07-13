library(caret)
library(rvest)
library(dplyr)
library(tidyverse)
library(here)
library(imputeTS)
`%notin%` <- Negate(`%in%`)

## NA threshold
na_thresh <- 0.70

## run census variable rename? 
census_data_rename <- FALSE

## load data CountiesMergedData_Jun_10.csv
# covid_data_unprocessed <- read_csv("Analysis/update_data/data/processed/CountiesMergedData20200517.csv")

# Changed to CountiesMergedData_July_10.csv
covid_data_unprocessed <- read_csv("Analysis/update_data/data/processed/CountiesMergedData_July_13.csv")

county_names <- covid_data_unprocessed$Name

covid_data_unprocessed <- covid_data_unprocessed %>% 
  select(-c(X1, Name, NearestAirportName, NearestAirportOver5000000Name))

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
descrCor <-  cor(covid_data_processed_features_numeric_imputed[,2:length(covid_data_processed_features_numeric_imputed)], use = "pairwise.complete.obs")
highlyCorDescr  <- findCorrelation(descrCor, cutoff = 0.99)

## check high correlation
highlyCorDescr

final_covid_processed <- covid_data_processed_features_numeric_imputed


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






  