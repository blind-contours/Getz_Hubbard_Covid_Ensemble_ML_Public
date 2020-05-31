library(caret)
library(rvest)
library(dplyr)
library(tidyverse)

covid_data_unprocessed <- read_csv("Analysis/update_data/data/processed/CountiesMergedData20200517.csv")

## removing variables based on known nonimportant names
covid_data_processed_1 <- covid_data_unprocessed[, -grep("..CI.low|..CI.high", colnames(covid_data_unprocessed))]
covid_data_processed_2 <- covid_data_processed_1[, -grep("numerator|denominator", colnames(covid_data_processed_1))]

## removing variables based on NA percent

covid_data_processed_3 <- covid_data_processed_2[, which(colMeans(!is.na(covid_data_processed_2)) > 0.75)]



## removing near zero variance variables
nz_idx_vector <- nearZeroVar(
  covid_data_processed_3,
  freqCut = 95/5,
  uniqueCut = 10,
  saveMetrics = FALSE,
  names = FALSE,
  foreach = FALSE,
  allowParallel = TRUE
)

## what variables are near nonvarying
covid_data_processed_4 <- covid_data_processed_3[,-nz_idx_vector]

## many of the variable names 915: 942 (?) are not relabeled, go to the census API and get the variable names table from the URL to rename

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

var_name_matches <- variable_name_tbs[variable_name_tbs$Name %in%  names(covid_data_processed_4), c("Name","Label")] 

covid_data_processed_5 <- covid_data_processed_4 %>% rename_at(var_name_matches$Name, ~ var_name_matches$Label)

## still some variables that do not have clear names but I need help from who aggregated to figure out what/where the keys are

names(covid_data_processed_5) <- gsub('!!', '_', names(covid_data_processed_5))
names(covid_data_processed_5) <- gsub('x..', '', names(covid_data_processed_5))


## check structures of data 
covid_data_processed_5[, sapply(covid_data_processed_5, class) == 'character']


## identifying and removing highly correlated features
descrCor <-  cor(covid_data_processed_5[4:dim(covid_data_processed_5)[2]])



  