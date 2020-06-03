library(tidyverse)
library(imputeTS)
library(pheatmap)
library(ggfortify)
library(gtools)
library(readxl)
library(here)

## load data after preprocessing
covid_data_processed <- read_excel(here("Analysis/update_data/data/processed/cleaned_covid_data_final.xlsx"),sheet = 1)

variable_labels <- variable_labels[9: length(variable_labels)]

## create quantiles of the outcome data to use as labels in the clustering analyses
outcomes <- c("CountyRelativeDay25Cases", "TotalCasesUpToDate", "USRelativeDay100Deaths" , "TotalDeathsUpToDate", "FirstCaseDay")

features_data <- covid_data_processed %>% 
  select(-c(outcomes, 'Row Label', 'FIPS', 'county_names'))

outcomes_data <- covid_data_processed %>% 
  select(outcomes)

## map quantcut from gtools across each column of outcomes_data
outcome_quantiles <- map(outcomes_data, quantcut, q=4)

## relabel the quantiles
for (i in seq_along(1:length(outcome_quantiles))) {
  levels(outcome_quantiles[[i]]) <-  c("Q1", "Q2", "Q3", "Q4")
}


## run PCA on feature data
covid_pca <- prcomp(covid_data_processed[c(9:dim(covid_data_processed)[2])], center = TRUE, scale = TRUE)
summary(covid_pca)

##plot the variance plots to see variance explained over PCs
screeplot(covid_pca, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(covid_pca$sdev^2 / sum(covid_pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

## using autoplot to make nicer plots 

autoplot(covid_pca)

## nothing very nice here but maybe there is some trend if we create factors for our outcome data and overlay some plot coloring

                    
## Heatmap


## make dataframe of each outcome, row names FIPS to do multiple annotations in the heatmap

## day 25 cases
cases_25_df = data.frame("Quantile_Day_25_Cases" = outcome_quantiles$CountyRelativeDay25Cases)
## total cases 
cases_total_df = data.frame("Quantile_Total_Cases" = outcome_quantiles$TotalCasesUpToDate)
## total cases 
deaths_100_df = data.frame("Quantile_Day_100_Deaths" = outcome_quantiles$USRelativeDay100Deaths)
## TotalDeathsUpToDate
deaths_total_df = data.frame("Quantile_Total_Deaths" = outcome_quantiles$TotalDeathsUpToDate)
## TotalDeathsUpToDate
first_day_df = data.frame("Quantile_Day_First_Case" = outcome_quantiles$FirstCaseDay)

oucome_annotation <- cbind(cases_25_df, 
                           cases_total_df,
                           deaths_100_df,
                           deaths_total_df,
                           first_day_df)


covid_numerical <- as.matrix(covid_data_processed[,9:dim(covid_data_processed)[2]])
rownames(covid_numerical) <- covid_data_processed$FIPS

#scale the data
covid_num_scale <- scale(covid_numerical)


my.breaks <- c(seq(-5, 0, by=0.1), seq(0.1, 5, by=0.1)) 
my.colors <- c(colorRampPalette(colors = c("blue", "white"))(length(my.breaks)/2), colorRampPalette(colors = c("white", "orange", "red", "purple"))(length(my.breaks)/2))

## set rownames for row dendrogram and column dendrogram
rownames(oucome_annotation) <-  rownames(covid_num_scale)
variable_labels <- as.data.frame(t(variable_labels))
rownames(variable_labels) <-  colnames(covid_num_scale)


covid_factors_heatmap <- pheatmap(covid_num_scale,main = "COVID-19 Heatmap",
         annotation_names_row = FALSE,
         annotation_names_col = FALSE,
         show_colnames = FALSE, 
         show_rownames = FALSE,
         scale = "row",
         color = my.colors,
         breaks = my.breaks,
         annotation_row = oucome_annotation,
         annotation_col = variable_labels,
         cutree_rows = 4,
         cutree_cols = 5)

clusters <- cutree(covid_factors_heatmap$tree_col, k = 5)
clusters <- as.data.frame(clusters)

colnames_dendro_reordered <- colnames(features_data)[covid_factors_heatmap$tree_col$order]

table(clusters)

hot_spot_1 <- colnames_dendro_reordered[1:24] ## 2
hot_spot_2 <- colnames_dendro_reordered[25:51] ## 4
hot_spot_3 <- colnames_dendro_reordered[53:83] ## 3
hot_spot_4 <- colnames_dendro_reordered[84:93] ## 1 
hot_spot_5 <- colnames_dendro_reordered[93:110] ## 5


clusters[rownames(clusters) %in% hot_spot_1,] ## so it appears that the cluster on the far right is cluster 2 from cut-tree
clusters[rownames(clusters) %in% hot_spot_2,] ## so it appears that the cluster on the second left is cluster 4 from cut-tree
clusters[rownames(clusters) %in% hot_spot_3,] ## so it appears that the cluster on the second left is cluster 4 from cut-tree
clusters[rownames(clusters) %in% hot_spot_4,] ## so it appears that the cluster on the second left is cluster 4 from cut-tree
clusters[rownames(clusters) %in% hot_spot_5,] ## so it appears that the cluster on the second left is cluster 4 from cut-tree






