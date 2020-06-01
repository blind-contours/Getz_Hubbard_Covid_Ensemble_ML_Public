

covid_data_processed <- read_csv("Analysis/update_data/data/processed/cleaned_covid_data_numeric.csv")
char_vars <- names(covid_data_processed[, sapply(covid_data_processed, class) == 'character'])

list_na <- colnames(covid_data_processed)[ apply(covid_data_processed, 2, anyNA) ]
colnames(covid_data_processed)[apply(covid_data_processed, 2, anyNA)]

covid_data_processed_means <- apply(covid_data_processed[,colnames(covid_data_processed) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)


covid_data_processed_mean_impute <- na_mean(covid_data_processed, option = "mean")


covid_pca <- prcomp(covid_data_processed_mean_impute[c(10:dim(covid_data_processed_mean_impute)[2])], center = TRUE, scale = TRUE, rank = 10)

summary(covid_pca)


plot(covid_pca$x[,1],covid_pca$x[,2])
