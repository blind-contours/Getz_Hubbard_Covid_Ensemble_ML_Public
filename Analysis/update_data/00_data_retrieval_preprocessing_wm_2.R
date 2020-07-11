# Adding polygon info for counties to get the centroid
library(tigris) # using the counties() command
library(sf)

library(caret)
library(rvest)
library(dplyr)
library(tidyverse)
library(here)

# Download USFacts data
usf <- data.frame(
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"),
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
)

usf$fips <- as.integer(usf[, 1])
# usf[ , grepl( "countyFIPS" , names( usf ) ) ]


# Parse FIPS as integers
usf$fips <- as.integer(usf$countyFIPS)
# Remove counties in Alaska and Hawaii
usf <- usf[!((usf$State %in% c("AK", "HI")) | (usf$fips == 0)), ]

# Read airports data
airports <- read.csv(here("Analysis/update_data/data/processed/counties_airports.csv"))


# Read counties polygons
# When I tried to pushed the changes to git
# there was an error because tl_2019_us_county.shp
# was too large
# polygons=sf::st_read("data/shape/tl_2019_us_county.shp")
polygons <- counties(cb = F, year = 2019, class = "sf")
# Parse FIPS as integers
polygons$fips <- as.integer(as.character(polygons$GEOID))
# Keep only counties with data from US Facts
polygons <- polygons[polygons$fips %in% usf$fips, ]
# Order polygons by FIPS
polygons <- polygons[order(polygons$fips), ]
# Calculate counties centroids
centroids <- sf::st_coordinates(sf::st_centroid(polygons))

# Initialize counties
counties <- data.frame(
  "FIPS" = polygons$fips,
  "Name" = polygons$NAME,
  "FirstCaseDay" = NA,
  "CountyRelativeDay25Cases" = NA,
  "TotalCasesUpToDate" = 0,
  "USRelativeDay100Deaths" = 0,
  "TotalDeathsUpToDate" = 0,
  "CentroidLat" = centroids[, 2],
  "CentroidLon" = centroids[, 1],
  "NearestAirportName" = NA,
  "NearestAirportDistance" = NA,
  "NearestAirportEnplanements" = NA,
  "NearestAirportOver5000000Name" = NA,
  "NearestAirportOver5000000Distance" = NA,
  "NearestAirportOver5000000Enplanements" = NA,
  "Population" = NA,
  "PublicTransportation" = NA,
  "GDP" = NA,
  "AreaLand" = polygons$ALAND,
  "AreaWater" = polygons$AWATER
)

# Convert cases and deaths data into matrix
ndays <- ncol(usf) / 2 - 5
usf <- usf[match(counties$FIPS, usf$fips), ]
mcases <- data.matrix(usf[, 6:(5 + ndays)])
mdeaths <- data.matrix(usf[, (ndays + 10):(2 * ndays + 9)])

# Calculate counties cases and deaths statistics
for (i in 1:nrow(counties)) {
  if (any(mcases[i, ] > 0)) {
    counties$TotalCasesUpToDate[i] <- mcases[i, ndays]
    counties$TotalDeathsUpToDate[i] <- mdeaths[i, ndays]
    fc <- min(which(mcases[i, ] > 0))
    counties$FirstCaseDay[i] <- fc
    counties$USRelativeDay100Deaths[i] <- mdeaths[i, 100]
    if (ndays - fc >= 24) {
      counties$CountyRelativeDay25Cases[i] <- mcases[i, fc + 24]
    }
  }
}

# Store a vector of indices of airports with enplanements of at least 5,000,000
f <- which(airports$CY.18.Enplanements >= 5000000, )
for (i in 1:nrow(counties)) {
  # Calculate county nearest airport
  dists <- gmt::geodist(counties$CentroidLat[i], counties$CentroidLon[i], airports$Latitude, airports$Longitude, units = "km")
  m <- which.min(dists)
  counties$NearestAirportName[i] <- as.character(airports$Name[m])
  counties$NearestAirportDistance[i] <- dists[m]
  counties$NearestAirportEnplanements[i] <- airports$CY.18.Enplanements[m]
  # Calculate county nearest airport with enplanements>5000000
  m <- which.min(dists[f])
  counties$NearestAirportOver5000000Name[i] <- as.character(airports$Name[f[m]])
  counties$NearestAirportOver5000000Distance[i] <- dists[f[m]]
  counties$NearestAirportOver5000000Enplanements[i] <- airports$CY.18.Enplanements[f[m]]
}

# Add population data
a <- read.csv(here("Analysis/update_data/data/processed/nir_covid_county_population_usafacts.csv"))
counties$Population <- a$population[match(counties$FIPS, as.integer(a$countyFIPS))]

# Add public transportation data
# a=read.csv(here('Analysis/update_data/data/raw/ACSST5Y2018.S0802_data_with_overlays_2020-04-11T224619.csv'))
# a=a[2:nrow(a),]
# counties$PublicTransportation=as.numeric(a$S0802_C04_001E[match(counties$FIPS,as.integer(substr(a$GEO_ID,10,15)))])

# Add county GDP data
# I (whitney) changed the countyGDP to lagdp1219
# we may need to change this back to what it was before
# switched back to CountyGDP 5/23/20
a <- read.csv(here("Analysis/update_data/data/raw/CountyGDP.csv"))
counties$GDP <- a$X2018[match(counties$FIPS, as.integer(a$GeoFips))]

# Add single value variables from census
for (name in c("air_quality", "all_heartdisease_deathrate", "all_stroke_deathrate", "num_hospitals", "percent_park_access", "urban_rural_status")) {
  a <- read.csv(here(paste("Analysis/update_data/data/raw/", name, ".csv", sep = "")))
  a$Value[a$Value == -1] <- NA
  counties[name] <- a$Value[match(counties$FIPS, a$cnty_fips)]
}

# Add analytic_data2020
a <- read.csv(here("Analysis/update_data/data/raw/analytic_data2020.csv"))
a <- a[2:nrow(a), ]
counties <- cbind(counties, a[match(counties$FIPS, as.integer(as.character(a$X5.digit.FIPS.Code))), c(8, 34, 39, 70, 75, 80, 85, 90, 95, 105, 130, 135, 140, 153, 173, 183, 188, 193, 218, 258, 263, 276, 282, 302, 307, 402, 407, 412, 417, 462, 503, 681, 686, 691, 701, 706)])

# Add County_Table_Chronic_Conditions_Prevalence_by_Age_2017.xlsx
age_group <- c("prev_2017_all_ages_", "prev_2017_under_65_", "prev_2017_over_65_")
for (i in 1:3) {
  a <- readxl::read_excel(here("Analysis/update_data/data/chronic_conditions_prev_by_age_2017.xlsx"), sheet = i)
  # a=a[1:nrow(a),]
  b <- a[, 4:24]
  colnames(b) <- paste0(age_group[i], colnames(b))
  c <- data.frame(a[, 1:3], b)
  counties <- cbind(counties, c[match(counties$FIPS, as.integer(c$State.County.FIPS.Code)), 4:ncol(c)])
}

# Add County_Table_Chronic_Conditions_Spending_2017.xlsx
# spending <- c("actual spending for", "standardized spending for")
# for (i in 1:2) {
#   a=readxl::read_excel(here("Analysis/update_data/data/chronic_conditions_actual_per_capita_spending_2017.xlsx"), sheet = i)
#   a=a[2:nrow(a),]
#   colnames(a) <- paste(spending[i], colnames(a))
#   counties=cbind(counties,a[match(counties$FIPS, as.integer(a$`State/County FIPS Code`)),4:ncol(a)])
# }

# Add DiabetesAtlasCountyData.csv
# a=read.csv(here("Analysis/update_data/data/raw/DiabetesAtlasCountyData.csv"),skip = 2)
# counties$diabetesAtlas=a[match(counties$FIPS,a$CountyFIPS),4]

# Add Education.xls
# a=readxl::read_excel(here("Analysis/update_data/data/raw/Education.xls"),skip=4)
# counties=cbind(counties,a[match(counties$FIPS,as.integer(a$`FIPS Code`)),4:ncol(a)])

# Add IHME_USA_COUNTY_RESP_DISEASE_MORTALITY_1980_2014_NATIONAL_Y2017M09D26.XLSX
# a=readxl::read_excel(here("Analysis/update_data/data/raw/IHME_USA_COUNTY_RESP_DISEASE_MORTALITY_1980_2014_NATIONAL_Y2017M09D26.XLSX" ),skip = 1)
# data=a[match(counties$FIPS,a$FIPS),3:ncol(a)]
# for (i in 1:nrow(data)){
#   for (cn in colnames(data)){
#     counties[i,cn]=as.numeric(strsplit(as.character(data[i,cn]),' ')[[1]][1])
#   }
# }

# Add SVI2018_US_COUNTY.csv - this has the CDC vulnerability scores:
a <- read.csv(here("Analysis/update_data/data/raw/SVI2018_US_COUNTY.csv"))
a <- select(a, c(
  "FIPS",
  "EPL_AGE65",
  "EPL_AGE17",
  "EPL_DISABL",
  "EPL_SNGPNT",
  "EPL_MINRTY",
  "EPL_LIMENG",
  "EPL_MUNIT",
  "EPL_MOBILE",
  "EPL_CROWD",
  "EPL_NOVEH",
  "EPL_GROUPQ",
  "EPL_PCI"
))
counties <- cbind(counties, a[match(counties$FIPS, a$FIPS), ])

# Add Unemployment.xls
# a=readxl::read_excel(here("Analysis/update_data/data/raw/Unemployment.xls"), sheet = 1,skip = 7)
# counties=cbind(counties,a[match(counties$FIPS,as.integer(a$FIPStxt)),4:ncol(a)])

# Changed from tester2.csv to 2018ACS.csv
# 06/10/2020 changed 2018ACS to acs_2018_Jun.csv
a <- read.csv(here("Analysis/update_data/data/raw/acs_2018_Jun.csv"))
# Changed a$GEOID to a$GEIOD because I missspelled it
counties <- cbind(counties, a[match(counties$FIPS, a$GEIOD), 3:ncol(a)])

# Prepare states to match census state fips to NOAA state fips
states <- as.character(unique(usf$State))
states_fips <- purrr::map(states, function(state) usf$stateFIPS[which(usf$State == state)[1]])

# Download average, min and max temperature and precipitation from NOAA
# taking "tmin","tmax" out of the for loop
# dir.create("Analysis/update_data/data/NOAA")
# NOAA_DIR = dir.create("Analysis/update_data/data/NOAA")
# NOAA_DIR
# if (!dir.exists(NOAA_DIR)) {dir.create(NOAA_DIR)}
for (p in c("tavg", "pcp")) {
  # Run over months
  for (m in 1:4) {
    cn <- sprintf("%s_m%d", p, m)
    print(cn)
    counties[cn] <- NA
    # Run over states
    for (n in 1:49) {
      file_name <- sprintf("%s/%s_M%d_ST%d.csv", "Analysis/update_data/data/NOAA", p, m, n)
      if (!file.exists(file_name)) {
        url <- sprintf("https://www.ncdc.noaa.gov/cag/county/mapping/%d-%s-20200%d-1.csv", n, p, m)
        download.file(url, file_name)
      }
      noaa <- read.csv(file_name, skip = 3)
      sfips <- states_fips[states == substr(as.character(noaa$Location.ID[1]), 1, 2)][[1]]
      fips <- as.integer(substr(as.character(noaa$Location.ID), 4, 6)) + sfips * 1000
      f <- floor(counties$FIPS / 1000) == sfips
      counties[cn][f, 1] <- noaa$Value[match(counties$FIPS[f], fips)]
    }
  }
}



# Read commuting data
commuting <- readxl::read_excel("Analysis/update_data/data/raw/USCommuting2015.xlsx", skip = 6)
commuting <- commuting[1:139433, ]
fips_residence <- as.integer(commuting$`State FIPS Code...1`) * 1000 + as.integer(commuting$`County FIPS Code...2`)
fips_work <- as.integer(commuting$`State FIPS Code...5`) * 1000 + as.integer(commuting$`County FIPS Code...6`)
by_residence <- aggregate(commuting$`Workers in Commuting Flow`, list(fips = fips_residence), sum)
by_work <- aggregate(commuting$`Workers in Commuting Flow`, list(fips = fips_work), sum)
counties$agg_commuting_by_residence_place <- by_residence$x[match(counties$FIPS, by_residence$fips)]
counties$agg_commuting_by_work_place <- by_work$x[match(counties$FIPS, by_work$fips)]


## read in employment data
lbs_employment_types <- read_excel("Analysis/update_data/data/raw/lbs_employment_types.xlsx",
  sheet = "US_St_Cn_MSA"
)

lbs_employment_x_county <- lbs_employment_types %>%
  filter(`Area Type` == "County") %>%
  select(c("Area\r\nCode", "Ownership", "Industry", "Establishment Count")) %>%
  mutate(fips = `Area\r\nCode`) %>%
  # mutate(fips = str_sub(fips, 2))  %>%
  mutate(occu_counts = `Establishment Count`)

lbs_employment_x_county$Industry <- paste(
  lbs_employment_x_county$Ownership,
  lbs_employment_x_county$Industry
)
lbs_employment_x_county$Industry <- gsub("[[:digit:]]+", "", lbs_employment_x_county$Industry)

lbs_employment_x_county <- lbs_employment_x_county %>%
  select(-c("Ownership", "Area\r\nCode", "Establishment Count"))

lbs_employment_x_county_wide <- spread(
  lbs_employment_x_county,
  Industry,
  occu_counts
)

lbs_employment_x_county_wide$fips <- as.integer(lbs_employment_x_county_wide$fips)

lbs_employment_x_county_wide_rename <-
  lbs_employment_x_county_wide %>% 
  select(fips,
         occ_total_all_industries = `Total Covered  Total, all industries`,
         occ_all_federal = `Federal Government  Total, all industries`,
         occ_all_state = `State Government  Total, all industries`,
         occ_all_local = `Local Government  Total, all industries`,
         occ_all_private = `Private  Total, all industries`,
         occ_goods_prod = `Private  Goods-producing`,
         occ_natural_mining = `Private  Natural resources and mining`,
         occ_construction = `Private  Construction`,
         occ_Manufacturing = `Private  Manufacturing`,
         occ_servic_prov = `Private  Service-providing`,
         occ_trade_trans_util = `Private  Trade, transportation, and utilities`,
         occ_Info = `Private  Information`,
         occ_financial = `Private  Financial activities`,
         occ_prof_business = `Private  Professional and business services`,
         occ_educ_health = `Private  Education and health services`,
         occ_leisure = `Private  Leisure and hospitality`,
         occ_other_services = `Private  Unclassified`
  )

counties_occ <- merge(counties, 
                      lbs_employment_x_county_wide_rename, by.x = "FIPS", by.y = "fips")

counties_occ$occ_total_all_industries  <- counties_occ$occ_total_all_industries / counties_occ$Population
counties_occ$occ_all_federal <- counties_occ$occ_all_federal / counties_occ$Population
counties_occ$occ_all_state <-  counties_occ$occ_all_state / counties_occ$Population
counties_occ$occ_all_local <- counties_occ$occ_all_local / counties_occ$Population 
counties_occ$occ_all_private <- counties_occ$occ_all_private / counties_occ$Population
counties_occ$occ_goods_prod <- counties_occ$occ_goods_prod / counties_occ$Population
counties_occ$occ_natural_mining <- counties_occ$occ_natural_mining / counties_occ$Population
counties_occ$occ_construction <- counties_occ$occ_construction / counties_occ$Population
counties_occ$occ_Manufacturing <- counties_occ$occ_Manufacturing / counties_occ$Population
counties_occ$occ_servic_prov <- counties_occ$occ_servic_prov / counties_occ$Population
counties_occ$occ_trade_trans_util <- counties_occ$occ_trade_trans_util / counties_occ$Population
counties_occ$occ_Info  <- counties_occ$occ_Info / counties_occ$Population
counties_occ$occ_financial <- counties_occ$occ_financial / counties_occ$Population
counties_occ$occ_prof_business <- counties_occ$occ_prof_business / counties_occ$Population
counties_occ$occ_educ_health <- counties_occ$occ_educ_health / counties_occ$Population
counties_occ$occ_leisure <- counties_occ$occ_leisure / counties_occ$Population
counties_occ$occ_other_services <- counties_occ$occ_other_services / counties_occ$Population


## getting additional data from county rankings that were not aggregated (i.e. obesity and segregration)

County_Health_Rankings_Data <- read_excel("Analysis/update_data/data/raw/2020 County Health Rankings Data.xlsx",
  sheet = "Ranked Measure Data", skip = 1
)

County_Health_Rankings_Data_add_msrs <- County_Health_Rankings_Data %>% select(
  FIPS,
  soc_assc_rate = `Social Association Rate`
)

County_Health_Rankings_Data_add_data <- read_excel("Analysis/update_data/data/raw/2020 County Health Rankings Data.xlsx",
  sheet = "Additional Measure Data", skip = 1
)

County_Health_Rankings_Data_add_data_msrs <- County_Health_Rankings_Data_add_data %>% 
  select(
  `FIPS`,
  seg_index = `Segregation index`,
  pct_mental_distress = `% Frequent Mental Distress`,
  pct_insufficient_sleep = `% Insufficient Sleep`
)

County_Health_Rankings_Data_targets <- merge(County_Health_Rankings_Data_add_msrs,
  County_Health_Rankings_Data_add_data_msrs,
  by = "FIPS"
)

counties_add_data <- merge(counties_occ, County_Health_Rankings_Data_targets, by = "FIPS")

## political party data

countypres_2000_2016 <- read_csv("Analysis/update_data/data/raw/countypres_2000-2016.csv")

countypres2016 <- countypres_2000_2016 %>% filter(year == 2016)

countypres2016_rep <- countypres2016 %>%
  group_by(FIPS) %>%
  mutate(rep_ratio = candidatevotes / sum(candidatevotes)) %>%
  filter(party == "republican")

countypres2016_rep <- countypres2016_rep %>% select(FIPS, rep_ratio)

counties_add_data_political <- merge(counties_add_data, countypres2016_rep, by = "FIPS")




# Results #
# We may need to take some columns out first
# View(CountiesMergedData20200517)

## removing variables based on NA percent
# counties2 <- counties[, which(colMeans(!is.na(counties)) > 0.75)]
#
# ## removing near zero variance variables
# nz_idx_vector <- nearZeroVar(
#   counties2,
#   freqCut = 95/5,
#   uniqueCut = 10,
#   saveMetrics = FALSE,
#   names = FALSE,
#   foreach = FALSE,
#   allowParallel = TRUE
# )
#
# ## what variables are near nonvarying
# counties3 <- counties2[,-nz_idx_vector]

# Write results to a file
# write.csv(counties3,here("Analysis/update_data/data/processed/CountiesMergedData20200517.csv"))
write.csv(
  counties_add_data_political,
  here("Analysis/update_data/data/processed/CountiesMergedData_July_10.csv")
)

