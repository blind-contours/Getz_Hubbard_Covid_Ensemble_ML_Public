# Adding polygon info for counties to get the centroid
library(tigris) # using the counties() command 
library(sf)
library(here)

# Download USFacts data
usf<-data.frame(
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"),
  read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
)

usf$fips=as.integer(usf[,1])
usf[ , grepl( "countyFIPS" , names( usf ) ) ]


# Parse FIPS as integers
usf$fips=as.integer(usf$countyFIPS)
# Remove counties in Alaska and Hawaii
usf<-usf[!((usf$State %in% c("AK","HI")) | (usf$fips==0)),]

# Read airports data
airports=read.csv(here("Analysis/update_data/data/processed/counties_airports.csv"))


# Read counties polygons
# When I tried to pushed the changes to git 
# there was an error because tl_2019_us_county.shp
# was too large
# polygons=sf::st_read("data/shape/tl_2019_us_county.shp")
polygons = counties(cb = F, year = 2019, class = "sf")
# Parse FIPS as integers
polygons$fips=as.integer(as.character(polygons$GEOID))
# Keep only counties with data from US Facts
polygons=polygons[polygons$fips %in% usf$fips,]
# Order polygons by FIPS
polygons=polygons[order(polygons$fips),]
# Calculate counties centroids
centroids=sf::st_coordinates(sf::st_centroid(polygons))

# Initialize counties
counties=data.frame(
  "FIPS"=polygons$fips,
  "Name"=polygons$NAME,
  "FirstCaseDay"=NA,
  "CountyRelativeDay25Cases"=NA,
  "TotalCasesUpToDate"=0,
  "USRelativeDay100Deaths"=0,
  "TotalDeathsUpToDate"=0,
  "CentroidLat"=centroids[,2],
  "CentroidLon"=centroids[,1],
  "NearestAirportName"=NA,
  "NearestAirportDistance"=NA,
  "NearestAirportEnplanements"=NA,
  "NearestAirportOver5000000Name"=NA,
  "NearestAirportOver5000000Distance"=NA,
  "NearestAirportOver5000000Enplanements"=NA,
  "Population"=NA,
  "PublicTransportation"=NA,
  "GDP"=NA,
  "AreaLand"=polygons$ALAND,
  "AreaWater"=polygons$AWATER
)

# Convert cases and deaths data into matrix
ndays=ncol(usf)/2-5
usf=usf[match(counties$FIPS,usf$fips),]
mcases<-data.matrix(usf[,6:(5+ndays)])
mdeaths<-data.matrix(usf[,(ndays+10):(2*ndays+9)])

# Calculate counties cases and deaths statistics
for (i in 1:nrow(counties)) {
  if (any(mcases[i,]>0)){
    counties$TotalCasesUpToDate[i]=mcases[i,ndays]
    counties$TotalDeathsUpToDate[i]=mdeaths[i,ndays]
    fc=min(which(mcases[i,]>0))
    counties$FirstCaseDay[i]=fc
    counties$USRelativeDay100Deaths[i]=mdeaths[i,100]
    if (ndays-fc>=24) {counties$CountyRelativeDay25Cases[i]=mcases[i,fc+24]}
  }
}

# Store a vector of indices of airports with enplanements of at least 5,000,000
f=which(airports$CY.18.Enplanements>=5000000,)
for (i in 1:nrow(counties)) {
  # Calculate county nearest airport
  dists=gmt::geodist(counties$CentroidLat[i],counties$CentroidLon[i],airports$Latitude,airports$Longitude,units="km")
  m=which.min(dists)
  counties$NearestAirportName[i]=as.character(airports$Name[m])
  counties$NearestAirportDistance[i]=dists[m]
  counties$NearestAirportEnplanements[i]=airports$CY.18.Enplanements[m]
  # Calculate county nearest airport with enplanements>5000000
  m=which.min(dists[f])
  counties$NearestAirportOver5000000Name[i]=as.character(airports$Name[f[m]])
  counties$NearestAirportOver5000000Distance[i]=dists[f[m]]
  counties$NearestAirportOver5000000Enplanements[i]=airports$CY.18.Enplanements[f[m]]
}

# Add population data
a=read.csv(here('Analysis/update_data/data/processed/nir_covid_county_population_usafacts.csv'))
counties$Population=a$population[match(counties$FIPS,as.integer(a$countyFIPS))]

# Add public transportation data
a=read.csv(here('Analysis/update_data/data/raw/ACSST5Y2018.S0802_data_with_overlays_2020-04-11T224619.csv'))
a=a[2:nrow(a),]
counties$PublicTransportation=as.numeric(a$S0802_C04_001E[match(counties$FIPS,as.integer(substr(a$GEO_ID,10,15)))])

# Add county GDP data
# I (whitney) changed the countyGDP to lagdp1219
# we may need to change this back to what it was before
# switched back to CountyGDP 5/23/20
a=read.csv(here("Analysis/update_data/data/raw/CountyGDP.csv"))
counties$GDP=a$X2018[match(counties$FIPS,as.integer(a$GeoFips))]

# Add single value variables from census
for (name in c("air_quality","all_heartdisease_deathrate","all_stroke_deathrate","num_hospitals","percent_park_access","urban_rural_status")) {
  a=read.csv(here(paste("Analysis/update_data/data/raw/",name,".csv",sep="")))
  a$Value[a$Value==-1]=NA
  counties[name]=a$Value[match(counties$FIPS,a$cnty_fips)]
}

# Add analytic_data2020
a=read.csv(here("Analysis/update_data/data/raw/analytic_data2020.csv"))
a=a[2:nrow(a),]
counties=cbind(counties,a[match(counties$FIPS,as.integer(as.character(a$X5.digit.FIPS.Code))),8:ncol(a)])

# Add County_Table_Chronic_Conditions_Prevalence_by_Age_2017.xlsx
for (i in 2:4) {
  a=readxl::read_excel(here("Analysis/update_data/data/raw/County_Table_Chronic_Conditions_Prevalence_by_Age_2017.xlsx"), sheet = i,skip = 4)
  a=a[2:nrow(a),]
  counties=cbind(counties,a[match(counties$FIPS, as.integer(a$...3)),4:ncol(a)])
}

# Add County_Table_Chronic_Conditions_Spending_2017.xlsx
for (i in 2:3) {
  a=readxl::read_excel(here("Analysis/update_data/data/raw/County_Table_Chronic_Conditions_Spending_2017.xlsx") ,sheet = i,skip = 4)
  a=a[2:nrow(a),]
  counties=cbind(counties,a[match(counties$FIPS, as.integer(a$...3)),4:ncol(a)])
}

# Add DiabetesAtlasCountyData.csv
a=read.csv(here("Analysis/update_data/data/raw/DiabetesAtlasCountyData.csv"),skip = 2)
counties$diabetesAtlas=a[match(counties$FIPS,a$CountyFIPS),4]

# Add Education.xls
a=readxl::read_excel(here("Analysis/update_data/data/raw/Education.xls"),skip=4)
counties=cbind(counties,a[match(counties$FIPS,as.integer(a$`FIPS Code`)),4:ncol(a)])

# Add IHME_USA_COUNTY_RESP_DISEASE_MORTALITY_1980_2014_NATIONAL_Y2017M09D26.XLSX
a=readxl::read_excel(here("Analysis/update_data/data/raw/IHME_USA_COUNTY_RESP_DISEASE_MORTALITY_1980_2014_NATIONAL_Y2017M09D26.XLSX" ),skip = 1)
data=a[match(counties$FIPS,a$FIPS),3:ncol(a)]
for (i in 1:nrow(data)){
  for (cn in colnames(data)){
    counties[i,cn]=as.numeric(strsplit(as.character(data[i,cn]),' ')[[1]][1])
  }
}

# Add SVI2018_US_COUNTY.csv
a=read.csv(here("Analysis/update_data/data/raw/SVI2018_US_COUNTY.csv"))
counties=cbind(counties,a[match(counties$FIPS,a$FIPS),7:ncol(a)])

# Add Unemployment.xls
a=readxl::read_excel(here("Analysis/update_data/data/raw/Unemployment.xls"), sheet = 1,skip = 7)
counties=cbind(counties,a[match(counties$FIPS,as.integer(a$FIPStxt)),4:ncol(a)])

# Add tester2.csv 
a=read.csv(here("Analysis/update_data/data/raw/5yearACSdata.csv"))
counties=cbind(counties,a[match(counties$FIPS,a$GEOID),3:ncol(a)])

# Prepare states to match census state fips to NOAA state fips
states=as.character(unique(usf$State))
states_fips=purrr::map(states,function(state) usf$stateFIPS[which(usf$State==state)[1]])

# Download average, min and max temperature and precipitation from NOAA
for (p in c("tavg","tmin","tmax","pcp")) {
  # Run over months
  for (m in 1:4) {
    cn=sprintf("%s_m%d",p,m)
    print(cn)
    counties[cn]=NA
    # Run over states
    for (n in 1:49) {
      url=sprintf("https://www.ncdc.noaa.gov/cag/county/mapping/1-%s-20200%d-1.csv",p,m)
      noaa=read.csv(url,skip=3)
      sfips=states_fips[states==substr(as.character(noaa$Location.ID[1]),1,2)][[1]]
      fips=as.integer(substr(as.character(noaa$Location.ID),4,6))+sfips*1000
      f=floor(counties$FIPS/1000)==sfips
      counties[cn][f,1]=noaa$Value[match(counties$FIPS[f],fips)]
    }
  }
}

# Results #
# We may need to take some columns out first

# View(CountiesMergedData20200517)

# Write results to a file
write.csv(counties,here("Analysis/update_data/data/processed/CountiesMergedData20200517.csv"))

