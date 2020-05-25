library(tidyverse)
library(airportr)
library(readxl)

airpo <- read_csv("data/airports/airports1.csv") %>%
  filter(Country == "United States") %>%
  select( -c("Latitude", "Longitude") )


location1 <- tibble()
city1 <- vector()
name1 <- vector()
IATA1 <- vector()

# Needs airportr library
for (i in 1:nrow(airpo)) {
  location <- airport_location(airpo$ICAO[i],input_type ="ICAO")
  city <- airport_lookup(airpo$ICAO[i], input_type = "ICAO", output_type = "city")
  name <- airport_lookup(airpo$ICAO[i], input_type = "ICAO", output_type = "name")
  IATA <- airport_lookup(airpo$ICAO[i], input_type = "ICAO", output_type = "IATA")
  
  location1 <- rbind(location1, location) 
  city1 <- c(city1, city) 
  name1 <- c(name1, name) 
  IATA1 <- c(IATA1, IATA)
    
  print(i)
}

airport_loc <- cbind(airpo, location1, city1, name1, IATA1) 



###############################################################
airp <- read_xlsx("data/airports/cy18-commercial-service-enplanements.xlsx") %>%
  filter(City != "NA") 
names(airp)[names(airp)=="Locid"] <- "IATA"

airp2 <- left_join(airp, airport_loc, by = "IATA") %>%
  filter( name1 != "NA") %>%
  select(
    c(
      "Rank", "RO", "ST", "IATA1", "ICAO", "Name", 
      "City.x", "City.y", "city1", "S/L", "Hub",
      "Latitude","Longitude",
      "CY 18 Enplanements", "CY 17 Enplanements", "% Change",
      "Altitude", "Timezone", "DST", "Tz database", "Type",              
      "Source"
    )
  )

names(airp2)[names(airp2)=="ST"] <- "state_id"
names(airp2)[names(airp2)=="city1"] <- "city"

airp2$city <- gsub(" CA", "", airp2$city)
airp2$city <- gsub(" VA", "", airp2$city)
airp2$city <- gsub(" IA", "", airp2$city)
airp2$city <- gsub(" NC", "", airp2$city)
airp2$city <- gsub(" KY", "", airp2$city)
airp2$city <- gsub(" NH", "", airp2$city)
airp2$city <- gsub(" CO", "", airp2$city)
airp2$city <- gsub(" WY", "", airp2$city)
airp2$city <- gsub(" VA", "", airp2$city)
airp2$city <- gsub(" AL", "", airp2$city)

airp2$city <- gsub("MONTGOMERY", "Montgomery", airp2$city)
airp2$city <- gsub("Dallas-Fort Worth", "Fort Worth", airp2$city)
airp2$city <- gsub("Hattiesburg/Laurel", "Laurel", airp2$city)
airp2$city <- gsub("PADUCAH", "Puducah", airp2$city)
airp2$city <- gsub("PARKERSBURG", "Parkersburg", airp2$city)
airp2$city <- gsub("Raleigh-durham", "Raleigh", airp2$city)
airp2$city <- gsub("Redmond-Bend", "Redmond", airp2$city)
airp2$city <- gsub("ANDERSON", "Anderson", airp2$city)
airp2$city <- gsub("Boulder", "Boulder City", airp2$city)

  
###############################################################
cities <- read_excel("data/airports/uscities.xlsx")
class(cities$timezone)

test <- test <- left_join(airp2, cities, by = c("city", "state_id")) %>%
  select(
    c(
      "Rank", "RO" , "state_id", "state_name", 
      "county_name", "county_fips",
      "IATA1", "ICAO", "Name", "City.x", "City.y", "city", 
      "S/L", "Hub",  "Latitude", "Longitude", 
      "CY 18 Enplanements", "CY 17 Enplanements", "% Change",
      "Altitude", "Timezone", "timezone", "DST", "Tz database",
      "Type", "Source"
    )
  ) %>%
  filter(county_name != "NA")

# test$county_fips <- gsub("2105", "02232", test$county_fips)
# test$county_fips <- gsub("2230", "02232", test$county_fips)
# 
# test$county_fips <- gsub("2158", "02270", test$county_fips)
# 
# test$county_fips <- gsub("2195", "51730", test$county_fips)
# 
# test$county_fips <- gsub("22329", "18027", test$county_fips)
# 
# test$county_fips <- gsub("2275", "02280", test$county_fips)

# test$county_fips <- as.numeric(test$county_fips)


###############################################################
# counties <- read_excel("C:/Users/Whitney/Downloads/FIPS_county_state.xlsx")
# names(counties)[names(counties)=="FIPS"] <- "county_fips"
# names(counties)[names(counties)=="Name"] <- "county_name"
# names(counties)[names(counties)=="State"] <- "state_id"
# 
# 
# test2 <- left_join(test, counties, by= c("county_fips"))
# 
# 
# getwd()

# to data to file
write.csv(test,"data/processed/counties_airports.csv", row.names = FALSE)
