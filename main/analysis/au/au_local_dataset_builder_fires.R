# Imports
install.packages(("suncalc"))
install.packages("nngeo")
library(suncalc)
library(sf) 
library(dplyr)
library(nngeo)

####################################
##### AU - Fires Full Dataset  #####
####################################

au_data <- read.csv("MSISS/wildfires/main/data/au_wildfires_2020.csv")

# 1) Load all raw fields

fire_data <- au_data[, c("latitude", "longitude", "brightness", "daynight", "acq_date", "acq_time", "daynight", "type")]

####################################
##### Datetime                 #####
####################################

fire_data$datetime <- as.POSIXct(
  paste(
    fire_data$acq_date,
    sprintf("%04d", fire_data$acq_time)
  ),
  format = "%Y-%m-%d %H%M",
  tz = "UTC"
)

##################################
##### SZA/Nighttime          #####
##################################

fire_data_crs_4326 <- st_as_sf(
  fire_data,
  coords = c("longitude", "latitude"),
  crs = 4326   # WGS84
)

coords <- st_coordinates(fire_data_crs_4326)


fire_data_crs_4326 <- fire_data_crs_4326 %>%
  mutate(
    longitude = coords[, "X"],
    latitude  = coords[, "Y"]
  )

pos <- getSunlightPosition(
  data = data.frame(
    date = fire_data_crs_4326$datetime,
    lat  = fire_data_crs_4326$latitude,
    lon  = fire_data_crs_4326$longitude
  )
)

fire_data_crs_4326$sza <- 90 - (pos$altitude * 180 / pi)

#################################
##### Geocoding             #####
#################################


# 1) Load US Cities

au_cities <- read.csv("MSISS/wildfires/main/data/cities/au_cities.csv")
# Transform
au_cities <- au_cities %>%
  rename(state_name = admin_name)


fires_sf <- st_as_sf(fire_data_crs_4326, coords = c("longitude", "latitude"), crs = 4326)
cities_sf <- st_as_sf(au_cities, coords = c("lng", "lat"), crs = 4326)

fires_sf_m <- st_transform(fires_sf, crs = 3857)
cities_sf_m <- st_transform(cities_sf, crs = 3857)


# 2) Assign Nearest US City

nearest <- st_nn(fires_sf_m, cities_sf_m, k = 1, maxdist = 200000, returnDist = TRUE)

fire_data_crs_4326$nearest_city <- sapply(nearest$nn, function(x) {
  if(length(x) == 0) {
    NA
  } else {
    au_cities$city[x]
  }
})


# 3) Add State and Population

fire_data_crs_4326 <- fire_data_crs_4326 %>%
  mutate(
    city = sapply(nearest$nn, function(x) if(length(x) == 0) NA else au_cities$city[x]),
    state = sapply(nearest$nn, function(x) if(length(x) == 0) NA else au_cities$state_name[x]),
    population = sapply(nearest$nn, function(x) if(length(x) == 0) NA else au_cities$population[x])
  )

#############################################
##### Add all Fields to Fire Data       #####
#############################################

fire_data$city       <- fire_data_crs_4326$city
fire_data$state      <- fire_data_crs_4326$state
fire_data$population <- fire_data_crs_4326$population
fire_data$daynight <- fire_data_crs_4326$daynight.1

###########################
##### Write to CSV    #####
###########################

write.csv(
  fire_data,
  "MSISS/wildfires/main/data/au/au_fires_full_dataset.csv",
  row.names = FALSE
)

