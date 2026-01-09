
##############################
##### Build Datasets    ######
##############################

# - Load Datasets from 2020

# 1) Australia
au_data <- read.csv("MSISS/wildfires/main/data/au_wildfires_2020.csv")

# 2) United States
us_data <- read.csv("MSISS/wildfires/main/data/us_wildfires_2020.csv")


##############################
##### Time Histograms   ######
##############################

# 1) Australia
au_data
# latitude longitude brightness scan track   acq_date acq_time satellite instrument confidence version bright_t31  frp daynight type
# 1  -13.2051  143.1472      337.2  1.0   1.0 2020-01-01       50     Terra      MODIS         87    6.03      298.0 27.9        D    0
# 2  -14.4737  143.5811      351.4  1.0   1.0 2020-01-01       51     Terra      MODIS         95    6.03      300.4 55.8        D    0
# 3  -14.4750  143.5905      321.7  1.0   1.0 2020-01-01       51     Terra      MODIS         57    6.03      299.0  7.6        D    0
# 4  -14.4828  143.5797      321.6  1.0   1.0 2020-01-01       51     Terra      MODIS         58    6.03      299.8  8.0        D    0
# 5  -13.6425  136.9439      312.6  1.9   1.4 2020-01-01       51     Terra      MODIS         55    6.03      293.5 18.8        D    3

au_data$acq_date
au_data$acq_time

hist(au_data$acq_time)
au_data$acq_date <- as.Date(au_data$acq_date)
hist(
  au_data$acq_date,
  breaks = "months",
  main = "Histogram of Acquisition Dates",
  xlab = "Acquisition Date",
  col = "lightblue",
  border = "white"
)


################################
##### Non Wildfire Data   ######
################################


# 1) Australia

install.packages("sf")
library(sf)

# 1a) Region Boundary.
# Define the study area.
# Ex, get a bounding box for the region.
fires <- st_as_sf(
  au_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

study_area <- st_bbox(fires)
# xmin     ymin     xmax     ymax 
# 113.1446 -43.5006 153.5515  -9.2741 

# 1b) Nonfire sampled from regional boundary.

set.seed(321)

n_nonfire <- nrow(au_data) * 2  # common ratio 1:2 or 1:3

nonfire_pts <- st_sample(
  st_as_sfc(study_area),
  size = n_nonfire,
  type = "random"
)

nonfire <- st_sf(
  data.frame(
    wildfire = 0
  ),
  geometry = nonfire_pts
)

# 1c) Combine with fires.
fires$wildfire <- 1

fire_data <- rbind(
  fires[, "wildfire"],
  nonfire[, "wildfire"]
)

sum(fire_data$wildfire)
# 156417


##########################
### Regular Predictors ###
##########################
# Add in predictors from the main dataset.

# 1) Australia

st_crs(fires)
st_crs(nonfire)

fires_type <- fires[, c("wildfire", "brightness", "acq_date", "acq_time", "type")]
nonfire_type <- nonfire[, "wildfire", "acq_date", "acq_time"]


nearest_idx <- st_nearest_feature(nonfire_type, fires_type)

nonfire_type$type <- fires_type$type[nearest_idx]

nonfire_type$brightness = 0


fires_type$datetime <- as.POSIXct(
  paste(
    fires_type$acq_date,
    sprintf("%04d", fires_type$acq_time)
  ),
  format = "%Y-%m-%d %H%M",
  tz = "Australia/Sydney"
)


set.seed(321)
nonfire_type$datetime <- sample(
  fires_type$datetime,
  size = nrow(nonfire_type),
  replace = TRUE
)

# Setup a buffer so that fire and non simulated fire data do not
# show up together.
max_dist  <- units::set_units(1, km)     # spatial buffer
max_time  <- as.difftime(1, units = "hours") # temporal buffer

idx <- st_nearest_feature(nonfire_type, fires_type)
nearest_fire <- fires_type[idx, ]

dist_space <- st_distance(nonfire_type, nearest_fire, by_element = TRUE)

dist_time <- abs(
  difftime(nonfire_type$datetime, nearest_fire$datetime, units = "hours")
)

valid_nonfire <- !(
  dist_space <= max_dist &
    dist_time  <= max_time
)

nonfire_clean <- nonfire_type[valid_nonfire, ]

fires_type_clean <- fires_type[, c("wildfire", "brightness", "type", "datetime")]

fire_data <- rbind(
  fires_type_clean,
  nonfire_clean
)

###################
### Histograms ###
##################

hist(
  fire_data$datetime,
  breaks = "months",
  main = "Histogram of Acquisition Dates",
  xlab = "Acquisition Date",
  col = "lightblue",
  border = "white"
)

table(fire_data$wildfire)
# 0      1 
# 312,825 156,417 

###########################################################################
###### Add Daynight back in, calculated with suncalc for nonfire data #####
###########################################################################


# library(lubridate)
# fire_data$datetime_utc <- with_tz(fire_data$datetime, "UTC")

install.packages(("suncalc"))
library(suncalc)

library(sf) 
coords <- st_coordinates(fire_data)
fire_data$longitude <- coords[, 1]
fire_data$latitude  <- coords[, 2]

pos <- getSunlightPosition(
  data = data.frame(
    date = fire_data$datetime,
    lat  = fire_data$latitude,
    lon  = fire_data$longitude
  )
)

fire_data$sza <- 90 - (pos$altitude * 180 / pi)

fire_data$computed_daynight_sza <- ifelse(
  fire_data$sza < 85, "N", "D"
)

# Sanity Checking
table(fire_data$computed_daynight_sza[fire_data$wildfire == 1])
# D      N 
# 120993  35422 
table(au_data$daynight)
# D      N 
# 120995  35422 
table(fire_data$computed_daynight_sza)
# D      N 
# 363116 106120 



############################################
###### Add data from third party API's #####
############################################


############################################
###### Geocoding - Add City            #####
############################################


fire_data
# datetime_utc longitude latitude      sza computed_daynight_sza
# 1  2019-12-31 13:50:00  143.1472 -13.2051 142.3345                     D
# 2  2019-12-31 13:51:00  143.5811 -14.4737 141.2765                     D
au_cities <- read.csv("MSISS/wildfires/main/data/cities/au_cities.csv")
au_cities
# city      lat      lng   country iso2                   admin_name
# 1            Melbourne -37.8142 144.9631 Australia   AU                     Victoria
# 2               Sydney -33.8678 151.2100 Australia   AU              New South Wales
# 3             Brisbane -27.4678 153.0281 Australia   AU                   Queensland
# 4                Perth -31.9559 115.8606 Australia   AU            Western Australia

library(sf)
library(dplyr)

# Fire points
fires_sf <- st_as_sf(fire_data, coords = c("longitude", "latitude"), crs = 4326)

# Cities
cities_sf <- st_as_sf(au_cities, coords = c("lng", "lat"), crs = 4326)


# Transform to metric CRS for accurate distances in meters
fires_sf_m <- st_transform(fires_sf, crs = 3857)
cities_sf_m <- st_transform(cities_sf, crs = 3857)

# Compute distance matrix (fires x cities)
dist_matrix <- st_distance(fires_sf_m, cities_sf_m) / 1000  # convert meters to km


max_distance_km <- 200

fire_data$nearest_city <- apply(dist_matrix, 1, function(x) {
  if(min(x) <= max_distance_km) {
    au_cities$city[which.min(x)]
  } else {
    NA  # no city nearby
  }
})

# Add population data:
library(dplyr)

# Join fire_data with au_cities to get population
fire_data <- fire_data %>%
  left_join(
    au_cities %>% select(city, population) %>% rename(nearest_city_population = population),
    by = c("nearest_city" = "city")
  )

head(fire_data)
table(fire_data$nearest_city)

city_counts <- table(fire_data$nearest_city, useNA = "ifany")
city_counts <- table(fire_data$nearest_city)
# 332,100 data with no city
# 152,052 data with a city
# 484,152 data total

# Fires

city_counts_fires <- table(fire_data$nearest_city[fire_data$wildfire == 1], useNA = "ifany")
city_counts_fires <- table(fire_data$nearest_city[fire_data$wildfire == 1])
barplot(city_counts_fires,
        main = "Cities with Fires Count",
        xlab = "City",
        ylab = "Data Points",
        las = 2,          # rotate city labels vertically
        col = "tomato")
#  sum(city_counts_fires[!is.na(names(city_counts_fires))])
# 84,039 data with fires and a city
# 80,974 with fires and no city

# No fires

city_counts_no_fires <- table(fire_data$nearest_city[fire_data$wildfire == 0], useNA = "ifany")
city_counts_no_fires <- table(fire_data$nearest_city[fire_data$wildfire == 0])
# 251,126 points with no fires with no city
# 68,013 points with no fires and a city
barplot(city_counts_fires,
        main = "Cities with No Fires Count",
        xlab = "City",
        ylab = "Data Points",
        las = 2,          # rotate city labels vertically
        col = "tomato")



mean(fire_data$population[!is.na(fire_data$nearest_city)])
# 152264.2

# Add the state
fire_data <- fire_data %>%
  left_join(au_cities %>% select(city, admin_name), 
            by = c("nearest_city" = "city")) %>%
  rename(state = admin_name)



# Weather
# TODO: see if I still need these.
install.packages("weatherOz")
library(weatherOz)

fire_data
# fire_data
# wildfire brightness type            datetime           geometry        datetime_utc longitude latitude      sza computed_daynight_sza
# 1         1      337.2    0 2020-01-01 00:50:00 143.1472, -13.2051 2019-12-31 13:50:00  143.1472 -13.2051 142.3345                     D
# 2         1      351.4    0 2020-01-01 00:51:00 143.5811, -14.4737 2019-12-31 13:51:00  143.5811 -14.4737 141.2765                     D
# 3         1      321.7    0 2020-01-01 00:51:00 143.5905, -14.4750 2019-12-31 13:51:00  143.5905 -14.4750 141.2774                     D
# 4         1      321.6    0 2020-01-01 00:51:00 143.5797, -14.4828 2019-12-31 13:51:00  143.5797 -14.4828 141.2673                     D
# 5         1      312.6    3 2020-01-01 00:51:00 136.9439, -13.6425 2019-12-31 13:51:00  136.9439 -13.6425 139.9907                     D
# 6         1      337.5    0 2020-01-01 00:51:00 143.7672, -14.7123 2019-12-31 13:51:00  143.7672 -14.7123 141.0880                     D

# library(httr)
# library(jsonlite)

# latitude  <- -33.87
# longitude <- 151.21

# start_date <- "2020-01-01"
# end_date   <- "2020-12-31"

# url <- sprintf(
 # paste0(
#    "https://archive-api.open-meteo.com/v1/archive?",
 #   "latitude=%s&longitude=%s",
 #   "&start_date=%s&end_date=%s",
#    "&hourly=temperature_2m",
 #   "&timezone=Australia/Perth"
#  ),
#   latitude, longitude, start_date, end_date
# )

# resp <- GET(url)
# stop_for_status(resp)

# data <- fromJSON(content(resp, "text", encoding = "UTF-8"))

# weather_df <- data.frame(
#  datetime = as.POSIXct(data$hourly$time, tz = "Australia/Perth"),
#  temp_C   = data$hourly$temperature_2m
# )

# library(lubridate)
# Convert ISO datetime string with hours to POSIXct
# weather_df$datetime <- ymd_hm(data$hourly$time, tz = "Australia/Perth")
# Extract hour
# weather_df$hour <- hour(weather_df$datetime)
# head(weather_df)




###################

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(sf)

# Round datetime to nearest hour for matching with weather
fire_data$datetime_hour <- floor_date(fire_data$datetime, unit = "hour")


# Initialize column
fire_data$temp_C <- NA

# Number of points to loop through at once (demo with 10)
N <- nrow(fire_data)  # use all points carefully
for(i in 1:N){
  lat <- fire_data$latitude[i]
  lon <- fire_data$longitude[i]
  
  # Use date only from datetime (Open-Meteo archive API requires start/end dates)
  date_str <- format(fire_data$datetime[i], "%Y-%m-%d")
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=temperature_2m",
      "&timezone=Australia/Perth"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime = ymd_hm(data$hourly$time, tz = "Australia/Perth"),
        temp_C   = data$hourly$temperature_2m
      )
      
      # Match temperature by nearest hour
      idx <- which.min(abs(difftime(weather_df$datetime, fire_data$datetime[i], units = "hours")))
      fire_data$temp_C[i] <- weather_df$temp_C[idx]
    }
  } else {
    warning("Failed request for point ", i)
  }
  
  if(i %% 10 == 0) cat("Processed", i, "points\n")
}


head(fire_data[, c("datetime", "latitude", "longitude", "temp_C")])



###################
# Google Elevation Data (From Maps)
###################
library(httr)
library(jsonlite)
install.packages("usethis")
usethis::edit_r_environ()

fire_data
# wildfire brightness type            datetime longitude latitude      sza computed_daynight_sza    nearest_city nearest_city_population      state
#  1         1      337.2    0 2020-01-01 00:50:00  143.1472 -13.2051 142.3345                     D            <NA>                      NA       <NA>
#  2         1      351.4    0 2020-01-01 00:51:00  143.5811 -14.4737 141.2765                     D            <NA>                      NA       <NA>
#  3         1      321.7    0 2020-01-01 00:51:00  143.5905 -14.4750 141.2774                     D            <NA>                      NA       <NA>
#  4         1      321.6    0 2020-01-01 00:51:00  143.5797 -14.4828 141.2673                     D            <NA>                      NA       <NA>
#  5         1      312.6    3 2020-01-01 00:51:00  136.9439 -13.6425 139.9907                     D            <NA>                      NA       <NA>


# api_key <- Sys.getenv("MY_API_KEY")
# latitude <-  -33.87
# longitude <- 151.21
# url <- paste0(
#  "https://maps.googleapis.com/maps/api/elevation/json?",
#  "locations=", latitude, ",", longitude,
#  "&key=", api_key
# )
# response <- GET(url)
# data <- fromJSON(content(response, "text", encoding = "UTF-8"))
# data$results$elevation
# 27.06797

api_key <- Sys.getenv("MY_API_KEY")

get_elevation <- function(lat, lon, key) {
  url <- paste0(
    "https://maps.googleapis.com/maps/api/elevation/json?",
    "locations=", lat, ",", lon,
    "&key=", key
  )
  
  resp <- GET(url)
  dat <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  
  if (length(dat$results) == 0) return(NA_real_)
  dat$results$elevation
}

fire_data$elevation <- mapply(
  get_elevation,
  fire_data$latitude,
  fire_data$longitude,
  MoreArgs = list(key = api_key)
)

# test
n_test <- 500

fire_data$elevation <- NA_real_

fire_data$elevation[1:n_test] <- mapply(
  get_elevation,
  fire_data$latitude[1:n_test],
  fire_data$longitude[1:n_test],
  MoreArgs = list(key = api_key)
)
