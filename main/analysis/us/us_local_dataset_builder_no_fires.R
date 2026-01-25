# Imports
install.packages("sf")   # only if not installed
install.packages(("suncalc"))
install.packages("nngeo")
library(suncalc)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(nngeo)
library(dplyr)
library(lubridate)

#######################################
##### US - No Fires Full Dataset  #####
#######################################

us_data <- read.csv("MSISS/wildfires/main/data/us/us_fires_full_dataset.csv")


# 1) Load all raw fields

fire_data <- us_data[, c("latitude", "longitude", "brightness", "daynight", "acq_date", "acq_time", "daynight", "type", "state", "city", "population", "datetime")]


#################
##### Seed  #####
#################

set.seed(321)



########################################
##### US - Sample from the Region  #####
########################################

n_nonfire <- nrow(fire_data) * 2
us <- ne_countries(country = "United States of America", returnclass = "sf")
nonfire_pts <- st_sample(
  us,
  size = n_nonfire,
  type = "random"
)
nonfire <- st_sf(
  data.frame(
    wildfire = 0
  ),
  geometry = nonfire_pts
)

#######################
##### Datetime    #####
#######################

fire_data$datetime <- as.POSIXct(fire_data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

start_time <- min(fire_data$datetime, na.rm = TRUE)
end_time   <- max(fire_data$datetime, na.rm = TRUE)

nonfire$datetime <- as.POSIXct(runif(nrow(nonfire), as.numeric(start_time), as.numeric(end_time)), origin = "1970-01-01", tz = "UTC")


###################################################
##### US - Remove Samples Too Close to Fires  #####
###################################################

# --- 1. Project to metric CRS (meters) ---
nonfire_type_m <- st_transform(nonfire, 3577)
fire_type_m <- st_as_sf(
  fire_data,
  coords = c("longitude", "latitude"),  # replace with your column names
  crs = 4326,  # WGS84, typical for lon/lat
  remove = FALSE  # keeps the original lon/lat columns
) %>%
  st_transform(3577)

# --- 2. Buffer fires by 5 km ---
fire_buffer <- st_buffer(fire_type_m, 5000)

# --- 3. Remove nonfire points within 5 km ---
# Use sparse = TRUE to avoid huge matrix
intersects_list <- st_intersects(nonfire_type_m, fire_buffer, sparse = TRUE)

# Keep only points that do NOT intersect any fire
nonfire_safe_space <- nonfire_type_m[lengths(intersects_list) == 0, ]


# --- 4. Convert datetimes ---
# Convert datetimes
nonfire_safe_space$datetime <- as.POSIXct(nonfire_safe_space$datetime, tz = "UTC")
fire_data$datetime <- as.POSIXct(fire_data$datetime, tz = "UTC")

time_threshold <- 3 * 3600  # 3 hours in seconds

# Vectorized time filtering
fire_times <- as.numeric(fire_data$datetime)

valid_times <- !is.na(nonfire_safe_space$datetime)
too_close_time <- sapply(as.numeric(nonfire_safe_space$datetime[valid_times]), function(t) {
  any(abs(fire_times - t) <= time_threshold, na.rm = TRUE)
})

nonfire <- nonfire_safe_space[valid_times, ][!too_close_time, ]



##################################
##### SZA/Nighttime          #####
##################################

nonfire_crs_4326 <- st_as_sf(
  nonfire,
  coords = c("longitude", "latitude"),
  crs = 4326   # WGS84
)

coords <- st_coordinates(nonfire_crs_4326)


nonfire_crs_4326 <- nonfire_crs_4326 %>%
  mutate(
    longitude = coords[, "X"],
    latitude  = coords[, "Y"]
  )

pos <- getSunlightPosition(
  data = data.frame(
    date = nonfire_crs_4326$datetime,
    lat  = nonfire_crs_4326$latitude,
    lon  = nonfire_crs_4326$longitude
  )
)

nonfire_crs_4326$sza <- 90 - (pos$altitude * 180 / pi)

nonfire_crs_4326$computed_daynight_sza <- ifelse(
  nonfire_crs_4326$sza < 85, "N", "D"
)


############################
##### Geocoding        #####
############################

#us-cities: link back to https://simplemaps.com/data/us-cities
us_cities <- read.csv("MSISS/wildfires/main/data/us_cities.csv")

fires_sf <- st_as_sf(nonfire_crs_4326, coords = c("longitude", "latitude"), crs = 4326)
cities_sf <- st_as_sf(us_cities, coords = c("lng", "lat"), crs = 4326)

fires_sf_m <- st_transform(fires_sf, crs = 3857)
cities_sf_m <- st_transform(cities_sf, crs = 3857)


# 2) Assign Nearest US City

nearest <- st_nn(fires_sf_m, cities_sf_m, k = 1, maxdist = 200000, returnDist = TRUE)

nonfire_crs_4326$nearest_city <- sapply(nearest$nn, function(x) {
  if(length(x) == 0) {
    NA
  } else {
    us_cities$city[x]
  }
})


# 3) Add State and Population

nonfire_crs_4326 <- nonfire_crs_4326 %>%
  mutate(
    city = sapply(nearest$nn, function(x) if(length(x) == 0) NA else us_cities$city[x]),
    state = sapply(nearest$nn, function(x) if(length(x) == 0) NA else us_cities$state_name[x]),
    population = sapply(nearest$nn, function(x) if(length(x) == 0) NA else us_cities$population[x])
  )

#############################################
##### Add all Fields to Fire Data       #####
#############################################

# Convert back to latitude, and longitude
nonfire_crs_4326 <- st_transform(nonfire_crs_4326, crs = 4326)

# Get coordinates
coords <- st_coordinates(nonfire_crs_4326)

# Add them as new columns
nonfire_crs_4326$longitude <- coords[,1]
nonfire_crs_4326$latitude  <- coords[,2]


nonfire$city       <- nonfire_crs_4326$city
nonfire$state      <- nonfire_crs_4326$state
nonfire$population <- nonfire_crs_4326$population
nonfire$daynight <- nonfire_crs_4326$daynight.1
nonfire$latitude <- nonfire_crs_4326$latitude
nonfire$longitude <- nonfire_crs_4326$longitude
nonfire$computed_daynight_sza <- nonfire_crs_4326$computed_daynight_sza

# Make latitude/longitude legible
nonfire_wgs84 <- st_transform(nonfire_crs_4326, crs = 4326)
coords <- st_coordinates(nonfire_wgs84)
nonfire_wgs84 <- nonfire_wgs84 %>%
  mutate(
    longitude = coords[, "X"],
    latitude  = coords[, "Y"]
  )

nonfire$latitude <- nonfire_wgs84$latitude
nonfire$longitude <- nonfire_wgs84$longitude

#drop geometry
nonfire <- st_drop_geometry(nonfire)

#other
nonfire$brightness <- 0

###########################
##### Write to CSV    #####
###########################

write.csv(
  nonfire,
  "MSISS/wildfires/main/data/us/us_no_fires_full_dataset.csv",
  row.names = FALSE
)




