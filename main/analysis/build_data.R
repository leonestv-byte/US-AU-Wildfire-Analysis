
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













