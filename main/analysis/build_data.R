



au_data <- read.csv("MSISS/wildfires/main/data/au_wildfires_2020.csv")
us_data <- read.csv("MSISS/wildfires/main/data/us_wildfires_2020.csv")


##############################
##### Time Histograms   ######
##############################

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

hist(us_data$acq_time)
us_data$acq_date <- as.Date(us_data$acq_date)
hist(
  us_data$acq_date,
  breaks = "months",
  main = "Histogram of Acquisition Dates",
  xlab = "Acquisition Date",
  col = "lightblue",
  border = "white"
)


################################
##### Non Wildfire Data   ######
################################

# Define the study area

install.packages("sf")
library(sf)

fires <- st_as_sf(
  au_data,
  coords = c("longitude", "latitude"),
  crs = 4326
)

study_area <- st_bbox(fires)

# Non fire points

set.seed(123)

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

# Combine with fires.
fires$wildfire <- 1

fire_data <- rbind(
  fires[, "wildfire"],
  nonfire[, "wildfire"]
)


# 

sum(fire_data$wildfire)
# 156417


##########################
### Regular Predictors ###
##########################

# Here, we take our simulated non fire data, add them together with observed wildfire data, and
# add in type, datetime, and brightness. 
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

fires_type_clean <- fires_type[, c("wildfire", "brightness", "type", "datetime")]

fire_data <- rbind(
  fires_type_clean,
  nonfire_type
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
# 312834 156417 

# TODO: Pivot Back to make just as much progress on the US data.















