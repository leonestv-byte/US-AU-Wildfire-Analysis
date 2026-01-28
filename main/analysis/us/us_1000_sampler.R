# Imports
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(sf)


fire_data <- read.csv("MSISS/wildfires/main/data/us/us_fires_full_dataset.csv")
non_fire_data <- read.csv("MSISS/wildfires/main/data/us/us_no_fires_full_dataset.csv")

fire_data$wildfire <- 1
non_fire_data$wildfire <- 0

####################
##### Sample  ######
####################


set.seed(321)
N = 1000
us_sample_fires <- fire_data[fire_data$wildfire == 1 & !is.na(fire_data$datetime),][sample(sum(fire_data$wildfire == 1 & !is.na(fire_data$datetime)),N),]
us_sample_no_fires <- non_fire_data[non_fire_data$wildfire == 0 & !is.na(non_fire_data$datetime), ][sample(sum(non_fire_data$wildfire == 0), N),]

####################
##### Temp C  ######
####################

us_sample_fires$temp_C <- NA

for(i in 1:N){
  if (!is.na(us_sample_fires$temp_C[i])) {
    next
  }
  
  lat <- us_sample_fires$latitude[i]
  lon <- us_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=temperature_2m",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime = ymd_hm(data$hourly$time, tz = "UTC"),
        temp_C   = data$hourly$temperature_2m
      )
      
      # Match temperature by nearest hour
      idx <- which.min(abs(difftime(weather_df$datetime, us_sample_fires$datetime[i], units = "hours")))
      us_sample_fires$temp_C[i] <- weather_df$temp_C[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f",
        i,
        us_sample_fires$temp_C[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


us_sample_no_fires$temp_C <- NA

for(i in 1:N){
  if (!is.na(us_sample_no_fires$temp_C[i])) {
    next
  }
  
  lat <- us_sample_no_fires$latitude[i]
  lon <- us_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=temperature_2m",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime = ymd_hm(data$hourly$time, tz = "UTC"),
        temp_C   = data$hourly$temperature_2m
      )
      
      # Match temperature by nearest hour
      idx <- which.min(abs(difftime(weather_df$datetime, us_sample_no_fires$datetime[i], units = "hours")))
      us_sample_no_fires$temp_C[i] <- weather_df$temp_C[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f",
        i,
        us_sample_no_fires$temp_C[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}

####################
##### Wind    ######
####################

us_sample_fires$wind <- NA

for(i in 1:N){
  if (!is.na(us_sample_fires$wind[i])) {
    next
  }
  
  lat <- us_sample_fires$latitude[i]
  lon <- us_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=wind_speed_10m",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime = ymd_hm(data$hourly$time, tz = "UTC"),
        wind_speed_10m = data$hourly$wind_speed_10m
      )
      
      # Match wind speed by nearest hour
      idx <- which.min(abs(difftime(
        weather_df$datetime,
        us_sample_fires$datetime[i],
        units = "hours"
      )))
      
      us_sample_fires$wind[i] <- weather_df$wind_speed_10m[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f m/s",
        i,
        us_sample_fires$wind[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


us_sample_no_fires$wind <- NA

for(i in 1:N){
  if (!is.na(us_sample_no_fires$wind[i])) {
    next
  }
  
  lat <- us_sample_no_fires$latitude[i]
  lon <- us_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=wind_speed_10m",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime = ymd_hm(data$hourly$time, tz = "UTC"),
        wind_speed_10m = data$hourly$wind_speed_10m
      )
      
      # Match wind speed by nearest hour
      idx <- which.min(abs(difftime(
        weather_df$datetime,
        us_sample_no_fires$datetime[i],
        units = "hours"
      )))
      
      us_sample_no_fires$wind[i] <- weather_df$wind_speed_10m[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f m/s",
        i,
        us_sample_no_fires$wind[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


#######################
##### Humidity   ######
#######################

us_sample_fires$humidity <- NA


for(i in 1:N){
  if (!is.na(us_sample_fires$humidity[i])) {
    next
  }
  
  lat <- us_sample_fires$latitude[i]
  lon <- us_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=relative_humidity_2m",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime = ymd_hm(data$hourly$time, tz = "UTC"),
        rh_2m = data$hourly$relative_humidity_2m
      )
      
      # Nearest hour match
      idx <- which.min(abs(difftime(
        weather_df$datetime,
        us_sample_fires$datetime[i],
        units = "hours"
      )))
      
      us_sample_fires$humidity[i] <- weather_df$rh_2m[idx]
      
      print(sprintf(
        "Processed request for point %d: %.1f%%",
        i,
        us_sample_fires$humidity[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}

us_sample_no_fires$humidity <- NA

for(i in 1:N){
  if (!is.na(us_sample_no_fires$humidity[i])) {
    next
  }
  
  lat <- us_sample_no_fires$latitude[i]
  lon <- us_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=relative_humidity_2m",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime = ymd_hm(data$hourly$time, tz = "UTC"),
        rh_2m = data$hourly$relative_humidity_2m
      )
      
      # Nearest hour match
      idx <- which.min(abs(difftime(
        weather_df$datetime,
        us_sample_no_fires$datetime[i],
        units = "hours"
      )))
      
      us_sample_no_fires$humidity[i] <- weather_df$rh_2m[idx]
      
      print(sprintf(
        "Processed request for point %d: %.1f%%",
        i,
        us_sample_no_fires$humidity[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}

################################
##### Precipitation.      ######
################################

us_sample_fires$precipitation <- NA

for(i in 1:N){
  if (!is.na(us_sample_fires$precipitation[i])) {
    next
  }
  
  lat <- us_sample_fires$latitude[i]
  lon <- us_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=precipitation",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime   = ymd_hm(data$hourly$time, tz = "UTC"),
        precip_mm = data$hourly$precipitation
      )
      
      # Match precipitation by nearest hour
      idx <- which.min(
        abs(difftime(weather_df$datetime,
                     us_sample_fires$datetime[i],
                     units = "hours"))
      )
      
      us_sample_fires$precipitation[i] <- weather_df$precip_mm[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f mm",
        i,
        us_sample_fires$precipitation[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


us_sample_no_fires$precipitation <- NA

for(i in 1:N){
  if (!is.na(us_sample_no_fires$precipitation[i])) {
    next
  }
  
  lat <- us_sample_no_fires$latitude[i]
  lon <- us_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(us_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=precipitation",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime   = ymd_hm(data$hourly$time, tz = "UTC"),
        precip_mm = data$hourly$precipitation
      )
      
      # Match precipitation by nearest hour
      idx <- which.min(
        abs(difftime(weather_df$datetime,
                     us_sample_no_fires$datetime[i],
                     units = "hours"))
      )
      
      us_sample_no_fires$precipitation[i] <- weather_df$precip_mm[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f mm",
        i,
        us_sample_no_fires$precipitation[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


################################
##### Soil/Earth Data     ######
################################
ee_Authenticate()
ee_check()

rgee::ee_clean_user_credentials()
ee_Authenticate()
library(rgee)
Sys.setenv(SSL_CERT_FILE = "/etc/ssl/cert.pem")
ee_Initialize(project = 'wildfiredata979')

# modis_ndvi <- ee$ImageCollection("MODIS/061/MOD13A1")$select("NDVI")

# get_ndvi_for_point <- function(lat, lon, date) {
 # point <- ee$Geometry$Point(c(lon, lat))
  
#  start <- as.character(as.Date(date) - 1)
#  end <- as.character(as.Date(date) + 1)
  
 # img <- modis_ndvi$filterDate(start, end)$first()
#  if (is.null(img)) return(NA)
  
 # ndvi_value <- tryCatch({
 #   img$reduceRegion(
 #     reducer = ee$Reducer$mean(),
 #     geometry = point,
  #    scale = 500
 #   )$get("NDVI")$getInfo()
#  }, error = function(e) NA)
#  
#  return(ndvi_value)
# }

# ndvi <- us_sample_fires[1:10] %>%
#  rowwise() %>%
#  mutate(NDVI = get_ndvi_for_point(latitude, longitude, acq_date)) %>%
#  ungroup()

# row format:

# One working version:

# ndvi_values <- numeric(nrow(us_sample_fires))

# V2:

###########################
## NDVI Retrieval Method ##
###########################

modis_ndvi <- ee$ImageCollection("MODIS/061/MOD13A1")$
  select("NDVI")

get_ndvi_for_point <- function(lat, lon, start_date, end_date) {
  
  point <- ee$Geometry$Point(c(lon, lat))
  
  img <- modis_ndvi$
    filterDate(as.character(start_date), as.character(end_date))$
    median()
  
  ndvi_value <- tryCatch({
    img$reduceRegion(
      reducer  = ee$Reducer$mean(),
      geometry = point,
      scale    = 500,
      bestEffort = TRUE
    )$get("NDVI")$getInfo()
  }, error = function(e) NA)
  
  if (is.null(ndvi_value) || length(ndvi_value) == 0) {
    return(NA_real_)
  }
  
  return(ndvi_value)
}


us_sample_fires$ndvi <- NA_real_

n <- nrow(us_sample_fires)


for (i in seq_len(n)) {
  if (!is.na(us_sample_fires$ndvi[i])) {
    next
  }
  
  fire_date <- as.Date(us_sample_fires$acq_date[i])
  
  start_date <- fire_date - 17
  end_date   <- fire_date - 1
  
  us_sample_fires$ndvi[i] <- get_ndvi_for_point(
    lat  = us_sample_fires$latitude[i],
    lon  = us_sample_fires$longitude[i],
    start_date = start_date,
    end_date   = end_date
  )
  
  if (i %% 10 == 0) {
    message("Processed ", i, " / ", n)
  }
}
# TODO: 0 is bad form, but, there is only one value missing
us_sample_fires$ndvi[is.na(us_sample_fires$ndvi)] <- 0
# Rescale NDVI (very important)
us_sample_fires$ndvi <- us_sample_fires$ndvi / 10000




us_sample_no_fires$ndvi <- NA_real_

n <- nrow(us_sample_no_fires)


for (i in seq_len(n)) {
  if (!is.na(us_sample_no_fires$ndvi[i])) {
    next
  }
  
  datetime <- as.POSIXct(
    us_sample_no_fires$datetime[i],
    format = "%Y-%m-%d %H:%M:%OS",
    tz = "UTC"
  )
  
  acq_date <- as.Date(datetime)
  
  fire_date <- as.Date(acq_date)
  
  start_date <- fire_date - 17
  end_date   <- fire_date - 1
  
  us_sample_no_fires$ndvi[i] <- get_ndvi_for_point(
    lat  = us_sample_no_fires$latitude[i],
    lon  = us_sample_no_fires$longitude[i],
    start_date = start_date,
    end_date   = end_date
  )
  
  if (i %% 10 == 0) {
    message("Processed ", i, " / ", n)
  }
}
# TODO: 0 is bad form, but, there is only one value missing
us_sample_no_fires$ndvi[is.na(us_sample_no_fires$ndvi)] <- 0
# Rescale NDVI (very important)
us_sample_no_fires$ndvi <- us_sample_no_fires$ndvi / 10000




####################
##### AQI     ######
####################



##########################
##### Elevation     ######
##########################




#############################
##### Write to CSV     ######
#############################


write.csv(
  us_sample_fires,
  "MSISS/wildfires/main/data/1000_samples/us_1000_sample_fires.csv",
  row.names = FALSE
)

write.csv(
  us_sample_no_fires,
  "MSISS/wildfires/main/data/1000_samples/us_1000_sample_no_fires.csv",
  row.names = FALSE
)


