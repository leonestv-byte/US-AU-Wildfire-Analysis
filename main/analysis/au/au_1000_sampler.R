# Imports
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(sf)


fire_data <- read.csv("MSISS/wildfires/main/data/au/au_fires_full_dataset.csv")
non_fire_data <- read.csv("MSISS/wildfires/main/data/au/au_no_fires_full_dataset.csv")

fire_data$wildfire <- 1
non_fire_data$wildfire <- 0

####################
##### Sample  ######
####################


set.seed(321)
N = 1000
au_sample_fires <- fire_data[fire_data$wildfire == 1 & !is.na(fire_data$datetime),][sample(sum(fire_data$wildfire == 1 & !is.na(fire_data$datetime)),N),]
au_sample_no_fires <- non_fire_data[non_fire_data$wildfire == 0 & !is.na(non_fire_data$datetime), ][sample(sum(non_fire_data$wildfire == 0), N),]

####################
##### Temp C  ######
####################

au_sample_fires$temp_C <- NA

for(i in 1:N){
  if (!is.na(au_sample_fires$temp_C[i])) {
    next
  }
  
  lat <- au_sample_fires$latitude[i]
  lon <- au_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
      idx <- which.min(abs(difftime(weather_df$datetime, au_sample_fires$datetime[i], units = "hours")))
      au_sample_fires$temp_C[i] <- weather_df$temp_C[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f",
        i,
        au_sample_fires$temp_C[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


au_sample_no_fires$temp_C <- NA

for(i in 1:N){
  if (!is.na(au_sample_no_fires$temp_C[i])) {
    next
  }
  
  lat <- au_sample_no_fires$latitude[i]
  lon <- au_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
      idx <- which.min(abs(difftime(weather_df$datetime, au_sample_no_fires$datetime[i], units = "hours")))
      au_sample_no_fires$temp_C[i] <- weather_df$temp_C[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f",
        i,
        au_sample_no_fires$temp_C[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}

####################
##### Wind    ######
####################

au_sample_fires$wind <- NA

for(i in 1:N){
  if (!is.na(au_sample_fires$wind[i])) {
    next
  }
  
  lat <- au_sample_fires$latitude[i]
  lon <- au_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
        au_sample_fires$datetime[i],
        units = "hours"
      )))
      
      au_sample_fires$wind[i] <- weather_df$wind_speed_10m[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f m/s",
        i,
        au_sample_fires$wind[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}

au_sample_no_fires$wind <- NA


for(i in 1:N){
  if (!is.na(au_sample_no_fires$wind[i])) {
    next
  }
  
  lat <- au_sample_no_fires$latitude[i]
  lon <- au_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
        au_sample_no_fires$datetime[i],
        units = "hours"
      )))
      
      au_sample_no_fires$wind[i] <- weather_df$wind_speed_10m[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f m/s",
        i,
        au_sample_no_fires$wind[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


#######################
##### Humidity   ######
#######################

au_sample_fires$humidity <- NA

for(i in 1:N){
  if (!is.na(au_sample_fires$humidity[i])) {
    next
  }
  
  lat <- au_sample_fires$latitude[i]
  lon <- au_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
      au_sample_fires$datetime[i],
      units = "hours"
    )))
    
    au_sample_fires$humidity[i] <- weather_df$rh_2m[idx]
    
    print(sprintf(
      "Processed request for point %d: %.1f%%",
      i,
      au_sample_fires$humidity[i]
    ))
  }
} else {
  print(sprintf("Failed request for point %d", i))
}
}

au_sample_no_fires$humidity <- NA

for(i in 1:N){
  if (!is.na(au_sample_no_fires$humidity[i])) {
    next
  }
  
  lat <- au_sample_no_fires$latitude[i]
  lon <- au_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
        au_sample_no_fires$datetime[i],
        units = "hours"
      )))
      
      au_sample_no_fires$humidity[i] <- weather_df$rh_2m[idx]
      
      print(sprintf(
        "Processed request for point %d: %.1f%%",
        i,
        au_sample_no_fires$humidity[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


################################
##### Cloud Cover         ######
################################


au_sample_fires$cloud_cover <- NA

for(i in 1:N){
  if (!is.na(au_sample_fires$cloud_cover[i])) {
    next
  }
  
  lat <- au_sample_fires$latitude[i]
  lon <- au_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=cloud_cover",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime     = ymd_hm(data$hourly$time, tz = "UTC"),
        cloud_cover  = data$hourly$cloud_cover
      )
      
      # Match cloud cover by nearest hour
      idx <- which.min(
        abs(difftime(weather_df$datetime,
                     au_sample_fires$datetime[i],
                     units = "hours"))
      )
      
      au_sample_fires$cloud_cover[i] <- weather_df$cloud_cover[idx]
      
      print(sprintf(
        "Processed request for point %d: %.1f%% cloud cover",
        i,
        au_sample_fires$cloud_cover[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}


au_sample_no_fires$cloud_cover <- NA


for(i in 1:N){
  if (!is.na(au_sample_no_fires$cloud_cover[i])) {
    next
  }
  
  lat <- au_sample_no_fires$latitude[i]
  lon <- au_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
    "%Y-%m-%d"
  )
  
  url <- sprintf(
    paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=%s&longitude=%s",
      "&start_date=%s&end_date=%s",
      "&hourly=cloud_cover",
      "&timezone=UTC"
    ),
    lat, lon, date_str, date_str
  )
  
  resp <- GET(url)
  
  if(status_code(resp) == 200){
    data <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    
    if(length(data$hourly$time) > 0){
      weather_df <- data.frame(
        datetime     = ymd_hm(data$hourly$time, tz = "UTC"),
        cloud_cover  = data$hourly$cloud_cover
      )
      
      # Match cloud cover by nearest hour
      idx <- which.min(
        abs(difftime(weather_df$datetime,
                     au_sample_no_fires$datetime[i],
                     units = "hours"))
      )
      
      au_sample_no_fires$cloud_cover[i] <- weather_df$cloud_cover[idx]
      
      print(sprintf(
        "Processed request for point %d: %.1f%% cloud cover",
        i,
        au_sample_no_fires$cloud_cover[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}




##############################
##### Precipitation     ######
##############################

au_sample_fires$precipitation <- NA

for(i in 1:N){
  if (!is.na(au_sample_fires$precipitation[i])) {
    next
  }
  
  lat <- au_sample_fires$latitude[i]
  lon <- au_sample_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
                     au_sample_fires$datetime[i],
                     units = "hours"))
      )
      
      au_sample_fires$precipitation[i] <- weather_df$precip_mm[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f mm",
        i,
        au_sample_fires$precipitation[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}

au_sample_no_fires$precipitation <- NA

for(i in 1:N){
  if (!is.na(au_sample_no_fires$precipitation[i])) {
    next
  }
  
  lat <- au_sample_no_fires$latitude[i]
  lon <- au_sample_no_fires$longitude[i]
  
  date_str <- format(
    as.POSIXct(au_sample_no_fires$datetime[i], "%Y-%m-%d %H:%M:%S"),
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
                     au_sample_no_fires$datetime[i],
                     units = "hours"))
      )
      
      au_sample_no_fires$precipitation[i] <- weather_df$precip_mm[idx]
      
      print(sprintf(
        "Processed request for point %d: %.2f mm",
        i,
        au_sample_no_fires$precipitation[i]
      ))
    }
  } else {
    print(sprintf("Failed request for point %d", i))
  }
}

##################
### Elevation  ###
##################

elevation_img <- ee$Image("USGS/SRTMGL1_003")$
  select("elevation")


get_elevation_for_point <- function(lat, lon) {
  
  point <- ee$Geometry$Point(c(lon, lat))
  
  elev_value <- tryCatch({
    elevation_img$reduceRegion(
      reducer  = ee$Reducer$mean(),
      geometry = point,
      scale    = 30,
      bestEffort = TRUE
    )$get("elevation")$getInfo()
  }, error = function(e) NA)
  
  if (is.null(elev_value) || length(elev_value) == 0) {
    return(NA_real_)
  }
  
  return(elev_value)
}

au_sample_fires$elevation[
  is.na(au_sample_fires$elevation)
] <- 0

au_sample_fires$elevation <- NA_real_

n <- nrow(au_sample_fires)

for (i in seq_len(n)) {
  
  au_sample_fires$elevation[i] <- get_elevation_for_point(
    lat = au_sample_fires$latitude[i],
    lon = au_sample_fires$longitude[i]
  )
  
  if (i %% 10 == 0) {
    message("Elevation processed ", i, " / ", n)
  }
}

au_sample_no_fires$elevation <- NA_real_

n <- nrow(au_sample_no_fires)

for (i in seq_len(n)) {
  
  au_sample_no_fires$elevation[i] <- get_elevation_for_point(
    lat = au_sample_no_fires$latitude[i],
    lon = au_sample_no_fires$longitude[i]
  )
  
  if (i %% 10 == 0) {
    message("Elevation processed ", i, " / ", n)
  }
}

au_sample_no_fires$elevation[
  is.na(au_sample_no_fires$elevation)
] <- 0





##########################
##### NDVI     ##########
##########################
#ee_Initialize() 


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


au_sample_fires$ndvi <- NA_real_

n <- nrow(au_sample_fires)


for (i in seq_len(n)) {
#  if (!is.na(au_sample_fires$ndvi[i])) {
#    next
 # }
  
  fire_date <- as.Date(au_sample_fires$acq_date[i])
  
  start_date <- fire_date - 17
  end_date   <- fire_date - 1
  
  au_sample_fires$ndvi[i] <- get_ndvi_for_point(
    lat  = au_sample_fires$latitude[i],
    lon  = au_sample_fires$longitude[i],
    start_date = start_date,
    end_date   = end_date
  )
  
  if (i %% 10 == 0) {
    message("Processed ", i, " / ", n)
  }
}
# TODO: 0 is bad form, but, there is only one value missing
au_sample_fires$ndvi[is.na(au_sample_fires$ndvi)] <- -9999
# Rescale NDVI (very important)
au_sample_fires$ndvi <- au_sample_fires$ndvi / 10000


au_sample_no_fires$ndvi <- NA_real_

n <- nrow(au_sample_no_fires)


for (i in seq_len(n)) {
  if (!is.na(au_sample_no_fires$ndvi[i])) {
    next
  }
  
  datetime <- as.POSIXct(
    au_sample_no_fires$datetime[i],
    format = "%Y-%m-%d %H:%M:%OS",
    tz = "UTC"
  )
  
  acq_date <- as.Date(datetime)
  
  fire_date <- as.Date(acq_date)
  
  start_date <- fire_date - 17
  end_date   <- fire_date - 1
  
  au_sample_no_fires$ndvi[i] <- get_ndvi_for_point(
    lat  = au_sample_no_fires$latitude[i],
    lon  = au_sample_no_fires$longitude[i],
    start_date = start_date,
    end_date   = end_date
  )
  
  if (i %% 10 == 0) {
    message("Processed ", i, " / ", n)
  }
}
# TODO: 0 is bad form, but, there is only one value missing
au_sample_no_fires$ndvi[is.na(au_sample_no_fires$ndvi)] <- -9999
# Rescale NDVI (very important)
au_sample_no_fires$ndvi <- au_sample_no_fires$ndvi / 10000


#######################################
##### Soil Moisture Data     ##########
#######################################


smap_sm <- ee$ImageCollection("NASA/SMAP/SPL4SMGP/008")$
  select("sm_surface")


get_sm_for_point <- function(lat, lon, start_date, end_date) {
  
  point <- ee$Geometry$Point(c(lon, lat))
  
  img <- smap_sm$
    filterDate(as.character(start_date), as.character(end_date))$
    median()
  
  sm_value <- tryCatch({
    img$reduceRegion(
      reducer    = ee$Reducer$mean(),
      geometry   = point,
      scale      = 9000,      # native SMAP resolution (~9 km)
      bestEffort = TRUE
    )$get("sm_surface")$getInfo()
  }, error = function(e) NA)
  
  if (is.null(sm_value) || length(sm_value) == 0) {
    return(NA_real_)
  }
  
  return(sm_value)
}

au_sample_fires$soil_moisture <- NA_real_

n <- nrow(au_sample_fires)

for (i in seq_len(n)) {
  if (!is.na(au_sample_fires$soil_moisture[i])) {
    next
  }
  
  fire_date <- as.Date(au_sample_fires$acq_date[i])
  
  start_date <- fire_date - 17
  end_date   <- fire_date - 1
  
  au_sample_fires$soil_moisture[i] <- get_sm_for_point(
    lat  = au_sample_fires$latitude[i],
    lon  = au_sample_fires$longitude[i],
    start_date = start_date,
    end_date   = end_date
  )
  
  if (i %% 10 == 0) {
    message("Processed ", i, " / ", n)
  }
}
# TODO: Missing values
au_sample_fires$soil_moisture[
  is.na(au_sample_fires$soil_moisture)
] <- 0


au_sample_no_fires$soil_moisture <- NA_real_
n <- nrow(au_sample_no_fires)

for (i in seq_len(n)) {
  if (!is.na(au_sample_no_fires$soil_moisture[i])) {
    next
  }
  
  datetime <- as.POSIXct(
    au_sample_no_fires$datetime[i],
    format = "%Y-%m-%d %H:%M:%OS",
    tz = "UTC"
  )
  
  acq_date <- as.Date(datetime)
  
  fire_date <- as.Date(acq_date)
  
  start_date <- fire_date - 17
  end_date   <- fire_date - 1
  
  au_sample_no_fires$soil_moisture[i] <- get_sm_for_point(
    lat  = au_sample_no_fires$latitude[i],
    lon  = au_sample_no_fires$longitude[i],
    start_date = start_date,
    end_date   = end_date
  )
  
  if (i %% 10 == 0) {
    message("Processed ", i, " / ", n)
  }
}
# TODO: Missing values
au_sample_no_fires$soil_moisture[
  is.na(au_sample_no_fires$soil_moisture)
] <- 0


#############################
##### Write to CSV     ######
#############################


write.csv(
  au_sample_fires,
  "MSISS/wildfires/main/data/1000_samples/au_1000_sample_fires.csv",
  row.names = FALSE
)

write.csv(
  au_sample_no_fires,
  "MSISS/wildfires/main/data/1000_samples/au_1000_sample_no_fires.csv",
  row.names = FALSE
)


