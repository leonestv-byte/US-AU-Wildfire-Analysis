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

i <- 50
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
  au_sample_fires,
  "MSISS/wildfires/main/data/1000_samples/au_1000_sample_fires.csv",
  row.names = FALSE
)

write.csv(
  au_sample_no_fires,
  "MSISS/wildfires/main/data/1000_samples/au_1000_sample_no_fires.csv",
  row.names = FALSE
)


