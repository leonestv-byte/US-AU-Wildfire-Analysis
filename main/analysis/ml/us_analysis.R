install.packages("brms")
library(rstanarm)
library(brms)

us_data_fires <- read.csv("MSISS/wildfires/main/data/1000_samples/us_1000_sample_fires.csv")
us_data_no_fires <- read.csv("MSISS/wildfires/main/data/1000_samples/us_1000_sample_no_fires.csv")


library(dplyr)

us_data <- bind_rows(us_data_fires, us_data_no_fires) %>%
  select(
    wildfire,
    temp_C,
    latitude,
    longitude,
    computed_daynight_sza
  )

#TODO: fix
#us_data$nighttime <- ifelse(us_data$computed_daynight_sza == "N", 1, 0)

data_clean <- na.omit(us_data[, c("wildfire",
                                  "latitude", "longitude", "temp_C")])

# 1. Scaled/centered columns
lat_center <- mean(data_clean$latitude)
lat_scale  <- sd(data_clean$latitude)
lon_center <- mean(data_clean$longitude)
lon_scale  <- sd(data_clean$longitude)

# 2. Turn columns into Vectors
data_clean$lat_sc <- as.vector((data_clean$latitude - lat_center) / lat_scale)
data_clean$lon_sc <- as.vector((data_clean$longitude - lon_center) / lon_scale)

# 3. Use geolocation to create knots
set.seed(321)
knots <- data_clean[sample(nrow(data_clean), 300), c("lat_sc", "lon_sc")]

n <- nrow(data_clean)
train_idx <- sample(seq_len(n), size = 0.8 * n)
train_data <- data_clean[train_idx, ]
test_data  <- data_clean[-train_idx, ]


fit <- brm(
  wildfire ~ temp_C  +
  s(latitude, longitude, k = 10),
  data = data_clean,
  family = bernoulli(),
  chains = 4,
  iter = 2000, #10,000
  cores = 4
)

summary(fit)
theta <- posterior_linpred(fit, transform = TRUE)


theta_mean <- colMeans(theta)
y_pred <- ifelse(theta_mean > 0.5, 1, 0)
y_true <- data_clean$wildfire
accuracy <- mean(y_pred == y_true)
accuracy # 0.828


