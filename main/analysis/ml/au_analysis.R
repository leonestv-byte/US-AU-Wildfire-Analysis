install.packages("brms")
library(rstanarm)
library(brms)

au_data_fires <- read.csv("MSISS/wildfires/main/data/1000_samples/au_1000_sample_fires.csv")
au_data_no_fires <- read.csv("MSISS/wildfires/main/data/1000_samples/au_1000_sample_no_fires.csv")

library(dplyr)

au_data_fires$population - ifelse(is.na(au_data_fires$population), 0, au_data_fires$population)
au_data_no_fires$population - ifelse(is.na(au_data_fires$population), 0, au_data_fires$population)

au_data_fires$state <- factor(au_data_fires$state)
au_data_no_fires$state <- factor(au_data_no_fires$state)

au_data_fires$city <- factor(au_data_fires$city)
au_data_no_fires$state <- factor(au_data_no_fires$city)


au_data <- bind_rows(au_data_fires, au_data_no_fires) %>%
  select(
    wildfire,
    temp_C,
    latitude,
    longitude,
    computed_daynight_sza,
    daynight,
    humidity,
    wind,
    elevation,
    soil_moisture,
    ndvi,
    cloud_cover,
    precipitation,
    population,
    state,
    city
  )

# Fix different values in computed_daynight_sza and daynight bug:
au_data <- au_data %>% mutate(daynight_combined = coalesce(daynight, computed_daynight_sza))
au_data$nighttime <- ifelse(au_data$daynight_combined == "N", 1, 0)


data_clean <- na.omit(au_data[, c("wildfire",
                                  "latitude", "longitude", "temp_C", "wind", "humidity", "nighttime", "elevation", "soil_moisture", "ndvi", "cloud_cover", "precipitation", "population", "city", "state")])

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
  wildfire ~ temp_C  + wind + humidity + ndvi + precipitation + cloud_cover + soil_moisture + elevation + nighttime  + 
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
accuracy # 0.869 # 0.8685 with nighttime

theta_mean <- colMeans(theta)
y_pred <- ifelse(theta_mean > 0.5, 1, 0) #0.35 - best TPR, TNR split
y_true <- data_clean$wildfire
accuracy <- mean(y_pred == y_true)
accuracy #  0.866



TP <- sum(y_pred == 1 & y_true == 1)
FP <- sum(y_pred == 1 & y_true == 0)
FN <- sum(y_pred == 0 & y_true == 1)
TN <- sum(y_pred == 0 & y_true == 0)

c(TP = TP, FP = FP, FN = FN, TN = TN)
TPR <- TP / (TP + FN)
FPR <- FP / (FP + TN)
TNR <- TN / (TN + FP)
FNR <- FN / (FN + TP)

rates <- c(
  Accuracy = accuracy,
  TPR = TPR,   # sensitivity
  FPR = FPR,
  TNR = TNR,   # specificity
  FNR = FNR
)

rates

# Accuracy      TPR      FPR      TNR      FNR 
# 0.866    0.877    0.145    0.855    0.123 



y_rep <- posterior_predict(fit)
pp_check(fit)
dev.off()

plot(fit)
plot(density(theta), main = "Posterior Distribution for Spatial Hierarchical Model")

#
# P(Wildfire | theta)
#
library(dplyr)
library(ggplot2)

# Combine posterior mean for each observation with temp_C
theta_mean <- apply(theta, 2, mean) # mean probability per observation
posterior_df <- data_clean %>%
  mutate(p_wildfire = theta_mean)

# Plot scatter + smooth
ggplot(posterior_df, aes(x = temp_C, y = p_wildfire)) +
  geom_jitter(height = 0, width = 0.2, alpha = 0.3) + # show raw points
  geom_smooth(method = "loess", color = "blue") + # smoothed posterior mean
  labs(x = "Temperature (°C)",
       y = "Posterior P(Wildfire | temp_C)",
       title = "Posterior Probability of Wildfire vs Temperature")


################################################
### Other Models - State, City Hierarchy 1) ####
################################################

n <- nrow(data_clean)
train_idx <- sample(seq_len(n), size = 0.6 * n)
train_data <- data_clean[train_idx, ]
test_data  <- data_clean[-train_idx, ]

fit <- brm(
  wildfire ~ temp_C  + wind + humidity + ndvi + precipitation + cloud_cover + soil_moisture + elevation + nighttime  + (1 | state) + (1 | state:city),
  data = train_data,
  family = bernoulli(),
  chains = 4,
  iter = 2000, #10,000
  cores = 4
)


########################
# Test Data Result     #
########################
p_test <- posterior_epred(fit, newdata = test_data, allow_new_levels = TRUE)
p_test_mean <- colMeans(p_test)
y_pred <- ifelse(p_test_mean > 0.5, 1, 0)
y_true <- test_data$wildfire

accuracy <- mean(y_pred == y_true)
accuracy # 0.8575

TP <- sum(y_pred == 1 & y_true == 1)
FP <- sum(y_pred == 1 & y_true == 0)
FN <- sum(y_pred == 0 & y_true == 1)
TN <- sum(y_pred == 0 & y_true == 0)

c(TP = TP, FP = FP, FN = FN, TN = TN)
TPR <- TP / (TP + FN)
FPR <- FP / (FP + TN)
TNR <- TN / (TN + FP)
FNR <- FN / (FN + TP)

rates <- c(
  Accuracy = accuracy,
  TPR = TPR,   # sensitivity
  FPR = FPR,
  TNR = TNR,   # specificity
  FNR = FNR
)

rates


################################################
### Other Models - State, City Hierarchy 2) ####
################################################

n <- nrow(data_clean)
train_idx <- sample(seq_len(n), size = 0.6 * n)
train_data <- data_clean[train_idx, ]
test_data  <- data_clean[-train_idx, ]


fit <- brm(
  wildfire ~ temp_C  + wind + humidity + ndvi + precipitation + cloud_cover + soil_moisture + elevation + nighttime  + (1 | state) + (1 | city),
  data = train_data,
  family = bernoulli(),
  chains = 4,
  iter = 2000, #10,000
  cores = 4
)

p_test <- posterior_epred(fit, newdata = test_data, allow_new_levels = TRUE)
p_test_mean <- colMeans(p_test)
y_pred <- ifelse(p_test_mean > 0.5, 1, 0)
y_true <- test_data$wildfire

accuracy <- mean(y_pred == y_true)
accuracy # 0.8575

TP <- sum(y_pred == 1 & y_true == 1)
FP <- sum(y_pred == 1 & y_true == 0)
FN <- sum(y_pred == 0 & y_true == 1)
TN <- sum(y_pred == 0 & y_true == 0)

c(TP = TP, FP = FP, FN = FN, TN = TN)
TPR <- TP / (TP + FN)
FPR <- FP / (FP + TN)
TNR <- TN / (TN + FP)
FNR <- FN / (FN + TP)

rates <- c(
  Accuracy = accuracy,
  TPR = TPR,   # sensitivity
  FPR = FPR,
  TNR = TNR,   # specificity
  FNR = FNR
)

rates

################################################
### Other Models - 3)                       ####
################################################


# We can see intercepts for each predictor per individual city or state, and how much they impact it. 

n <- nrow(data_clean)
train_idx <- sample(seq_len(n), size = 0.6 * n)
train_data <- data_clean[train_idx, ]
test_data  <- data_clean[-train_idx, ]


fit <- brm(
  wildfire ~ temp_C + ndvi + wind + humidity  + precipitation + cloud_cover + elevation + nighttime  + (1  | state) + (1 + soil_moisture  | city),
  data = train_data,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4
)

p_test <- posterior_epred(fit, newdata = test_data, allow_new_levels = TRUE)
p_test_mean <- colMeans(p_test)
y_pred <- ifelse(p_test_mean > 0.5, 1, 0)
y_true <- test_data$wildfire

accuracy <- mean(y_pred == y_true)
accuracy # 0.8575

TP <- sum(y_pred == 1 & y_true == 1)
FP <- sum(y_pred == 1 & y_true == 0)
FN <- sum(y_pred == 0 & y_true == 1)
TN <- sum(y_pred == 0 & y_true == 0)

c(TP = TP, FP = FP, FN = FN, TN = TN)
TPR <- TP / (TP + FN)
FPR <- FP / (FP + TN)
TNR <- TN / (TN + FP)
FNR <- FN / (FN + TP)

rates <- c(
  Accuracy = accuracy,
  TPR = TPR,   # sensitivity
  FPR = FPR,
  TNR = TNR,   # specificity
  FNR = FNR
)

rates




