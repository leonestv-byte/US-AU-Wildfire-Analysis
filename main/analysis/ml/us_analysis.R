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
    computed_daynight_sza,
    humidity,
    wind
  )

#TODO: fix
#us_data$nighttime <- ifelse(us_data$computed_daynight_sza == "N", 1, 0)

data_clean <- na.omit(us_data[, c("wildfire",
                                  "latitude", "longitude", "temp_C", "wind", "humidity")])

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
  wildfire ~ temp_C  + wind + humidity + 
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
accuracy # 0.874



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


plot(density(theta), main = "Posterior Distribution for Spatial Hierarchical Model")

# Accuracy      TPR      FPR      TNR      FNR 
# 0.828    0.844    0.188    0.812    0.156

# Accuracy      TPR      FPR      TNR      FNR 
# 0.874    0.891    0.143    0.857    0.109

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


