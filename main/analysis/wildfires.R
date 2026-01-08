#######################################################
##### Steven Leone - Comparison of Wildfires.  ########
#######################################################
# https://firms.modaps.eosdis.nasa.gov/content/descriptions/FIRMS_MODIS_Firehotspots.html

# Bayesian Analysis for Wildfire Inference in the US and Australia through Spatial Hierarchical Models



library(rstanarm)
# 2019
# au_data <- read.csv("MSISS/wildfires/main/data/au_wildfires_2019.csv")
# us_data <- read.csv("MSISS/wildfires/main/data/us_wildfires_2019.csv")

# 2020
au_data <- read.csv("MSISS/wildfires/main/data/au_wildfires_2020.csv")
us_data <- read.csv("MSISS/wildfires/main/data/us_wildfires_2020.csv")


min(au_data$acq_date)
# 2019-08-01
max(au_data$acq_date)
# 2019-09-30

min(us_data$acq_date)
# 2019-08-01
max(us_data$acq_date)
# 2019-09-30


#######################################################
##### Night Time - One Parameter Models   #############
#######################################################

au_data$nighttime <- ifelse(au_data$daynight == "N", 1, 0)
us_data$nighttime <- ifelse(us_data$daynight == "N", 1, 0)

au_successes <- sum(au_data$nighttime == 1)
#35422 wildfires in the night
au_failures <- sum(au_data$nighttime == 0)
#120995 wildfires in the day

us_successes <- sum(us_data$nighttime == 1)
#43369 wildfires in the night
us_failures <- sum(us_data$nighttime == 0)
#110547 wildfires in the day

#####                                 ##### 
##### AU Prior, Likelihood, Posterior ##### 
#####                                 ##### 

alpha <- 3
beta <- 12

alpha_posterior = alpha + au_successes
beta_posterior = beta + au_failures

# Simple Posterior Plot
au_samples <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(au_samples), main = "AU Nighttime Wildfires")


# Prior, Posterior, Likelihood
p_values <- seq(0, 1, length = 100)

prior_density <- dbeta(p_values, alpha, beta)
posterior_density <- dbeta(p_values, alpha_posterior, beta_posterior)

plot(p_values, posterior_density, xlab="X",
     ylab = "Beta Density", type = "l",
     col = "Red")

lines(p_values, prior_density, type = "l", col = "blue")

likelihood_values <- dbinom(au_successes, size = au_successes + au_failures, prob = p_values)
likelihood_density <- likelihood_values / max(likelihood_values)
lines(p_values, likelihood_density, col = "green", lwd = 2)


#####                                 ##### 
##### US Prior, Likelihood, Posterior ##### 
#####                                 #####

alpha <- 4
beta <- 11

alpha_posterior = alpha + us_successes
beta_posterior = beta + us_failures

# Simple Posterior Plot
us_samples <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(us_samples), main = "US Nighttime Wildfires")

# Prior, Posterior, Likelihood
p_values <- seq(0, 1, length = 100)

prior_density <- dbeta(p_values, alpha, beta)
posterior_density <- dbeta(p_values, alpha_posterior, beta_posterior)

plot(p_values, posterior_density, xlab="X",
     ylab = "Beta Density", type = "l",
     col = "Red")

lines(p_values, prior_density, type = "l", col = "blue")

likelihood_values <- dbinom(us_successes, size = us_successes + us_failures, prob = p_values)
likelihood_density <- likelihood_values / max(likelihood_values)
lines(p_values, likelihood_density, col = "green", lwd = 2)


##############################################################
##### Night Time - Spatial Hierarchical Models   #############
##############################################################


#####                                 ##### 
##### AU Spatial Hierarchical Model   ##### 
#####                                 #####

library(brms)

set.seed(321)

data_clean <- na.omit(au_data[, c("nighttime", "brightness",
                                  "latitude", "longitude")])

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

fit <- brm(
  nighttime ~ brightness +
    s(latitude, longitude, k = 10),
  data = data_clean,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4
)


summary(fit)
au_fit <- fit
theta <- posterior_linpred(au_fit, transform = TRUE)

theta_mean <- colMeans(theta)
y_pred <- ifelse(theta_mean > 0.5, 1, 0)
y_true <- data_clean$nighttime
accuracy <- mean(y_pred == y_true)
accuracy #  0.8393269

y_true <- data_clean$nighttime

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

# Accuracy       TPR       FPR       TNR       FNR 
# 0.8393269 0.4915871 0.0588702 0.9411298 0.5084129 


# Family: bernoulli 
# Links: mu = logit 
# Formula: nighttime ~ brightness + s(latitude, longitude, k = 10) 
# Data: data_clean (Number of observations: 156417) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000

# Smoothing Spline Hyperparameters:
#  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(slatitudelongitude_1)     5.00      1.33     3.15     8.32 1.01      679     1158

# Regression Coefficients:
#                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               25.00      0.19    24.62    25.37 1.00     3139     2925
# brightness              -0.08      0.00    -0.08    -0.08 1.00     3069     2975
# slatitudelongitude_1    16.12      0.25    15.64    16.61 1.00     2428     2619
# slatitudelongitude_2    -7.56      0.16    -7.87    -7.26 1.00     2286     2309


#####                                 ##### 
##### US Spatial Hierarchical Model   ##### 
#####                                 #####

library(brms)

set.seed(321)

data_clean <- na.omit(us_data[, c("nighttime", "brightness",
                               "latitude", "longitude")])

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

knots <- data.frame(
  lat_sc = as.numeric(knots$lat_sc),
  lon_sc = as.numeric(knots$lon_sc)
)


fit <- brm(
  nighttime ~ brightness +
    s(latitude, longitude, k = 10),
  data = data_clean,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4
)


summary(fit)
# Family: bernoulli 
# Links: mu = logit 
# Formula: nighttime ~ brightness + s(latitude, longitude, k = 10) 
# Data: data_clean (Number of observations: 153916) 
# Draws: 1 chains, each with iter = 50; warmup = 25; thin = 1;
# total post-warmup draws = 25

# Smoothing Spline Hyperparameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(slatitudelongitude_1)     3.92      0.23     3.70     4.36 2.13        2       10

# Regression Coefficients:
#  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                1.90      0.22     1.70     2.31 1.18        4       16
# brightness              -0.01      0.00    -0.01    -0.00 2.02        2       11
# slatitudelongitude_1     0.48      0.02     0.45     0.50 1.97        2       10
# slatitudelongitude_2    -1.39      0.02    -1.41    -1.36 2.13        2       10






