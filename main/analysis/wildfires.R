#######################################################
##### Steven Leone - Comparison of Wildfires.  ########
#######################################################

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
  chains = 1,
  iter = 50,
  cores = 1
)


summary(fit)
# Family: bernoulli 
# Links: mu = logit 
# Formula: nighttime ~ brightness + s(latitude, longitude, k = 10) 
# Data: data_clean (Number of observations: 156417) 
# Draws: 1 chains, each with iter = 50; warmup = 25; thin = 1;
# total post-warmup draws = 25

# Smoothing Spline Hyperparameters:
#  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(slatitudelongitude_1)     0.98      0.09     0.82     1.06 2.12        2       10

# Regression Coefficients:
#  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               14.61      2.09    12.67    18.39 1.93        2       10
# brightness              -0.05      0.01    -0.06    -0.04 1.97        2       10
# slatitudelongitude_1    -0.15      0.04    -0.18    -0.06 1.85        2       10
# slatitudelongitude_2    -1.88      0.00    -1.88    -1.88 1.01        9       13


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
  chains = 1,
  iter = 50,
  cores = 1
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






