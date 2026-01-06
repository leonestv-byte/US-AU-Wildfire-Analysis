#######################################################
##### Steven Leone - Comparison of Wildfires.  ########
#######################################################

# Bayesian Analysis for Wildfire Inference in the US and Australia through Spatial Hierarchical Models



library(rstanarm)
au_data <- read.csv("MSISS/wildfires/main/data/au_wildfires.csv")
us_data <- read.csv("MSISS/wildfires/main/data/us_wildfires.csv")


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
#7808 wildfires in the night
au_failures <- sum(au_data$nighttime == 0)
#28203 wildfires in the day

us_successes <- sum(us_data$nighttime == 1)
#3838 wildfires in the night
us_failures <- sum(us_data$nighttime == 0)
#13694 wildfires in the day

#####                                 ##### 
##### AU Prior, Likelihood, Posterior ##### 
#####                                 ##### 

alpha <- 7
beta <- 30

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

alpha <- 3
beta <- 13

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
# Data: data_clean (Number of observations: 36011) 
# Draws: 1 chains, each with iter = 50; warmup = 25; thin = 1;
# total post-warmup draws = 25

# Smoothing Spline Hyperparameters:
#  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(slatitudelongitude_1)     1.30      1.10     0.21     3.68 1.63        2       10

# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               33.33      2.30    28.97    36.89 1.67        2       10
# brightness              -0.11      0.01    -0.12    -0.09 1.67        2       10
# slatitudelongitude_1    -2.99      4.45   -11.40     0.67 1.36        3       10
# slatitudelongitude_2    -1.88      0.62    -2.94    -1.13 1.81        2       13


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
# 1 chain, 50 iterations, 1 core:
# Family: bernoulli 
# Links: mu = logit 
# Formula: nighttime ~ brightness + s(latitude, longitude, k = 10) 
# Data: data_clean (Number of observations: 17532) 
# Draws: 1 chains, each with iter = 50; warmup = 25; thin = 1;
# total post-warmup draws = 25

# Smoothing Spline Hyperparameters:
#  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(slatitudelongitude_1)     0.40      0.21     0.15     0.83 1.13       10       10

# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               16.93      2.98    11.21    20.22 1.97        2       10
# brightness              -0.06      0.01    -0.07    -0.04 1.97        2       10
# slatitudelongitude_1     0.87      1.69    -1.06     3.72 1.47        3       10
# slatitudelongitude_2    -1.57      0.76    -2.65    -0.67 2.02        2       11

# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
