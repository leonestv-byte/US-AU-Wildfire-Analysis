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


# AU Prior, Likelihood, Posterior

alpha <- 7
beta <- 30

alpha_posterior = alpha + au_successes
beta_posterior = beta + au_failures

au_samples <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(au_samples), main = "AU Nighttime Wildfires")


# US Prior, Likelihood, Posterior

alpha <- 3
beta <- 13

alpha_posterior = alpha + us_successes
beta_posterior = beta + us_failures

us_samples <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(us_samples), main = "US Nighttime Wildfires")


