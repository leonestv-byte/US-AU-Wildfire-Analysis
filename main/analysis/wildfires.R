#######################################################
##### Steven Leone - Comparison of Wildfires.  ########
#######################################################



library(rstanarm)
au_data <- read.csv("MSISS/wildfires/main/data/au_wildfires.csv")


min(au_data$acq_date)
# 2019-08-01
max(au_data$acq_date)
# 2019-09-30
