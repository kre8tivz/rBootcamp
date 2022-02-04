#####################################
# Clear Environment
#####################################
rm(list = ls())

#####################################
# Load Librarys
#####################################
require(tidyr)
require(ggplot2)

#####################################
# Read CSV
#####################################

citibike_weather_df <- read.csv('citibike_weather_2019.csv')

#####################################
# Weekday with most trips
#####################################

#getting the weekdays

citibike_weather_df$weekday <- weekdays(as.Date(citibike_weather_df$DATE))

# Histogram
ggplot(data.frame(citibike_weather_df$weekday), aes(x=citibike_weather_df$weekday)) +
  geom_bar()

#####################################
# time period with most trips
#####################################


