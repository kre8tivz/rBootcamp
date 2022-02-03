##############################
# Reading the files
##############################

citibike_jan19 <- read.csv(file = file.path('C:\\Users\\Yanik\\switchdrive\\SharedOstSchweiz',
                                            '\\R Bootcamp\\Data\\201901-citibike-tripdata.csv'))

citibike_apr19 <- read.csv(file = file.path('C:\\Users\\Yanik\\switchdrive\\SharedOstSchweiz',
                                            '\\R Bootcamp\\Data\\201904-citibike-tripdata.csv'))

citibike_jul19 <- read.csv(file = file.path('C:\\Users\\Yanik\\switchdrive\\SharedOstSchweiz',
                                            '\\R Bootcamp\\Data\\201907-citibike-tripdata.csv'))

citibike_oct19 <- read.csv(file = file.path('C:\\Users\\Yanik\\switchdrive\\SharedOstSchweiz',
                                            '\\R Bootcamp\\Data\\201910-citibike-tripdata.csv'))

nyc_weather_2019 <- read.csv(file = file.path('C:\\Users\\Yanik\\switchdrive\\SharedOstSchweiz',
                                              '\\R Bootcamp\\Data\\New York Weather 2019.csv'))

##############################
# Sampleing the Dataframes
##############################

library(dplyr)
library(stringr)
library(tidyr)

citibike_jan19_sample <- sample_n(citibike_jan19, size=1000)
citibike_apr19_sample <- sample_n(citibike_apr19, size=1000)
citibike_jul19_sample <- sample_n(citibike_jul19, size=1000)
citibike_oct19_sample <- sample_n(citibike_oct19, size=1000)

##############################
# Testing
##############################

head(citibike_jan19_sample)
head(citibike_apr19_sample)
head(citibike_jul19_sample)
head(citibike_oct19_sample)

##############################
# Factor loading
##############################

citibike_jan19_sample <- cbind(citibike_jan19_sample, season='winter')
citibike_apr19_sample <- cbind(citibike_apr19_sample, season='spring')
citibike_jul19_sample <- cbind(citibike_jul19_sample, season='summer')
citibike_oct19_sample <- cbind(citibike_oct19_sample, season='autumn')


##############################
# Dataframe merging
##############################


citibike_merged <- rbind(citibike_jan19_sample, citibike_apr19_sample, 
                         citibike_jul19_sample, citibike_oct19_sample)

View(citibike_merged)
nrow(citibike_merged)

#############################
# Weather Data cleaning
#############################

nyc_weather_cleaned <- subset(nyc_weather_2019, select = -c(TSUN, TAVG, AWND))
nrow(nyc_weather_cleaned)

#############################
# Add Date column
#############################

citibike_merged$DATE <- substr(citibike_merged$starttime, 1, 10)


#############################
# Join nyc_weather_cleaned & citibike_merged
#############################


citibike_weather_combined <- left_join(citibike_merged, nyc_weather_cleaned, 
                                       by='DATE')

##############################
# Calculate average temperature
##############################


citibike_weather_combined$meantemp <- (citibike_weather_combined$TMAX + citibike_weather_combined$TMIN) / 2

##############################
# Grouping rain intensity
##############################

citibike_weather_combined$raingrouped <- ifelse(citibike_weather_combined$PRCP <=2.0, 'less',
                                         ifelse(citibike_weather_combined$PRCP >2.0 & citibike_weather_combined$PRCP <=10.0, 'moderate',
                                         ifelse(citibike_weather_combined$PRCP >10.0 & citibike_weather_combined$PRCP <=30.0, 'heavy', 'intens')))


##############################
# Write the csv
##############################

write.csv(citibike_weather_combined, sep = ',', file = "citibike_weather_2019.csv")
