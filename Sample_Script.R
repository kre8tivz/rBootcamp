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
# Join nyc_weather_cleaned & citibike_merged
#############################





##############################
# Write the csv
##############################

write.csv(citibike_merged, sep = ',', file = "citibike_merged_2019.csv")
