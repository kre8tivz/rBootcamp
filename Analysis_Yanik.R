#####################################
# Clear Environment
#####################################
rm(list = ls())

#####################################
# Load Librarys
#####################################
require(tidyr)
require(ggplot2)
require(ggthemes)
require(dplyr)
require(scales)
require(knitr)
require(lubridate)
require(ggmap)
require(purrr)
library(tidyverse)
install.packages("purrr")

#####################################
# Read CSV
#####################################

citibike_weather_df <- read.csv('citibike_weather_2019.csv')

#####################################
# Weekday with most trips
#####################################


Sys.setlocale("LC_TIME", "English") #Weekdays in english

#getting the weekdays
citibike_weather_df$weekday <- weekdays(as.Date(citibike_weather_df$DATE))
citibike_weather_df$weekday <- factor(citibike_weather_df$weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# Histogram
ggplot(data.frame(citibike_weather_df$weekday), aes(x=citibike_weather_df$weekday, fill=factor(citibike_weather_df$weekday))) +
  geom_bar(alpha=.8) +
  theme(legend.position="none")

#####################################
# time period with most trips
#####################################

citibike_weather_df$onlytime <- as.POSIXct(substr(citibike_weather_df$starttime, 12, 19), format="%H:%M:%S")
citibike_weather_df$time <- strftime(citibike_weather_df$onlytime,format="%H:%M")
citibike_weather_df$hour <- as.numeric(substr(citibike_weather_df$time,1,2))

citibike_weather_df$daytime <- ifelse(citibike_weather_df$onlytime <= as.POSIXct("06:00:00",format="%H:%M:%S"), 'night',
                               ifelse(citibike_weather_df$onlytime > as.POSIXct("06:00:00",format="%H:%M:%S") & citibike_weather_df$onlytime <= as.POSIXct("12:00:00",format="%H:%M:%S"), 'morning',
                               ifelse(citibike_weather_df$onlytime > as.POSIXct("12:00:00",format="%H:%M:%S") & citibike_weather_df$onlytime <= as.POSIXct("18:00:00",format="%H:%M:%S"), 'afternoon', 
                               ifelse(citibike_weather_df$onlytime > as.POSIXct("18:00:00",format="%H:%M:%S") & citibike_weather_df$onlytime <= as.POSIXct("24:00:00",format="%H:%M:%S"), 'evening', 'night'))))

citibike_weather_df$daytime <- factor(citibike_weather_df$daytime,levels = c("morning","afternoon","evening","night"))

# Histogram
ggplot(data.frame(citibike_weather_df$daytime), aes(x=citibike_weather_df$daytime)) +
  geom_bar(fill='#2980B9') +
  theme(legend.position="none",axis.text.y=element_blank(),plot.title=element_text(hjust=.5)) +
  theme_fivethirtyeight()+
  xlab("Daytime") +
  ylab("Number of rides")

#Hourly Distribution of Rides

citibike_weather_df %>% ggplot(aes(x=hour,fill=factor(weekday))) + 
  geom_density(alpha=.2)+facet_wrap(~weekday,ncol=1)+theme_fivethirtyeight()+
  theme(legend.position="none",axis.text.y=element_blank(),plot.title=element_text(hjust=.5)) + 
  ggtitle(expression(atop("Hourly Distribution of Rides",atop("Weekday Peaks during Morning Rush (8am-9am) and Afternoon Rush (5pm-6pm)"))))


##########################################################################
# Influence of average temperature per day on the number of rides per day
##########################################################################

ggplot(aes(x=meantemp,y=tripduration),data=citibike_weather_df)+
  geom_point() +
  scale_y_continuous(labels = scales::comma)


View(citibike_weather_df)
max(citibike_weather_df$tripduration)

#########################################################################
# ANOVA with Tripduration per Day
#########################################################################

sum_monday    <- sum(citibike_weather_df[which(citibike_weather_df$weekday == "Monday"), "tripduration"])
sum_tuesday   <- sum(citibike_weather_df[which(citibike_weather_df$weekday == "Tuesday"), "tripduration"])
sum_wednesday <- sum(citibike_weather_df[which(citibike_weather_df$weekday == "Wednesday"), "tripduration"])
sum_thursday  <- sum(citibike_weather_df[which(citibike_weather_df$weekday == "Thursday"), "tripduration"])
sum_friday    <- sum(citibike_weather_df[which(citibike_weather_df$weekday == "Friday"), "tripduration"])
sum_saturday  <- sum(citibike_weather_df[which(citibike_weather_df$weekday == "Saturday"), "tripduration"])
sum_sunday    <- sum(citibike_weather_df[which(citibike_weather_df$weekday == "Sunday"), "tripduration"])

tapply(citibike_weather_df$tripduration, citibike_weather_df$weekday, mean)

model <- aov(tripduration ~ meantemp + PRCP, data = citibike_weather_df)
summary(model)
