## ----setup, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE)


## ---- echo=TRUE, warning=FALSE, message=FALSE-------

require(knitr)        # Engine for dynamic report generation
require(dplyr)        # For data manipulation
require(tidyr)        # Provides various important functions that can be used for data cleaning
require(broom)        # For summarizing statistical model objects in tidy tibbles
require(lubridate)    # For working with date-times and time-spans
require(skimr)        # For summary statistic
require(ggplot2)      # tidyverse data visualization package
require(ggeasy)       # Series of aliases to commonly used but diff. to remember ggplot2 seq.
require(ggthemes)     # Extra themes, geoms, and scales for 'ggplot2'
require(scales)       # Internal scaling infrastructure to ggplot2 and its functions
require(maps)         # For computing the areas of regions in a projected map
require(ggmap)        # To visualize spatial data & models on top of static maps
require(RColorBrewer) # Color palette
library(gridExtra)    # Provides a number of user-level functions to work with "grid" graphics
library(data.table)   # Widely used for fast aggregation of large datasets


## ----citibike-import--------------------------------
citibike_jan19 <- read.csv(file = file.path('C:\\Users\\noelr\\switchdrive',
'\\SharedOstSchweiz\\R Bootcamp\\Data\\201901-citibike-tripdata.csv'))

citibike_apr19 <- read.csv(file = file.path('C:\\Users\\noelr\\switchdrive',
'\\SharedOstSchweiz\\R Bootcamp\\Data\\201904-citibike-tripdata.csv'))

citibike_jul19 <- read.csv(file = file.path('C:\\Users\\noelr\\switchdrive',
'\\SharedOstSchweiz\\R Bootcamp\\Data\\201907-citibike-tripdata.csv'))

citibike_oct19 <- read.csv(file = file.path('C:\\Users\\noelr\\switchdrive',
'\\SharedOstSchweiz\\R Bootcamp\\Data\\201910-citibike-tripdata.csv'))


## ----weather-import---------------------------------
nyc_weather_2019 <- read.csv(file = file.path('C:\\Users\\noelr\\switchdrive',
'\\SharedOstSchweiz\\R Bootcamp\\Data\\New York Weather 2019.csv'))


## ----samples----------------------------------------
citibike_jan19_sample <- sample_n(citibike_jan19, size=1000)
citibike_apr19_sample <- sample_n(citibike_apr19, size=1000)
citibike_jul19_sample <- sample_n(citibike_jul19, size=1000)
citibike_oct19_sample <- sample_n(citibike_oct19, size=1000)


## ----new-var-season---------------------------------
citibike_jan19_sample <- cbind(citibike_jan19_sample, season='winter')
citibike_apr19_sample <- cbind(citibike_apr19_sample, season='spring')
citibike_jul19_sample <- cbind(citibike_jul19_sample, season='summer')
citibike_oct19_sample <- cbind(citibike_oct19_sample, season='autumn')


## ---------------------------------------------------
citibike_merged_2019 <- rbind(citibike_jan19_sample, citibike_apr19_sample, 
                              citibike_jul19_sample, citibike_oct19_sample)


## ---------------------------------------------------
citibike_merged_2019$DATE <- substr(citibike_merged_2019$starttime, 1, 10)


## ---------------------------------------------------
nyc_weather_cleaned <- subset(nyc_weather_2019, select = -c(TSUN, TAVG, AWND, STATION, NAME))


## ---------------------------------------------------
citi_cb <- left_join(citibike_merged_2019, nyc_weather_cleaned, by='DATE')


## ---------------------------------------------------
citi_cb$meantemp <- (citi_cb$TMAX + citi_cb$TMIN) / 2


## ---------------------------------------------------
citi_cb$raingrouped <- ifelse(citi_cb$PRCP == 0, 'no rain',
                       ifelse(citi_cb$PRCP > 0   & citi_cb$PRCP <=2.0, 'weak',
                       ifelse(citi_cb$PRCP >2.0  & citi_cb$PRCP <=10.0, 'moderate',
                       ifelse(citi_cb$PRCP >10.0 & citi_cb$PRCP <=30.0, 'heavy', 'intense'))))

# To make sure that the categories are in the intended order
citi_cb$raingrouped <- factor(citi_cb$raingrouped, levels = c("no rain", "weak", "moderate", "heavy", "intense"))


## ---------------------------------------------------
citi_cb$Snow_yesno <- ifelse(citi_cb$SNOW > 0, 'yes', 'no')


## ---------------------------------------------------
# Weekdays in english
Sys.setlocale("LC_TIME", "English")

# Define Weekday
citi_cb$weekday <- weekdays(as.Date(citi_cb$DATE))

# Setting correct factor order
citi_cb$weekday <- factor(citi_cb$weekday,levels = c("Monday","Tuesday","Wednesday",
                                                     "Thursday","Friday","Saturday","Sunday"))


## ---------------------------------------------------
citi_cb$onlytime <- as.POSIXct(substr(citi_cb$starttime, 12, 19), format="%H:%M:%S")

# Only Hours and Minutes
citi_cb$time     <- strftime(citi_cb$onlytime,format="%H:%M")

# Only Hour
citi_cb$hour     <- as.numeric(substr(citi_cb$time,1,2))

# Set Daytime
citi_cb$daytime  <- ifelse(citi_cb$onlytime <= as.POSIXct("06:00:00",format="%H:%M:%S"), 'night',
                    ifelse(citi_cb$onlytime > as.POSIXct("06:00:00",format="%H:%M:%S") & 
                    citi_cb$onlytime <= as.POSIXct("12:00:00",format="%H:%M:%S"), 'morning',
                    ifelse(citi_cb$onlytime > as.POSIXct("12:00:00",format="%H:%M:%S") & 
                    citi_cb$onlytime <= as.POSIXct("18:00:00",format="%H:%M:%S"), 'afternoon', 
                    ifelse(citi_cb$onlytime > as.POSIXct("18:00:00",format="%H:%M:%S") & 
                    citi_cb$onlytime <= as.POSIXct("24:00:00",format="%H:%M:%S"), 'evening', 'night'))))

# Setting correct factor order
citi_cb$daytime <- factor(citi_cb$daytime,levels = c("morning","afternoon","evening","night"))


## ---------------------------------------------------
citi_cb$tripdur_cat <- ifelse(citi_cb$tripduration <=300, '0 to 5 min',
                       ifelse(citi_cb$tripduration >300 & 
                              citi_cb$tripduration <=900, '5 to 15 min',
                       ifelse(citi_cb$tripduration >900 & 
                              citi_cb$tripduration <=1800, '15 to 30 min',
                       ifelse(citi_cb$tripduration >1800 & 
                              citi_cb$tripduration <=3600, '30 to 60 min',
                       ifelse(citi_cb$tripduration >3600 & 
                              citi_cb$tripduration <=14400, '1 to 4 h',
                       ifelse(citi_cb$tripduration >14400 & 
                              citi_cb$tripduration <=43200, '4 to 12 h', 'over 12 h'))))))

# To make sure that the categories are in ascending order
citi_cb$tripdur_cat <- factor(citi_cb$tripdur_cat, levels = c("0 to 5 min", "5 to 15 min", "15 to 30 min", "30 to 60 min", "1 to 4 h", "4 to 12 h", "over 12 h"))


## ---- fig.cap = "Boxplot to examine how the variable is distributed and if there are outliers.", fig.height=2.5----
options(scipen = 999) # Deactivate scientific notation

ggplot(citi_cb,aes(y=tripduration)) +
  geom_boxplot() +
  scale_x_discrete() +
  theme_light() +
  theme(plot.margin=unit(c(0,0,-0.01,0), "null")) + # Space between figure and caption
  labs(title = "Boxplot of Trip Duration",
       y = "Trip Duration (in seconds)") +
  ggeasy::easy_center_title() +
  coord_flip()


## ---- fig.cap = "The histogram shows that the trip durations are mostly short-term.", fig.height=4----
tripdur_cat_count <- citi_cb %>%
  group_by(tripdur_cat) %>%
  summarise(counts = n())

# Histogram trip duration categories
ggplot(tripdur_cat_count, aes(x=tripdur_cat, y=counts)) +
  geom_bar(fill = "#2980B9",
           stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + # Label above bar
  theme_light() +
  ggtitle("Histogram of Trip Duration Categories") +
  ggeasy::easy_center_title() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.margin=unit(c(0,0,-0.04,0), "null")) + # Space between figure and caption
  xlab('') + # Remove text on x-axis
  ylab("Number of rides")


## ---------------------------------------------------
citi_cb <- subset(citi_cb, tripduration < 43200)


## ---------------------------------------------------
# Alternative to summary to get an overview of the data frame
str(citi_cb)


## ---------------------------------------------------
# Alternative to summary to get an overview of the data frame
skim(citi_cb)


## ---- fig.cap="Most bikes were rented in the afternoon, while comparatively few bikes were rented at night."----
## Histogramm Daytime
ggplot(data.frame(citi_cb$daytime), aes(x=citi_cb$daytime)) +
  geom_bar(fill='#2980B9') +
  theme(legend.position="none", axis.text.y=element_blank(), plot.title=element_text(hjust=.5)) +
  theme_light() +
  ggtitle("Histogram with Number of Rides by Time of Day") +
  ggeasy::easy_center_title() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Daytime") +
  ylab("Number of rides")


## ---- fig.cap="Density plot which shows the hourly distribution of bikes rented."----
citi_cb %>% ggplot(aes(x=hour,fill=factor(weekday))) +
  scale_fill_manual(values = c('#2980B9', '#2980B9', '#2980B9', '#2980B9', '#2980B9', '#2980B9', '#2980B9')) +
  geom_density(alpha=.2) +
  facet_wrap(~weekday,ncol=1) +
  theme(legend.position="none", axis.text.y=element_blank(),plot.title=element_text(hjust=.5)) + 
  ggtitle(expression(atop("Hourly Distribution of Rides")))


## ---------------------------------------------------
# Create a dataframe with a column which counts how often a start station was used
start.station.rank <- data.frame(citi_cb %>%
                                       group_by(start.station.name) %>%
                                       summarize(count.start=n()) %>%
                                       arrange(desc(count.start)))

# Create a dataframe with a column which counts how often an end station was used
end.station.rank <- data.frame(citi_cb %>%
                                     group_by(end.station.name) %>%
                                     summarize(count.end=n()) %>%
                                     arrange(desc(count.end)))

# Combine the start station and end station data frames, so that we have 
# for each station the number of bikes rented and the number of bikes returned
station_usage <- left_join(x=start.station.rank, y=end.station.rank, 
                           by=c("start.station.name"="end.station.name"))

# Check if there are start or end stations which were not used
na_check_start_bef <- sum(is.na(station_usage$count.start))
na_check_end_bef <- sum(is.na(station_usage$count.end))

print(paste0("There are ", na_check_start_bef, " start stations with NA values and ", na_check_end_bef, " end stations with NA values."))

# Replace all NAs with 0 (Stations which no one used should be shown as zero for the subsequent sum)
station_usage <- station_usage %>% 
  mutate_all(~replace(., is.na(.), 0))

# Check if the NA values were successfully replaced
na_check_start_aft <- sum(is.na(station_usage$count.start))
na_check_end_aft <- sum(is.na(station_usage$count.end))

if (na_check_start_aft != 0){
  print(paste0("After replacement, there are ", na_check_start_aft, " start stations with NA values and ", na_check_end_aft, " end stations with NA values.")) 
} else{
  print("Successfully replaced NA values with the value 0.")}

# Create a new column which shows the combined number of bikes rented and returned
station_usage$station.count.comb <- rowSums(station_usage[, c("count.start", "count.end")])

# Order the dataframe by the combined number (from largest to smallest) to get a ranking
station_usage <- station_usage[order(-station_usage$station.count.comb),]

# Rename columns for improved readability
setnames(station_usage, old = c('start.station.name','count.start', 'count.end', 'station.count.comb'), new = c('Station Name','Bikes rented', 'Bikes returned', 'Total'))

# Check the 10 most and least used stations
kable(head(station_usage, 10), caption="Most used stations")
kable(tail(station_usage, 10), caption="Least used stations")


## ---- warning=FALSE, message=FALSE, fig.height=3.75, fig.cap = "Maps of New York which show with red dots the most and least used stations."----
# Create variables with the most and least used stations
top10 <- head(station_usage, 10)
least10 <- tail(station_usage, 10)

# Here we add the coordinates to our dataframe
top_10_coord <- left_join(x=top10, y=citi_cb, by=c("Station Name"="start.station.name"))

top10_coord <- top_10_coord %>% 
  select('Station Name','start.station.latitude', 'start.station.longitude', 'Bikes rented', 'Bikes returned', 'Total')

top10_coord <- distinct(top10_coord)

# Here we add the coordinates to our dataframe
least10_coord <- left_join(x=least10, y=citi_cb, by=c("Station Name"="start.station.name"))

least10_coord <- least10_coord %>% 
  select('Station Name','start.station.latitude', 'start.station.longitude', 'Bikes rented', 'Bikes returned', 'Total')

least10_coord <- distinct(least10_coord)

# Set longitudes and latitudes to define map tile for New York City
bbox <- c(left = -74.1, bottom = 40.65, right = -73.875, top = 40.825)
top10_map <- ggmap(get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")) +
                geom_point(data=top10_coord, 
                aes(x=start.station.longitude, 
                    y=start.station.latitude),
                color = 'red', size = 2, alpha = 0.5) +
                theme_update(plot.title = element_text(hjust = 0.5)) +
                ggtitle("10 most used Stations") +
                ggeasy::easy_center_title() +
                xlab('') +                            # Remove text on x-axis
                ylab('') +                            # Remove text on y-axis
                theme(axis.line = element_blank(),    # Remove axis line
                      axis.text = element_blank(),    # Remove axis text
                      axis.ticks = element_blank(),   # Remove axis ticks
                      plot.margin=unit(
                        c(-0.1,0.001,-0.3,0), "null"))# Margins around graph 

least10_map <- ggmap(get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")) +
                geom_point(data=least10_coord, 
                aes(x=start.station.longitude, 
                    y=start.station.latitude),
                color = 'red', size = 2, alpha = 0.5) +
                theme_update(plot.title = element_text(hjust = 0.5)) +
                ggtitle("10 least used Stations") +
                xlab('') +                            # Remove text on x-axis
                ylab('') +                            # Remove text on y-axis
                theme(axis.line = element_blank(),    # Remove axis line
                      axis.text = element_blank(),    # Remove axis text
                      axis.ticks = element_blank(),   # Remove axis ticks
                      plot.margin=unit(
                        c(-0.1,0,-0.3,0.001), "null"))# Margins around graph 

grid.arrange(top10_map, least10_map, ncol=2)          # Show the two maps side by side


## ---- warning=FALSE, message=FALSE------------------
# Set longitudes and latitudes to define map tile for New York City
bbox <- c(left = -74.1, bottom = 40.65, right = -73.875, top = 40.825)

# Create density map for start stations
start_station_dens <- ggmap(get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")) +
  stat_density_2d(
    data = citi_cb,
    aes(x=start.station.longitude, y=start.station.latitude, fill= ..level..),
    alpha = .15, bins = 18, geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlGnBu")) + # library RColorbrewer
  ggtitle(label = "Start Station Density") +
  theme(plot.title = element_text(hjust = 0.5),       # Center the plot title
       plot.margin=unit(c(0,0,-0.04,0), "null")) +    # Space between figure and caption      
  xlab("") +
  ylab("")

# Create density map for end stations
end_station_dens <- ggmap(get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")) +
  stat_density_2d(data = citi_cb, 
                  aes(x=end.station.longitude, y=end.station.latitude, fill= ..level..),
                  alpha = .15, bins = 18, geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlGnBu")) + # library RColorbrewer
  ggtitle(label = "End Station Density") +
  theme(plot.title = element_text(hjust = 0.5),       # Center the plot title
       plot.margin=unit(c(0,0,-0.04,0), "null")) +    # Space between figure and caption      
  xlab("") +
  ylab("")


## ---- echo=FALSE, include=TRUE,  fig.cap="Map of New York which shows where the most bikes were rented."----
start_station_dens


## ---- echo=FALSE, include=TRUE,  fig.cap="Map of New York which shows where the most bikes were returned."----
end_station_dens


## ---- fig.cap="Scatter plot to examine the influence of the average daily temperature on the trip durations."----
# How strong is the effect of the average daily temperature on the trip duration?
trip_dur_filter <- citi_cb %>%
  filter(tripduration < 8000)

ggplot(trip_dur_filter, aes(x=meantemp, y=tripduration)) +
  geom_point(color='#2980B9', size=1.5) + 
  geom_smooth(method=lm, color='darkred', linetype="dashed") +
  theme_light() +
  ggtitle("Effect of Average daily Temperature on Trip Duration") +
  ggeasy::easy_center_title() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(0,0,-0.04,0), "null")) +  
  xlab("Average daily temperature (in Celsius)") +
  ylab("Trip duration (in seconds)")


## ---------------------------------------------------
# Spearman because there are two quantitative variables
cor_tripdur_meantemp <- round(cor(x=citi_cb$meantemp, y=citi_cb$tripduration, method="spearman"),2)


## ---- results='asis', echo=FALSE--------------------
print(paste0("Interpretation: For every degree in Celsius, the time a customer rents a bike changes by ", cor_tripdur_meantemp, " seconds. Since we are using samples and not the whole population, the result needs to be interpreted with caution. To investigate the result further, we use inferential statistics in the form of an ANOVA test."))


## ---------------------------------------------------
# How strong is the effect of precipitation on the number of trips?
# Calculation for each category of precipitation how often it rained
rain_cat_freq <- citi_cb %>% 
  group_by(raingrouped) %>%
  summarise(counts = n())

# Plot of the result
rain_hist <- ggplot(rain_cat_freq, aes(x = raingrouped, y = counts)) +
  geom_bar(fill = "#2980B9",stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) +   # Label above bar
  theme_light() +
  ggtitle("Histogram with Rain Categories") +
  ggeasy::easy_center_title() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(0,0,-0.04,0), "null")) + 
  xlab("Rain Category") +
  ylab("Number of Rides")


## ---- fig.cap="Histogram which shows the distribution by rain category."----
rain_hist


## ---- fig.cap="Scatter plot to examine the influence of precipitation on the trip durations."----
trip_dur_filter <- citi_cb %>%
  filter(tripduration < 8000)

ggplot(trip_dur_filter, aes(x=PRCP, y=tripduration)) +
  geom_point(color='#2980B9', size=1.5) + 
  geom_smooth(method=lm, color='darkred', linetype="dashed") +
  theme_light() +
  ggtitle("Effect of Precipitation on Trip Duration") +
  ggeasy::easy_center_title() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(0,0,-0.04,0), "null")) + 
  xlab("Precipitation (in mm)") +
  ylab("Trip duration (in seconds)")


## ---------------------------------------------------
cor_tripduration_precipitation <- round(cor(x=citi_cb$PRCP, y=citi_cb$tripduration, 
                                            method="spearman"),2)


## ---- results='asis', echo=FALSE--------------------
print(paste0("Interpretation: For every mm of precipitation, the time a customer rents a bike changes by ", cor_tripduration_precipitation, " seconds. Since we are using samples and not the whole population, the result needs to be interpreted with caution. To investigate the result further, we use inferential statistics in the form of an ANOVA test."))


## ---------------------------------------------------
model <- aov(tripduration ~ meantemp + PRCP, data = citi_cb)
summary(model)

