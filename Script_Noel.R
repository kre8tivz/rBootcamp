
#####################################
# Load Libraries
#####################################

library(ggplot2)
library(maps)
library(leaflet)
library(skimr)
library(DataExplorer)
library(st)
library(sf)
library(dplyr)
library(leaflet.extras)
library(ggeasy)
library(ggpubr)

#####################################
# Options
#####################################

#Deactivate scientific notation
options(scipen = 999)

#####################################
# Read CSV
#####################################

citibike_weather_df <- read.csv('citibike_weather_2019.csv')
View(citibike_weather_df)

# Check whether the data set was successfully read into a data frame
class(citibike_weather_df)


#####################################
# Data mutation
#####################################

# Add column with trip duration groups

citibike_weather_df$tripduration_cat <- ifelse(citibike_weather_df$tripduration <=300, '0 to 5 min',
                                        ifelse(citibike_weather_df$tripduration >300 & citibike_weather_df$tripduration <=900, '5 to 15 min',
                                        ifelse(citibike_weather_df$tripduration >900 & citibike_weather_df$tripduration <=1800, '15 to 30 min',
                                        ifelse(citibike_weather_df$tripduration >1800 & citibike_weather_df$tripduration <=3600, '30 to 60 min','over 60 min'))))



#####################################
# Inspect Data
#####################################

# First and last 5 observations
head(citibike_weather_df)
tail(citibike_weather_df)

# Dimensions of the data set (number of rows and columns)
dim(citibike_weather_df)

# Variables of the data set
names(citibike_weather_df)

# Summary of the data set
summary(citibike_weather_df)

# Alternative to summary to get an overview of the data frame
skim(citibike_weather_df)

# Interesting function (creates an HTML report)
create_report(citibike_weather_df)

#####################################
# Top 10 most popular stations
#####################################

# Workaround to save image from Leaflet
# Leaflet is a JavaScript library for building interactive maps for the web and hence not static
webshot::install_phantomjs()
library(mapview)
library(webshot)

citibike_weather_df %>% 
  group_by(start.station.name) %>% 
  summarise(count=n())


most_popular <- data.frame(citibike_weather_df %>%
  group_by(start.station.name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)))

most_popular_stations <- most_popular %>%
  left_join(citibike_weather_df, by = "start.station.name") %>%
  select(start.station.name, count, start.station.longitude, start.station.latitude)

station_ranking_top10 <- head(distinct(most_popular_stations),10)
station_ranking_top10

m <- leaflet() %>%
  addTiles() %>%
  addCircles(data = station_ranking_top10,
             lng = station_ranking_top10$start.station.longitude, 
             lat = station_ranking_top10$start.station.latitude,
             popup = (citibike_weather_df$start.station.name)) %>% 
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Voyager")



## 'leaflet' objects (image above)
mapshot(m, file = "most_popular_stations.png")


#####################################
# Top 10 least popular stations
#####################################


least_popular <- data.frame(citibike_weather_df %>%
                             group_by(start.station.name) %>% 
                             summarize(count=n()) %>% 
                             arrange(desc(count)))

least_popular_stations <- least_popular %>%
  left_join(citibike_weather_df, by = "start.station.name") %>%
  select(start.station.name, count, start.station.longitude, start.station.latitude)

station_ranking_least10 <- head(distinct(least_popular_stations),10)


leaflet() %>%
  addTiles() %>%
  addCircles(data = station_ranking_least10,
             lng = station_ranking_least10$start.station.longitude, 
             lat = station_ranking_least10$start.station.latitude,
             popup = (citibike_weather_df$start.station.name)) %>% 
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron")



#####################################
# Map of New York
#####################################

leaflet() %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron")


#####################################
# Map of New York with polygons
#####################################

library(rgdal)
library(httr)

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F, use_iconv = TRUE, encoding = "UTF-8")

library(broom)

nyc_neighborhoods_df <- tidy(nyc_neighborhoods)


ggplot() + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group))


leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")



#####################################
# Map showing the starter          
#####################################

# Link to bike lanes in NYC: 
# https://data.cityofnewyork.us/Transportation/Bicycle-Routes/7vsa-caz7

nyc_bike_lanes <- read_sf("C:/Users/noelr/OneDrive/Documents/rBootcamp/citibike_vis/NYC_BICYCLE_NETWORK_19D_202103.shp")
# https://community.rstudio.com/t/projection-problems-with-leaflet/27747
nyc_bike_lanes_transformed <- st_transform(nyc_bike_lanes, 4326)



# Here we add a new column with the info which pops up when the markers are clicked to the DF
# paste0() concatenates the strings
citibike_weather_df <- mutate(citibike_weather_df, content=paste0('<strong>Start time: </strong>',starttime,
                                                                  '<br><strong>Stop time: </strong>',stoptime,
                                                                  '<br><strong>Start station: </strong>',start.station.name,
                                                                  '<br><strong>End station: </strong>',end.station.name,
                                                                  '<br><strong>Start station lat & long: </strong>',start.station.latitude,", ",start.station.longitude,
                                                                  '<br><strong>End station lat & long: </strong>',end.station.latitude,", ",end.station.longitude,
                                                                  '<br><strong>Avg temperature (°C): </strong>',meantemp))


leaflet(citibike_weather_df) %>%
  addPolygons(data = nyc_bike_lanes_transformed, # Add bike lanes to map
              color = "#2b8cbe", 
              fill = NA, 
              weight = 1.1) %>% 
  addTiles()%>%
  addCircleMarkers(data = citibike_weather_df, lng = citibike_weather_df$start.station.longitude, 
                   lat = citibike_weather_df$start.station.latitude,
                   radius = 3, 
                   popup = ~as.character(citibike_weather_df$content),
                   stroke = FALSE, 
                   fillOpacity = 0.8,
                   clusterOptions = markerClusterOptions()) %>%
  addProviderTiles("CartoDB.Voyager")


#####################################
# Density Map
# Shows where the most rides started and the bike lanes in NYC
#####################################

leaflet(citibike_weather_df) %>%
  addPolygons(data = nyc_bike_lanes_transformed, # Add bike lanes to map
              color = "blue", 
              fill = NA, 
              weight = 1.1) %>% 
  addTiles()%>%
  addHeatmap(lng = citibike_weather_df$start.station.longitude, 
             lat = citibike_weather_df$start.station.latitude, 
             max=100, radius=20, blur=10) %>% 
  addProviderTiles("CartoDB.Positron")
  
#####################################
# Density Map
# Shows where the most rides ended and the bike lanes in NYC
#####################################

leaflet(citibike_weather_df) %>%
  addPolygons(data = nyc_bike_lanes_transformed, # Add bike lanes to map
              color = "blue", 
              fill = NA, 
              weight = 1.1) %>% 
  addTiles()%>%
  addHeatmap(lng = citibike_weather_df$end.station.longitude, 
             lat = citibike_weather_df$end.station.latitude, 
             max=100, radius=20, blur=10) %>% 
  addProviderTiles("CartoDB.Positron")  
  

#####################################
# Influence of Meteorological Factors
#####################################

# Check if the variable trip duration has outliers
boxplot(citibike_weather_df$tripduration)

# There are outliers -> Remove outliers in trip duration
citibike_weather_df <- subset(citibike_weather_df, tripduration < 6000)

# Check how the variables trip duration and average temperature are distributed
hist(citibike_weather_df$tripduration)
hist(citibike_weather_df$meantemp)
hist(citibike_weather_df$PRCP)

# Neither the variable trip duration nor the variable average temperature
# are normally distributed and both are quantitative variables
# Hence, we use a nonparametric test -> Spearman's rank correlation


# Influence of Temperature
##############################

## How strong is the effect of the average daily temperature on the number of trips per day?

## How strong is the effect of the average daily temperature on the trip duration?

ggplot(citibike_weather_df, aes(x=meantemp, y=tripduration)) +
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50') +
  ggtitle(label="Effect of Average daily Temperature on Trip Duration") +
  ggeasy::easy_center_title() +
  xlab("Average daily temperature") +
  ylab("Trip duration (in seconds")

cor(x=citibike_weather_df$meantemp, y=citibike_weather_df$tripduration, method="spearman")

## How strong is the effect of precipitation on the number of trips?

# Calculate the for each category of precipitation how often it rained
rain_cat_freq <- citibike_weather_df %>%
  group_by(raingrouped) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

rain_cat_freq

ggplot(rain_cat_freq, aes(x=raingrouped, y=n)) +
  geom_bar(fill = "#0073C2FF")

hist(rain_cat_freq)

rain_cat_freq <- citibike_weather_df %>%
  group_by(raingrouped) %>%
  summarise(counts = n())


ggplot(rain_cat_freq, aes(x = raingrouped, y = counts)) +
  geom_bar(fill = "#0073C2FF", 
           stat = "identity") +
  geom_text(aes(label = counts), 
            vjust = -0.3) # Label above bar

## How strong is the effect of precipitation on the trip duration?

ggplot(citibike_weather_df, aes(x=PRCP, y=tripduration)) +
  geom_point(color='#2980B9') + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50') +
  ggtitle(label="Effect of Precipitation on Trip Duration") +
  ggeasy::easy_center_title() +
  xlab("Precipitation (in mm)") +
  ylab("Trip duration (in seconds)")

cor_tripduration_precipitation <- round(cor(x=citibike_weather_df$PRCP, y=citibike_weather_df$tripduration, method="spearman"),2)
print(paste0("Interpretation: For every mm of precipitation, the time a customer rents a bike changes by ", cor_tripduration_precipitation, " seconds."))
