
##########################################################################
# Load Libraries
##########################################################################

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
library(ggmap)
library(RColorBrewer)

##########################################################################
# Notes for this script
##########################################################################

# There are different ways to create maps in R. It needs to be differentiated between static and dynamic maps:

# Dynamic map: Interactive object which are designed for web pages or applications.
# Static map: Static maps can be used for any output format but are not interactive.

# Maps can be created in R with different map providers and packages. It needs to be noted that some of the providers require API keys:

# PACKAGES:
  # ggmap (static)
  # Leaflet (dynamic)
  # OpenStreetMap (dynamic)


# MAP PROVIDERS:
  # Google Maps (requires API key -> Credit card required ... no credit card required with developer access) https://www.youtube.com/watch?v=367oxHpnn_4&t=116s
  # OpenStreetMap (requires API key -> which is FREE)
  # Stamen Maps (no API required)
  # CloudMade Maps


##########################################################################
# Options
##########################################################################

#Deactivate scientific notation
options(scipen = 999)

##########################################################################
# Read CSV
##########################################################################

citibike_weather_df <- read.csv('citibike_weather_2019.csv')
View(citibike_weather_df)

ggplot(citi_cb,aes(y=tripduration)) +
  geom_boxplot() +
  scale_x_discrete() +
  theme_light() +
  theme(plot.margin=unit(c(0,0,-0.04,0), "null")) + # Space between figure and caption
  coord_flip()

library(skimr)
skim(citibike_weather_df)

skim(citibike_weather_df) %>%
  dplyr::select(skim_type, skim_variable, n_missing)

# Check whether the data set was successfully read into a data frame
class(citibike_weather_df)


##########################################################################
# Data mutation
##########################################################################

# Add column with trip duration groups
citibike_weather_df$tripduration_cat <- ifelse(citibike_weather_df$tripduration <=300, '0 to 5 min',
                                        ifelse(citibike_weather_df$tripduration >300 & citibike_weather_df$tripduration <=900, '5 to 15 min',
                                        ifelse(citibike_weather_df$tripduration >900 & citibike_weather_df$tripduration <=1800, '15 to 30 min',
                                        ifelse(citibike_weather_df$tripduration >1800 & citibike_weather_df$tripduration <=3600, '30 to 60 min','over 60 min'))))


# Here we add a new column to the DF with info summarized information
# The info can be used as a popup for markers in interactive maps 
# The info pops up when the markers on the map are clicked 
# paste0() concatenates the strings
citibike_weather_df <- mutate(citibike_weather_df, content=paste0('<strong>Start time: </strong>',starttime,
                                                                  '<br><strong>Stop time: </strong>',stoptime,
                                                                  '<br><strong>Start station: </strong>',start.station.name,
                                                                  '<br><strong>End station: </strong>',end.station.name,
                                                                  '<br><strong>Start station lat & long: </strong>',start.station.latitude,", ",start.station.longitude,
                                                                  '<br><strong>End station lat & long: </strong>',end.station.latitude,", ",end.station.longitude,
                                                                  '<br><strong>Avg temperature (°C): </strong>',meantemp))


##########################################################################
# Load Polygons      
##########################################################################

# New York City Bike Lanes Polygon

# Link to bike lanes in NYC: https://data.cityofnewyork.us/Transportation/Bicycle-Routes/7vsa-caz7
# Read polygons with New York City's bike lanes 
nyc_bike_lanes <- read_sf("C:/Users/noelr/OneDrive/Documents/rBootcamp/citibike_vis/NYC_BICYCLE_NETWORK_19D_202103.shp")

# Why it is important to transform the data: https://community.rstudio.com/t/projection-problems-with-leaflet/27747
nyc_bike_lanes_transformed <- st_transform(nyc_bike_lanes, 4326)


##########################################################################
# Inspect Data
##########################################################################

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


##########################################################################
# DATA ANALYSIS
##########################################################################


#############################################     #################################
# Top 10 most popular stations with Leaflet #     # !! NO USE FOR RMD FILE !!     #
#############################################     #################################

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

station_ranking_top10 %>% 
  select(start.station.name, count)

# Create map with markers to show the top 10 stations
m <- leaflet() %>%
  addTiles() %>%
  addCircles(data = station_ranking_top10,
             lng = station_ranking_top10$start.station.longitude, 
             lat = station_ranking_top10$start.station.latitude,
             popup = (citibike_weather_df$start.station.name)) %>% 
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Voyager")

# Create png of the created map
mapshot(m, file = "most_popular_stations.png")


###############################################    #################################
# Top 10 least popular stations with Leaflet  #    # !! NO USE FOR RMD FILE !!     #
###############################################    #################################

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



#####################################    #################################
# Plain map of New York                  # !! NO USE FOR RMD FILE !!     #
#####################################    #################################

leaflet() %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron")


##########################################################    ################################################
# Map of New York with polygons showing the neighborhoods     # Note: Leaflet maps not suitable for .Rmd file
##########################################################    ################################################

library(rgdal)
library(httr)
library(broom)

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F, use_iconv = TRUE, encoding = "UTF-8")

nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")



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


#####################################     #################################
# DENSITY MAP: Start Stations       #     # !! NO USE FOR RMD FILE !!     #
#####################################     #################################

# Shows where the most rides started and the bike lanes in NYC 


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
  
#####################################     #################################
# DENSITY MAP: End Stations         #     # !! NO USE FOR RMD FILE !!     #
#####################################     #################################

# Shows where the most rides ended and the bike lanes in NYC

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
  

#####################################     #################################
# Density Map with Mapbox           #     # !! NO USE FOR RMD FILE !!     #
#####################################     #################################

library(mapdeck)
mb_access_token(token="pk.eyJ1Ijoibm9lbHJuayIsImEiOiJja3hkZzd6a3A0c2IyMndsYWU3dDFoM2F3In0.DJEE19AotBVDcn9_1l-PUA", install = TRUE)
set_token(Sys.getenv("MAPBOX_PUBLIC_TOKEN"))
ms = mapdeck_style("dark")

mapdeck(style = ms, 
        pitch = 45, 
        location = c(0, 52), 
        zoom = 3) %>%
  add_grid(data = citibike_weather_df, 
           lat = "start.station.latitude", 
           lon = "start.station.longitude", 
           cell_size = 500,
           elevation_scale = 2, 
           layer_id = "grid_layer",
           colour_range = viridisLite::plasma(6))


#####################################
# Plain Map with Stamen (ggmap)
#####################################

# If map appears blurry in plots window, increase the windows size.

lat<- c(40.915568 , 40.495992)
long<- c(-74.257159 ,-73.699215)
bbox<- make_bbox(long, lat,f=0.05)
c<- get_map(bbox, maptype = "toner-lite" , source = "stamen")
ggmap(c)
# https://rpubs.com/Joe11579/455454


##################################
# Density map with ggmap (Stamen)
##################################

bbox <- c(left = -74.1, bottom = 40.65, right = -73.875, top = 40.825)
ggmap(get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")) +
  geom_point(data=citibike_weather_df, 
             aes(x=start.station.longitude, y=start.station.latitude),size = 0.1, alpha = 0.05)


bbox <- c(left = -74.1, bottom = 40.65, right = -73.875, top = 40.825)
ggmap(get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")) +
  stat_density_2d(
    data = citibike_weather_df, aes(x=start.station.longitude, y=start.station.latitude, fill= ..level..),
    alpha = .15,
    bins = 18,
    geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlGnBu"), guide="none") +
  ggtitle(label = "Density Map: Start Station") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Longitude") +
  ylab("Latitude")


###############################################################################################################
# Influence of Meteorological Factors on Bike-Sharing Service
###############################################################################################################

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

##############################
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
  


##################################################      ###########################
# How to create image files from interactive maps       # ! NO USE FOR RMD FILE ! #
##################################################      ###########################

webshot::install_phantomjs()
library(mapview)
library(webshot)

m <- leaflet() %>%
  addTiles() %>%
  addCircles(data = station_ranking_top10,
             lng = station_ranking_top10$start.station.longitude,
             lat = station_ranking_top10$start.station.latitude,
             popup = (citibike_weather_df$start.station.name)) %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Voyager")

m

## 'leaflet' objects (image above)
mapshot(m, file = "most_popular_stations.png")

image <- image_read("most_popular_stations.png")
image_crop(image, "992x446")

knitr::include_graphics('most_popular_stations.png')



#######################################################################
# MOST POPULAR START STATIONS
#######################################################################

# Data frame which ranks all stations by number of rides
start.station.rank <- data.frame(citibike_weather_df %>%
                             group_by(start.station.name) %>%
                             summarize(count=n()) %>%
                             arrange(desc(count)))

# Add coordinates to data frame
start.station.rank.coord <- start.station.rank %>%
  left_join(citibike_weather_df, 
            by = "start.station.name") %>%
  select(start.station.name, 
         count, 
         start.station.longitude, start.station.latitude)

# What we need for the density map
start.station.coord <- distinct(start.station.rank.coord)

start.station.rank.t10 <- head(distinct(start.station.rank.coord),10)

# Displaying the top 10 start stations
start.station.rank.t10 %>%
  select(start.station.name, count)


#########################################
# The 10 least popular start stations   #
#########################################

start.station.rank.l10 <- data.frame(citibike_weather_df %>%
                              group_by(start.station.name) %>% 
                              summarize(count=n()) %>% 
                              arrange(count)) %>% 
                              head(10)



# The 10 most popular end stations
citibike_weather_df %>% 
  group_by(end.station.name) %>% 
  summarise(count=n())

most_popular_end_stations <- data.frame(citibike_weather_df %>%
                                          group_by(end.station.name) %>%
                                          summarize(count=n()) %>%
                                          arrange(desc(count)))

most_popular_stations <- most_popular %>%
  left_join(citibike_weather_df, by = "end.station.name") %>%
  select(end.station.name, count, end.station.longitude, end.station.latitude)

end_station_ranking_top10 <- head(distinct(most_popular_stations),10)

end_station_ranking_top10 %>%
  select(end.station.name, count)


# The 10 least popular end stations
least_popular <- data.frame(citibike_weather_df %>%
                              group_by(start.station.name) %>% 
                              summarize(count=n()) %>% 
                              arrange(count))

least_popular_stations <- least_popular %>%
  left_join(citibike_weather_df, by = "start.station.name") %>%
  select(start.station.name, count, start.station.longitude, start.station.latitude)

station_ranking_least10 <- head(distinct(least_popular_stations),10)

station_ranking_least10 %>%
  select(start.station.name, count)




##############
library(data.table)

# Create a dataframe with a column which counts how often a start station was used
start.station.rank <- data.frame(citibike_weather_df %>%
                                       group_by(start.station.name) %>%
                                       summarize(count.start=n()) %>%
                                       arrange(desc(count.start)))

# Create a dataframe with a column which counts how often an end station was used
end.station.rank <- data.frame(citibike_weather_df %>%
                                     group_by(end.station.name) %>%
                                     summarize(count.end=n()) %>%
                                     arrange(desc(count.end)))


# Combine the start station and end station dataframes
# So that we have for each station the number of bike rented and the number of bikes returned
station_usage <- left_join(x=start.station.rank, y=end.station.rank, by=c("start.station.name"="end.station.name"))
station_usage



# Check if there are start or end stations which were not used
sum(is.na(station_usage$count.start))
sum(is.na(station_usage$count.end))

# Replace all NAs with 0 (Stations which no one used should be shown as zero for the subsequent sum)
station_usage <- station_usage %>% 
  mutate_all(~replace(., is.na(.), 0))
station_usage

# Check if the NA values were successfully replaced
sum(is.na(station_usage$count.start))
sum(is.na(station_usage$count.end))

# Create a new column which shows the combined number of bikes rented and returned
station_usage$station.count.comb <- rowSums(station_usage[, c("count.start", "count.end")]) 

# Order the dataframe by the combined number (from largest to smallest) to get a ranking
station_usage <- station_usage[order(-station_usage$station.count.comb),]

# Check the 10 most and least used stations
head(station_usage, 10)
tail(station_usage, 10)
# Rename columns for improved readability
setnames(station_usage, old = c('start.station.name','count.start', 'count.end', 'station.count.comb'), 
         new = c('Station Name','Bikes rented', 'Bikes returned', 'Total'))

# Create variables with the most and least used stations
top10 <- head(station_usage, 10)
least10 <- tail(station_usage, 10)

# Here we add the coordinates to our dataframes
top_10_coord <- left_join(x=top10, y=citibike_weather_df, by=c("Station Name"="start.station.name"))

top10_coord <- top_10_coord %>% 
  select('Station Name','start.station.latitude', 'start.station.longitude', 'Bikes rented', 'Bikes returned', 'Total')

distinct(top10_coord)

bbox <- c(left = -74.1, bottom = 40.65, right = -73.875, top = 40.825)
ggmap(get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")) +
  geom_point(data=top10_coord, 
             aes(x=start.station.longitude, 
                 y=start.station.latitude),
             size = 0.1, alpha = 0.05)
