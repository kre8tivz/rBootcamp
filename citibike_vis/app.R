#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(sf)

setwd("..")

# Define UI for application
ui <- navbarPage(
    
    # Crete navbar to choose between map, data and readme
    navbarPage("Citibike New York City", id="main",
               tabPanel("Map", leafletOutput("citibikemap", height=1000, width=1000)),
               tabPanel("Data", DT::dataTableOutput("data")),
               tabPanel("Read Me",includeMarkdown("C:/Users/noelr/OneDrive/Documents/rBootcamp/README.md")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Read the data with the weather and the rides
    citibike_weather_df <- read.csv("C:/Users/noelr/OneDrive/Documents/rBootcamp/citibike_weather_2019.csv", stringsAsFactors=FALSE)
    
    # # Add a new column to the dataframe to group the trip durations
    # citibike_weather_df$tripduration_cat <- ifelse(citibike_weather_df$tripduration <=300, '0 to 5 min',
    #                                         ifelse(citibike_weather_df$tripduration >300 & citibike_weather_df$tripduration <=900, '5 to 15 min',
    #                                         ifelse(citibike_weather_df$tripduration >900 & citibike_weather_df$tripduration <=1800, '15 to 30 min',
    #                                         ifelse(citibike_weather_df$tripduration >1800 & citibike_weather_df$tripduration <=3600, '30 to 60 min','over 60 min'))))
    
    nyc_bike_lanes <- read_sf("C:/Users/noelr/OneDrive/Documents/rBootcamp/citibike_vis/NYC_BICYCLE_NETWORK_19D_20210311.shp")
    # https://community.rstudio.com/t/projection-problems-with-leaflet/27747
    nyc_bike_lanes_transformed <- st_transform(nyc_bike_lanes, 4326)
    
    
    # Create color palette for category type
    # pal <- colorFactor(pal = c("#33BEFF", "#7AFF33", "#FF5733", "#FFDD33"), domain = c("winter", "spring", "summer", "autumn"))
    #pal <- colorFactor(pal = c("#DAECE4", "#BEECD8", "#89E7BE", "#53E8A7", "#0BEA89"), domain = c("0 to 5 min", "5 to 15 min", "15 to 30 min", "30 to 60 min", "over 60 min"))
    
    # Here we add a new column with the info which pops up when the markers are clicked to the DF
    # paste0() concatenates the strings
    citibike_weather_df <- mutate(citibike_weather_df, content=paste0('<strong>Start time: </strong>',starttime,
                                                                      '<br><strong>Stop time: </strong>',stoptime,
                                                                      '<br><strong>Start station: </strong>',start.station.name,
                                                                      '<br><strong>End station: </strong>',end.station.name,
                                                                      '<br><strong>Start station lat & long: </strong>',start.station.latitude,", ",start.station.longitude,
                                                                      '<br><strong>End station lat & long: </strong>',end.station.latitude,", ",end.station.longitude,
                                                                      '<br><strong>Avg temperature (°C): </strong>',meantemp
                                                                      ))
    # Create the map for the navbar
    output$citibikemap <- renderLeaflet({
        leaflet(citibike_weather_df) %>%
            addTiles() %>%
            addPolygons(data = nyc_bike_lanes_transformed, # Add bike lanes to map
                        color = "#2c7fb8", 
                        fill = NA, 
                        weight = 1.5) %>% 
            addCircles(lng = citibike_weather_df$start.station.longitude, 
                       lat = citibike_weather_df$start.station.latitude,
                       color = "#8dd3c7") %>%
            addCircleMarkers(lng = citibike_weather_df$start.station.longitude, lat = citibike_weather_df$start.station.latitude,
                             radius = 3, 
                             popup = ~as.character(citibike_weather_df$content),
    #                        color = ~pal(tripduration_cat),
                             stroke = FALSE, 
                             fillOpacity = 0.1,
                             clusterOptions = markerClusterOptions()) %>%
    #       addLegend(pal=pal, values=citibike_weather_df$tripduration_cat, opacity=1, na.label="Not Available") %>%
            addProviderTiles("CartoDB.Voyager") %>%
            clearBounds() %>% 
            addEasyButton(easyButton(
                icon="fa-crosshairs", 
                title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    
    # Create the data panel for the navbar
    output$data <-DT::renderDataTable(datatable(
        citibike_weather_df[,c("tripduration", "starttime", "stoptime", "start.station.name", "end.station.name", "DATE", "meantemp", "raingrouped", "Snow_yesno")],filter = 'top',
        colnames = c("Trip Duration (in sec)", "Start Time", "End Time", "Start Station Name", "End Station Name","Date",
                     "Average Temp (in C)","Rain Category","Snow?")
    ))
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)