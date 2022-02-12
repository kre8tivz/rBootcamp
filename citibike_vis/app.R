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
library(leaflet.extras)
library(shinythemes)

library(shinycssloaders)

setwd("..")

# Read the data with the weather and the rides
citibike_weather_df <- read.csv("C:/Users/noelr/OneDrive/Documents/rBootcamp/citibike_weather_2019.csv", stringsAsFactors=FALSE)

# Read the bike lane shape file
nyc_bike_lanes <- read_sf("C:/Users/noelr/OneDrive/Documents/rBootcamp/citibike_vis/NYC_BICYCLE_NETWORK_19D_20210311.shp")
# https://community.rstudio.com/t/projection-problems-with-leaflet/27747
nyc_bike_lanes_transformed <- st_transform(nyc_bike_lanes, 4326)

# Read the shape file with NYC's boroughs
nyc_boroughs <- read_sf("C:/Users/noelr/OneDrive/Documents/rBootcamp/citibike_vis/nybb.shp")
# https://community.rstudio.com/t/projection-problems-with-leaflet/27747
nyc_boroughs_transformed <- st_transform(nyc_boroughs, 4326)

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

# Define UI for application
ui <- navbarPage(
    
    # Crete navbar to choose between map, data and readme
    navbarPage("Citibike New York City", theme = shinytheme("lumen"), id="main",
               tabPanel("Map", leafletOutput("citibikemap", height=1000, width=1000)),
               tabPanel("Data", DT::dataTableOutput("data")),
               tabPanel("Read Me",includeMarkdown("C:/Users/noelr/OneDrive/Documents/rBootcamp/README.md"))),

                       
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Create the map for the navbar
    output$citibikemap <- renderLeaflet({
        leaflet(citibike_weather_df) %>%
            addTiles() %>%
            addPolygons(data = nyc_bike_lanes_transformed, # Add bike lanes to map
                        color = "#2c7fb8", 
                        fill = NA, 
                        weight = 1.5) %>%
            addPolygons(data = nyc_boroughs_transformed, # Add polygons to highlight boroughs
                        color = "#ff9900",
                        fill = NA,
                        weight = 2) %>% 
            addCircleMarkers(lng = citibike_weather_df$start.station.longitude, lat = citibike_weather_df$start.station.latitude,
                             radius = 3,
                             popup = ~as.character(citibike_weather_df$content),
                             stroke = FALSE,
                             fillOpacity = 0.5,
                             clusterOptions = markerClusterOptions()) %>%
            addProviderTiles("CartoDB.Voyager") %>%
            clearBounds() %>% 
            addEasyButton(easyButton(
                icon="fa-crosshairs", 
                title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
            addHeatmap(lng = citibike_weather_df$start.station.longitude, 
                       lat = citibike_weather_df$start.station.latitude,
                       radius=8)
    })
    
    # Create the data panel for the navbar
    output$data <-DT::renderDataTable(datatable(
        citibike_weather_df[,c("tripduration", "starttime", "stoptime", "start.station.name", "end.station.name", "DATE", "meantemp", "raingrouped", "Snow_yesno")],
        filter = 'top',
        colnames = c("Trip Duration (in sec)", "Start Time", "End Time", "Start Station Name", "End Station Name","Date",
                     "Average Temp (in C)","Rain Category","Snow?")
    ))
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)