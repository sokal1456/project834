library(leaflet)
library(maps)
library(tigris)
library(dplyr)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(scales)
library(tidyverse)
library(ggmap)
library(devtools)
library(emo)
library(tidycensus) 
library(FinCal)
library(viridis)
library(stringr)
library(sf)
library(shiny)

census_clean_data_2017_bed <- get_acs(geography = "zip code tabulation area",
                                      variables = c("1" = "B25031_003", "2" = "B25031_004", "3" = "B25031_005", "4" = "B25031_006", "5" = "B25031_007"),
                                      geometry = TRUE)

mlsdata<-read_csv("myRealEstateData.csv") %>% select(-c(description))
mlsdata <- mlsdata[-1,]

points_by_zip <- mlsdata %>% 
        group_by(postal_code, city, state, beds) %>%
        summarize(med_price=median(price))  %>%
        filter(state=="CT" || state=="MA")

  
myJoin <- census_clean_data_2017_bed %>%
rename(postal_code = GEOID) %>%
rename(beds=variable) %>%
mutate(beds=as.numeric(beds)) %>% 
inner_join(points_by_zip,by=c("postal_code","beds")) %>%
mutate(annualized_income=estimate*12) %>% 
mutate(price_rent_ratio=med_price/annualized_income) %>%
#filter(state=="CT") %>%
filter(beds==2)


#map_data$med_price<-ifelse(map_data$med_price=="NA", 0, map_data$med_price)
pal <- colorQuantile(palette = "viridis", domain = myJoin$price_rent_ratio, n=10)
myJoin$label<-paste0(myJoin$city, " ", (round(myJoin$price_rent_ratio,2)))





myJoin %>%
  st_transform(crs = "+init=epsg:4326") %>%

### this graph shows us where the most expensive prices are
leaflet(width = "100%") %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(popup = ~ label,
                            stroke = FALSE,
                            smoothFactor = 0,
                            fillOpacity = 0.7,
                            color = ~ pal(price_rent_ratio))  
  
addLegend("topleft",
            pal = pal,
            values = ~ med_price,
            title = "Median Housing Prices",
            opacity = .5)

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points"),
  selectInput("state", "Choose State", c("Massachusetts" = "MA", "Connecticut" = "CT")),
    selectInput("Selector", "Choose Map Type", c("HeatMap" = "hm", "Props" = "p"))

  )

server <- function(input, output, session) {
  
output$mymap <- renderLeaflet({

if (input$Selector == "hm")
{
  #map_data$med_price<-ifelse(map_data$med_price=="NA", 0, map_data$med_price)
  pal <- colorQuantile(palette = "viridis", domain = myJoin$price_rent_ratio, n=10)
  myJoin$label<-paste0(myJoin$city, " ", (round(myJoin$price_rent_ratio,2)))
        
    myJoin %>%
    subset(state == input$state) %>%
      st_transform(crs = "+init=epsg:4326") %>%
      ### this graph shows us where the most expensive prices are
      leaflet(width = "100%") %>%
      addTiles() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(popup = ~ label,
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal(price_rent_ratio))  
}
else {
  mlsDataShow <- subset(mlsdata, state == input$state)
  mlsDataShow$longitude <- as.double(mlsDataShow$longitude)
  mlsDataShow$latitude <- as.double(mlsDataShow$latitude)
    leaflet("mymap", data = mlsDataShow) %>%
    clearMarkers() %>%
    clearMarkerClusters() %>%
    addTiles() %>%
    addMarkers(~longitude, ~latitude,  clusterOptions = markerClusterOptions())  
}
  })
}

shinyApp(ui, server)
