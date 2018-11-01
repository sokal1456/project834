library(httr)
library(jsonlite)
library(lubridate)
library(jsonlite)
library(foreach)
library(doParallel)
require(RJSONIO) 
library(leaflet)
library(crosstalk)
library(tidyverse)
library(shiny)
library(shinycssloaders)
library(markdown)
library(DT)
library(leaflet)
library(zipcode)
library(scales)


#ClearMLS Username and Password Information
username <- "e2c04699573192d60c6cd5912c7094b6"
password <- ""

#Function to assist with the flattening of retrieved data from ClearMLS
FLAT <- function(inlist) {
  A <- data.frame(inlist)
  out <- lapply(A, function(y) {
    if (is.list(y)) {
      y <- unlist(y)
      m <- matrix(y, nrow(A), byrow = TRUE, dimnames = list(NULL, unique(names(y))))
      y <- data.frame(m, stringsAsFactors = FALSE)
      y[] <- lapply(y, type.convert)
    }
    y
  })
  do.call(cbind, out)
}


MyCTData <- data.frame()

##Zipcode Search function - Returns all Active MLS data from queried zipcode
SearchZip <- function (rzip) {
  
  username <- "e2c04699573192d60c6cd5912c7094b6"
  password <- ""
  FinalData <- data.frame()
  i <- TRUE
  ##Sets Variable pagenum which is the current page being retrieved from Clear MLS as a global Variable for access on progress bar loading in teh future (e.g. Currently loading page 3 of 10)
  pagenum <<- 1

  ##Sets Query based on Zipcode function input and an active status
  base <- paste("https://api.clearmls.io/properties?query=postal_code:'",rzip,"'%20status:'Active'&page=",pagenum,sep="")
  
  ##Get command which uses query and authenticaion above to retrieve JSON listing output (50 listing max per page).
  ##Note: This initial query can be removed with code enhancemnt.  Made it easy to run through loops below
  get_listings <- GET(base,authenticate(username,password, type="basic"))
  s_data <- content(get_listings)
  if (length(s_data$properties) > 0) {
  ##Loop through each page until there are no properties recieved in the query 
  while (i) {
    if (length(s_data[["properties"]]) > 0){      #this checks the length so on 14th page it will fail
      i = TRUE
      print(paste("Downloading Page ", pagenum, " for ", rzip,sep = ""))

      DataPull <- s_data$properties
      DataPull <- data.frame(t(sapply(DataPull,c)))
      FinalData <- rbind.data.frame(FinalData,DataPull)
      pagenum <<- pagenum+1
      base <- paste("https://api.clearmls.io/properties?query=postal_code:'",rzip,"'%20status:'Active'&page=",pagenum,sep="")
      get_listings <- GET(base,authenticate(username,password, type="basic"))
      #Convert JSON File retrieved to a dataframe and add to existing Data already received
      s_data <- content(get_listings)
      
      }else{
      i = FALSE
      try({
      FinalData2 <<- FLAT(FinalData)

      #Add Column Names
      names(FinalData2) <- names(FinalData)
      print("Download Completed")      
      #Convert Long/Lat and price to Numeric and beds to factors
      FinalData2$longitude <- as.numeric(FinalData2$longitude)
      FinalData2$latitude <- as.numeric(FinalData2$latitude)
      FinalData2$price <- as.numeric(as.character(FinalData2$price))
      FinalData2$beds <- factor(FinalData2$beds)
      FinalData2$mls_number <- as.character(FinalData2$mls_number)
      FinalData2$address <- as.character(FinalData2$address)
      FinalData2$description <- as.character(FinalData2$description)
      FinalData2$sold_date <- as.character(FinalData2$sold_date)
      FinalData2$Label <- paste(sep = "",
                               "<b>",dollar(FinalData2$price), "</b>", "<br/>",
                               FinalData2$address, "<br/>", FinalData2$city, ", " ,FinalData2$state, " ", FinalData2$postal_code," <img src='https://upload.wikimedia.org/wikipedia/commons/b/b1/El_Frigor%C3%ADfico_ANGLO_en_acci%C3%B3n.jpg", "' />")
      MyCTData <<- rbind(MyCTData, FinalData2)
      
      }, silent = TRUE)
          

    }
  

  }




#Function that creates a different color based on number of bedrooms. This was removed for now but can be re-added
# getColor <- function(dFilter) {
#   sapply(dFilter$beds, function(beds) {
#     if(beds <= 1) {
#       "green"
#     } else if(beds <= 2) {
#       "orange"
#     } else {
#       "red"
#     } })
# }

#Using AwesomeIcons Function to apply colors from above
# icons <- awesomeIcons(
#   icon = 'ios-close',
#   iconColor = 'black',
#   library = 'ion',
#   markerColor = getColor(FinalData2)
# )




##Extract out all Property Types to filter.  Remove anything that's super long (likely misentered)
propTypes <- levels(FinalData2$property_type)
propTypes <<- propTypes[nchar(as.character(propTypes)) <= 30]
}
else {
  print(paste(rzip," does not contain any listings", sep = ""))
  return(NULL)
}
}

##Uncomment
#data(zipcode)
#CTZips <- subset(zipcode,zipcode$state == "CT")
#ZipsSearch <- zipcode[-c(1:238),]
#sapply(ZipsSearch$zip,SearchZip)
MyCTData <- ""
SearchZip("06095")
FinalData2 <- MyCTData

#Convert Long/Lat and price to Numeric and beds to factors
FinalData2$longitude <- as.numeric(FinalData2$longitude)
FinalData2$latitude <- as.numeric(FinalData2$latitude)
FinalData2$price <- as.numeric(as.character(FinalData2$price))
FinalData2$beds <- factor(FinalData2$beds)
FinalData2$Label <- paste(sep = "",
                          "<b>",dollar(FinalData2$price), "</b>", "<br/>",
                          FinalData2$address, "<br/>", FinalData2$city, ", " ,FinalData2$state, " ", FinalData2$postal_code," <img src='https://upload.wikimedia.org/wikipedia/commons/b/b1/El_Frigor%C3%ADfico_ANGLO_en_acci%C3%B3n.jpg", "' />")

FinalData2 <- read.csv("myRealEstateData.csv", sep = ";")


app <- shinyApp(
  ui <- bootstrapPage(
    tags$head(tags$script(src = "message-handler.js")),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
      leafletOutput('map', width = "100%", height = "90%"),

##Initialize Input Filters for Bedrooms, Price, Property Type, and Zipcode
      absolutePanel(top = 10, right = 10, 
                    draggable = FALSE,
                    wellPanel(
                      HTML(markdownToHTML(fragment.only=TRUE, text=c(
                        "<h3>Welcome to Project 834</h3><h5>Please select filters below</h5><i>Note: Type is not activated</i>"
                      ))),
      selectInput("beds", "# of Beds",
                  c("All",levels(FinalData2$beds))),
      sliderInput("price", "Sales Price",min(FinalData2$price),500000,c(min(FinalData2$price),max(FinalData2$price)),40000),
      selectInput("type", "Property Type",
                  propTypes),
      textInput("zipcode", "Please Enter Zipcode to search"),
      actionButton("btnSearch", "Search")
      
),
style = "opacity: 0.85"
      ),
absolutePanel(bottom = 10, left = 0, width="100%", height = "10%",
              draggable = FALSE,


                # Output: HTML table with requested number of observations ----
                dataTableOutput("tbl"),
                

              style = "opacity: 0.60"
)      

    
  ),

    server <- function(input, output, session) {
      
      in_bounding_box <- function(data, lat, long, bounds) {
        data %>%
          dplyr::filter(
            lat > bounds$south &
              lat < bounds$north &
              long < bounds$east & long > bounds$west 
          )
      }
      
      myvars <- c("address", "city", "state", "price", "beds", "baths", "property_type", "days_on_market", "price_per_square_foot", "latitude", "longitude")
#      DTShow <- FinalData2[myvars]
      data_map <- reactive({
        if (is.null(input$map_bounds)) {
          FinalData2
        } else {
          if(input$beds=="All"){
           DTShow <- FinalData2[FinalData2$price >= input$price[1] & FinalData2$price <= input$price[2],]
          }
          else{
            DTShow <- FinalData2[FinalData2$beds==input$beds & FinalData2$price >= input$price[1] & FinalData2$price <= input$price[2],]
          }
          DTShow <- DTShow[myvars]
          bounds <- input$map_bounds
          in_bounding_box(DTShow, DTShow$latitude, DTShow$longitude, bounds)
        }
      })
      
      # Reactive expression for the data subsetted to what the user selected
      filteredData <- reactive({
        if(input$beds=="All"){
          FinalData2[FinalData2$price >= input$price[1] & FinalData2$price <= input$price[2],]
        }
        else{
        FinalData2[FinalData2$beds==input$beds & FinalData2$price >= input$price[1] & FinalData2$price <= input$price[2],]
}
                })
      

      # Show the first "n" observations ----
#      output$view <- renderTable({
#        head(FinalData2)
#      })
      # 
      output$tbl <- DT::renderDataTable({
        DT::datatable(
          data_map(),
          extensions = "Scroller",
          style = "bootstrap",
          class = c("cell-border", "stripe"),
          width = "100%",
          options = list(
            deferRender = TRUE,
            extensions = "Scroller",
            pageLength = 5,
            scrollY = 300,
            scroller = TRUE,
      #      initComplete = JS(
      #        "function(settings, json) {",
      #        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      #        "}"),
            searching = FALSE

          )
        )
      })

##Render the map and fit to Min and Max Lat/Lon of dataset
        output$map <- renderLeaflet({
          leaflet(FinalData2) %>%
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
          })

##Add Markers for each observation in the FinalData Dataframe        
        observe({
          leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            addTiles() %>%
            addAwesomeMarkers(~longitude, ~latitude, label=~Label, clusterOptions = markerClusterOptions())  
                      })

##Update the Slider Inputs selectable Price Range based on the data that is filtered.        
        observe({
          if(input$beds=="All"){
            updateSliderInput(session,"price",value = c(min(FinalData2$price),max(FinalData2$price)),min = min(FinalData2$price), max = max(FinalData2$price))
            }
          else {
            updateSliderInput(session,"price",value = c(min(FinalData2[FinalData2$beds==input$beds,]$price),max(FinalData2[FinalData2$beds==input$beds,]$price)),min = min(FinalData2[FinalData2$beds==input$beds,]$price), max = max(FinalData2[FinalData2$beds==input$beds,]$price))
          }
            })

  ##If Search is clicked show notification and run the function to search Zipcode
  ##Re-Render Map on Completion
      observeEvent(input$btnSearch, {
        showNotification(paste("Downloading Data for",input$zipcode, "please wait"))
        SearchZip(input$zipcode)
        output$map <- renderLeaflet({
          leaflet(FinalData2) %>%
            addTiles() %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
        })
        leafletProxy("map", data = filteredData()) %>%
          clearMarkers() %>%
          clearMarkerClusters() %>%
          addTiles() %>%
          addAwesomeMarkers(~longitude, ~latitude, label=~paste(as.character(address)," Sold For:$",as.character(price), sep = ""), clusterOptions = markerClusterOptions())
          

                  })
        

    }
)


if (interactive()) print(app)
