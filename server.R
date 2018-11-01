
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  in_bounding_box <- function(data, lat, long, bounds) {
    data %>%
      dplyr::filter(lat > bounds$south &
                      lat < bounds$north &
                      long < bounds$east & long > bounds$west)
  }
  
  myvars <-
    c(
      "address",
      "city",
      "state",
      "price",
      "beds",
      "baths",
      "property_type",
      "days_on_market",
      "price_per_square_foot",
      "latitude",
      "longitude"
    )
  #      DTShow <- FinalData2[myvars]
  data_map <- reactive({
    if (is.null(input$map_bounds)) {
      FinalData2
    } else {
      if (input$beds == "All") {
        DTShow <-
          FinalData2[FinalData2$price >= input$price[1] &
                       FinalData2$price <= input$price[2], ]
      }
      else{
        DTShow <-
          FinalData2[FinalData2$beds == input$beds &
                       FinalData2$price >= input$price[1] &
                       FinalData2$price <= input$price[2], ]
      }
      DTShow <- DTShow[myvars]
      bounds <- input$map_bounds
      in_bounding_box(DTShow, DTShow$latitude, DTShow$longitude, bounds)
    }
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    if (input$beds == "All") {
      FinalData2[FinalData2$price >= input$price[1] &
                   FinalData2$price <= input$price[2], ]
    }
    else{
      FinalData2[FinalData2$beds == input$beds &
                   FinalData2$price >= input$price[1] &
                   FinalData2$price <= input$price[2], ]
    }
  })
  
  
  # Show the first "n" observations ----
  #      output$view <- renderTable({
  #        head(FinalData2)
  #      })
  #
  output$tbl <- DT::renderDataTable({
    DT::datatable(
      FinalData2,
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
  
  output$tbl2 <- DT::renderDataTable({
    DT::datatable(
      FinalData2,
      extensions = "Scroller",
      style = "bootstrap",
      class = c("cell-border", "stripe"),
      width = "100%",
      options = list(
        deferRender = TRUE,
        extensions = "Scroller",
        pageLength = 5,
        scrollY = 600,
        scroller = TRUE,
        #      initComplete = JS(
        #        "function(settings, json) {",
        #        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        #        "}"),
        searching = FALSE
        
      )
    )
  })
  
  # # Show a popup at the given location
  # showPopup <- function(zipcode, lat, lng) {
  #   selectedProp <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(FinalData2$)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #                              selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  # 
  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     showPopup(event$id, event$lat, event$lng)
  #   })
  # })
  
  
  ##Render the map and fit to Min and Max Lat/Lon of dataset
  output$map <- renderLeaflet({
    leaflet(FinalData2) %>%
      addTiles() %>%
      fitBounds( ~ min(longitude),
                 ~ min(latitude),
                 ~ max(longitude),
                 ~ max(latitude))
  })
  
  ##Add Markers for each observation in the FinalData Dataframe
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addTiles() %>%
      addAwesomeMarkers( ~longitude,
                         ~latitude,
                         popup =  ~Label,
                         clusterOptions = markerClusterOptions())
  
    })
  
  ##Update the Slider Inputs selectable Price Range based on the data that is filtered.
  observe({
    if (input$beds == "All") {
      updateSliderInput(
        session,
        "price",
        value = c(min(FinalData2$price), max(FinalData2$price)),
        min = min(FinalData2$price),
        max = max(FinalData2$price)
      )
    }
    else {
      updateSliderInput(
        session,
        "price",
        value = c(min(FinalData2[FinalData2$beds == input$beds, ]$price), max(FinalData2[FinalData2$beds ==
                                                                                           input$beds, ]$price)),
        min = min(FinalData2[FinalData2$beds == input$beds, ]$price),
        max = max(FinalData2[FinalData2$beds == input$beds, ]$price)
      )
    }
  })
  
  ##If Search is clicked show notification and run the function to search Zipcode
  ##Re-Render Map on Completion
  observeEvent(input$btnSearch, {
    showNotification(paste("Downloading Data for", input$zipcode, "please wait"))
    SearchZip(input$zipcode)
    output$map <- renderLeaflet({
      leaflet(FinalData2) %>%
        addTiles() %>%
        fitBounds( ~ min(longitude),
                   ~ min(latitude),
                   ~ max(longitude),
                   ~ max(latitude))
    })
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addTiles() %>%
      addAwesomeMarkers(
        ~ longitude,
        ~ latitude,
        label =  ~ paste(
          as.character(address),
          " Sold For:$",
          as.character(price),
          sep = ""
        ),
        clusterOptions = markerClusterOptions()
      )
    
    
  })
  
  
}
)
