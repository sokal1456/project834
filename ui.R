#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage("Project 834", id = "nav",
             tabPanel(
               "Interactive Map",
               div(
                 class = "outer",
                 tags$head(# Include our custom CSS
                   includeCSS("styles.css")),
                 
                 leafletOutput('map', width = "100%", height = "90%"),
                 
                 ##Initialize Input Filters for Bedrooms, Price, Property Type, and Zipcode
                 absolutePanel(
                   top = 10,
                   right = 10,
                   draggable = FALSE,
                   wellPanel(
                     HTML(markdownToHTML(
                       fragment.only = TRUE,
                       text = c(
                         "<h3>Welcome to Project 834</h3><h5>Please select filters below</h5><i>Note: Type is not activated</i>"
                       )
                     )),
                     selectInput("beds", "# of Beds",
                                 c("All", levels(FinalData2$beds))),
                     sliderInput(
                       "price",
                       "Sales Price",
                       min(FinalData2$price),
                       500000,
                       c(min(FinalData2$price), max(FinalData2$price)),
                       40000
                     ),
                     selectInput("type", "Property Type",
                                 propTypes),
                     textInput("zipcode", "Please Enter Zipcode to search"),
                     actionButton("btnSearch", "Search")
                     
                   ),
                   style = "opacity: 0.85"
                 ),
                 absolutePanel(
                   bottom = 10,
                   left = 0,
                   width = "100%",
                   height = "10%",
                   draggable = FALSE,
                   
                   # Output: HTML table with requested number of observations ----
                   dataTableOutput("tbl"),
                   
                   
                   style = "opacity: 0.60"
                 )
               )),
             tabPanel("Data explorer",

                      DT::dataTableOutput("tbl2")
             )
             
             ))
