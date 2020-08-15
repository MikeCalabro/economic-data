#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(quantmod)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Federal Reserve Economic Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
         selectInput("stat",
                     "Select Economic Data to View",
                     c("Fed Funds Rate" = "FEDFUNDS",
                       "Unemployment Rate"  = "UNRATE",
                       "ND000334Q",
                       "CPIAUCSL",
                       "M1V",
                       "DGS2",
                       "TEDRATE",
                       "INTDSRUSM193N"),
                     selected = "UNRATE"),
         
         sliderInput("year",
                     "Select Years to View:",
                     min = 1900,
                     max = 2020,
                     value = c(1980, 2020),
                     sep = "")
      ),
      
      mainPanel(
         plotOutput("distPlot"),
         textOutput("testText")
      )
   )
)



server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     my_data <- getSymbols(input$stat, src = "FRED", auto.assign = FALSE)
     autoplot(my_data[sprintf("%i/%i", input$year[1], input$year[2])])
   })
   
   output$testText <- renderText({
     print(input$stat)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

