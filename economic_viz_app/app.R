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
         sliderInput("year",
                     "Select Years to View:",
                     min = 1980,
                     max = 2020,
                     value = c(1980, 2020),
                     sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

getSymbols("FEDFUNDS", src = "FRED")


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      autoplot(FEDFUNDS[sprintf("%i/%i", input$year[1], input$year[2])])
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

