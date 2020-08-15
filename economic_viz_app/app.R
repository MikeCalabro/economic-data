library(shiny)
library(tidyverse)
library(quantmod)
library(shinythemes)
library(xts)

ui <- fluidPage(
   
   theme = shinytheme("cosmo"),

   titlePanel("Federal Reserve Economic Data"),

   sidebarLayout(
      sidebarPanel(
        
         selectInput("stat",
                     "Select Economic Data to View:",
                     c("Real GDP" = "GDPC1",
                       "Consumer Price Index" ="CPIAUCSL",
                       "Unemployment Rate"  = "UNRATE",
                       "Initial Claims" = "ICSA",
                       "Total Nonfarm Payroll" = "PAYNSA",
                       "M1 Money Stock" = "M1",
                       "M2 Money Stock" = "M2",
                       "Velocity of M1 Money Stock" = "M1V",
                       "Velocity of M2 Money Stock" = "M2V",
                       "TED Spread" = "TEDRATE",
                       "Fed Funds Rate" = "FEDFUNDS",
                       "2-Year US Treasury Rate" = "DGS2",
                       "Fed Discount Rate" = "INTDSRUSM193N",
                       "Income Tax - Highest Bracket" = "IITTRHB",
                       "Income Tax - Lowest Bracket" = "IITTRLB"),
                     selected = "FEDFUNDS"),
         
         selectInput("plot",
                     "Plot Type:",
                     c("Line Graph" = "line",
                       "Scatterplot"  = "point",
                       "Area Chart" ="area"),
                     selected = "point"),
         
         selectInput("position",
                     "Select the Side of the Y-Axis:",
                     c("Right" = "right",
                       "Left" ="left"),
                     selected = "left"),
         
         sliderInput("breaks",
                     "Select Number of Y-axis ticks",
                     min = 5,
                     max = 40,
                     value = 20),
         
         sliderInput("year",
                     "Select Years to View:",
                     min = 1960,
                     max = 2020,
                     value = c(1980, 2020),
                     sep = ""),
         
         br(),
         h4("Shiny App Created By:"),
         h4("Michael Calabro")
      ),
      
      mainPanel(
         plotOutput("dataPlot")
      )
   )
)



server <- function(input, output) {
   
   output$dataPlot <- renderPlot({
     
     my_data <- getSymbols(input$stat, src = "FRED", auto.assign = FALSE)
     
     autoplot(my_data[sprintf("%i/%i", input$year[1], input$year[2])], geom = input$plot) +
       xlab("Year") +
       scale_y_continuous(n.breaks = input$breaks, position = input$position)
       
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

