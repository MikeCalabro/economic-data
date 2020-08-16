library(shiny)
library(tidyverse)
library(quantmod)
library(shinythemes)
library(xts)

ui <- fluidPage(
   
   theme = shinytheme("readable"),

   titlePanel("Federal Reserve Economic Data"),

   sidebarLayout(
      sidebarPanel(
        
         selectInput("stat",
                     "Select Economic Data to View:",
                     c("Real GDP" = "GDPC1",
                       "Consumer Price Index" ="CPIAUCSL",
                       "Unemployment Rate"  = "UNRATE",
                       "Initial Claims" = "ICSA",
                       "Total Nonfarm Payroll" = "PAYEMS",
                       "M1 Money Stock" = "M1",
                       "M2 Money Stock" = "M2",
                       "Velocity of M1 Money Stock" = "M1V",
                       "Velocity of M2 Money Stock" = "M2V",
                       "TED Spread" = "TEDRATE",
                       "Fed Funds Rate" = "FEDFUNDS",
                       "Fed Discount Rate" = "INTDSRUSM193N",
                       "3-Month US Treasury Rate" = "DTB3",
                       "1-Year US Treasury Rate" = "DGS1",
                       "2-Year US Treasury Rate" = "DGS2",
                       "5-Year US Treasury Rate" = "DGS5",
                       "10-Year US Treasury Rate" = "DGS10",
                       "20-Year US Treasury Rate" = "DGS20",
                       "30-Year US Treasury Rate" = "DGS30",
                       "10-Year Rate Minus 2-Year Rate" = "T10Y2Y",
                       "Income Tax - Highest Bracket" = "IITTRHB",
                       "Income Tax - Lowest Bracket" = "IITTRLB"),
                     selected = "UNRATE"),
         
         selectInput("plot",
                     "Select Plot Type:",
                     c("Line Graph" = "line",
                       "Scatterplot"  = "point",
                       "Area Chart" ="area"),
                     selected = "line"),
         
         selectInput("position",
                     "Select the Side of the Y-Axis:",
                     c("Right" = "right",
                       "Left" ="left"),
                     selected = "left"),
         
         sliderInput("breaks",
                     "Select Number of Y-axis ticks",
                     min = 5,
                     max = 40,
                     value = 20,
                     step = 5),
         
         sliderInput("year",
                     "Select Years to View:",
                     min = 1940,
                     max = 2020,
                     value = c(2005, 2020),
                     sep = ""),
         
         checkboxInput("trendline",
                       "Show Trendline",
                       value = FALSE),
         
         h4("Shiny App Created By:"),
         h4("Michael Calabro"),
         br(),
         a("Data sourced from Federal Reserve of St. Louis Website", href="https://fred.stlouisfed.org/")
      ),
      
      mainPanel(
         plotOutput("dataPlot"),
         br(),
         textOutput("description")
      )
   )
)



server <- function(input, output) {
   
   output$dataPlot <- renderPlot({
     
     my_data <- getSymbols(input$stat, src = "FRED", auto.assign = FALSE)
     
     autoplot(my_data[sprintf("%i/%i", input$year[1], input$year[2])], geom = input$plot) +
       xlab("Year") +
       ylab("") +
       ggtitle(sprintf("Select Economic Data From %i - %i", input$year[1], input$year[2])) +
       scale_y_continuous(n.breaks = input$breaks, position = input$position) +
       if(input$trendline){
         geom_smooth(se = FALSE)
       } 
   })
   
   output$description <- renderText({
     print("Describing the Economic Variable")
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

