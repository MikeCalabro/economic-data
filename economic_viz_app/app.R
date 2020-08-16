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
         br(),
         textOutput("description"),
         br(),
         a("Learn more at Investopedia.com", href = "https://www.investopedia.com/financial-term-dictionary-4769738")
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
     case_when(
       input$stat == "GDPC1" ~ print("Gross domestic product (GDP) is the total monetary or market value of all the
                                     finished goods and services produced within a country's borders in a specific
                                     time period. As a broad measure of overall domestic production, it functions as
                                     a comprehensive scorecard of a given country’s economic health. The calculation
                                     of a country's GDP encompasses all private and public consumption, government
                                     outlays, investments, additions to private inventories, paid-in construction
                                     costs, and the foreign balance of trade. (Exports are added to the value and
                                     imports are subtracted)."),
       input$stat == "CPIAUCSL" ~ print("The Consumer Price Index (CPI) is a measure that examines the weighted average
                                        of prices of a basket of consumer goods and services, such as transportation,
                                        food, and medical care. It is calculated by taking price changes for each item
                                        in the predetermined basket of goods and averaging them. Changes in the CPI are
                                        used to assess price changes associated with the cost of living. The CPI is one
                                        of the most frequently used statistics for identifying periods of inflation or
                                        deflation. Essentially it attempts to quantify the aggregate price level in an
                                        economy and thus measure the purchasing power of a country's unit of currency.
                                        The weighted average of the prices of goods and services that approximates an
                                        individual's consumption patterns is used to calculate CPI."),
       input$stat == "UNRATE" ~ print("The unemployment rate is the percent of the labor force that is jobless. It is a
                                      lagging indicator, meaning that it generally rises or falls in the wake of changing
                                      economic conditions, rather than anticipating them. When the economy is in poor
                                      shape and jobs are scarce, the unemployment rate can be expected to rise. When the
                                      economy is growing at a healthy rate and jobs are relatively plentiful, it can be
                                      expected to fall. The official unemployment rate is known as U-3. It defines
                                      unemployed people as those who are willing and available to work, and who have
                                      actively sought work within the past four weeks. Those with temporary, part-time,
                                      or full-time jobs are considered employed, as are those who perform at least 15
                                      hours of unpaid family work."),
       input$stat == "ICSA" ~ print("Initial claims are an employment report that measures the number of new jobless
                                    claims filed by individuals seeking to receive unemployment benefits. The report,
                                    published since 1967, also shows how many unemployed individuals qualify for and
                                    are receiving benefits under unemployment insurance. The initial claims number is
                                    watched closely by financial analysts because it provides insight into the health
                                    of the economy. Policy makers use the initial claims figure in conjunction with other
                                    employment data to determine the strength of the labor market."),
       input$stat == "PAYEMS" ~ print("Nonfarm payrolls is the measure of the number of workers in the U.S. excluding
                                      farm workers and workers in a handful of other job classifications. This is measured
                                      by the Bureau of Labor Statistics (BLS), which surveys private and government
                                      entities throughout the U.S. about their payrolls. The BLS reports the nonfarm
                                      payroll numbers to the public on a monthly basis through the closely followed
                                      “Employment Situation” report. In addition to farm workers, nonfarm payrolls data
                                      also excludes some government workers, private households, proprietors, and
                                      non-profit employees."),
       input$stat == "M1" ~ print("M1 is the money supply that is composed of physical currency and coin, demand deposits,
                                  travelers' checks, other checkable deposits, and negotiable order of withdrawal (NOW)
                                  accounts. M1 includes the most liquid portions of the money supply because it contains
                                  currency and assets that either are or can be quickly converted to, cash. M1 does not
                                  include financial assets, such as savings accounts and bonds. M1 money is the money
                                  supply metric most frequently utilized by economists to reference how much money is in
                                  circulation in a country."),
       input$stat == "M2" ~ print("M2 is a calculation of the money supply that includes all elements of M1 as well as
                                  'near money.' M1 includes cash and checking deposits, while near money refers to savings
                                  deposits, money market securities, mutual funds, and other time deposits. These assets
                                  are less liquid than M1 and not as suitable as exchange mediums, but they can be quickly
                                  converted into cash or checking deposits. A consumer or business typically doesn't use
                                  savings deposits and other non-M1 components of M2 when making purchases or paying bills,
                                  but it could convert them to cash in relatively short order."),
       input$stat == "M1V" ~ print("The velocity of money is a measurement of the rate at which money is exchanged in an
                                   economy. It is the number of times that money moves from one entity to another. It also
                                   refers to how much a unit of currency is used in a given period of time. Simply put,
                                   it's the rate at which consumers and businesses in an economy collectively spend money.
                                   The velocity of money is usually measured as a ratio of gross domestic product (GDP)
                                   to a country's M1 or M2 money supply."),
       input$stat == "M2V" ~ print("The velocity of money is a measurement of the rate at which money is exchanged in an
                                   economy. It is the number of times that money moves from one entity to another. It also
                                   refers to how much a unit of currency is used in a given period of time. Simply put,
                                   it's the rate at which consumers and businesses in an economy collectively spend money.
                                   The velocity of money is usually measured as a ratio of gross domestic product (GDP)
                                   to a country's M1 or M2 money supply."),
       input$stat == "TEDRATE" ~ print("The TED spread is the difference between the three-month Treasury bill and the
                                       three-month LIBOR based in US dollars. To put it another way, the TED spread is
                                       the difference between the interest rate on short-term US government debt and the
                                       interest rate on interbank loans. TED is an acronym for Treasury-EuroDollar rate. 
                                       The TED spread is used as an indicator of credit risk. This is because U.S. T-bills
                                       are considered risk free and measure an ultra-safe bet – the U.S. government’s
                                       creditworthiness. In addition, the LIBOR is a dollar-denominated gauge used to
                                       reflect the credit ratings of corporate borrowers or the credit risk that large
                                       international banks assume when they lend money to each other. By comparing the
                                       risk-free rate to any other interest rate, an analyst can determine the perceived
                                       difference in risk."),
       input$stat == "FEDFUNDS" ~ print("The Federal funds rate is the target interest rate set by the FOMC at which
                                        commercial banks borrow and lend their excess reserves to each other overnight. 
                                        By law, banks must maintain a reserve equal to a certain percentage of their
                                        deposits in an account at a Federal Reserve bank. Any money in their reserve
                                        that exceeds the required level is available for lending to other banks that
                                        might have a shortfall."),
       input$stat == "INTDSRUSM193N" ~ print("The discount rate refers to the interest rate charged to the commercial
                                             banks and other financial institutions for the loans they take from the
                                             Federal Reserve Bank through the discount window loan process. While
                                             commercial banks are free to borrow and loan capital among each other
                                             without the need of any collateral using the market-driven interbank
                                             rate, they can also borrow the money for their short term operating
                                             requirements from the Federal Reserve Bank. Such loans are served by the
                                             12 regional branches of the Fed, and the loaned capital is used by the
                                             financial institutes to fulfill for any funding shortfalls, to prevent
                                             any potential liquidity problems or, in the worst-case scenario, to
                                             prevent a bank’s failure."),
       input$stat == "DTB3" ~ print("Treasury yield is the return on investment, expressed as a percentage, on the
                                    U.S. government's debt obligations. Looked at another way, the Treasury yield
                                    is the effective interest rate that the U.S. government pays to borrow money for
                                    different lengths of time. When the U.S. government needs to raise capital to source
                                    projects, such as building new infrastructure, it issues debt instruments through the
                                    U.S. Treasury. The types of debt instruments that the government issues include
                                    Treasury bills (T-bills), Treasury notes (T-notes) and Treasury bonds (T-bonds),
                                    which come in different maturities up to 30 years. The T-bills are short-term bonds
                                    that mature within a year, the T-notes have maturity dates of 10 years or less, and
                                    the T-bonds are long-term bonds that offer maturities of 20 and 30 years."),
       input$stat == "DGS1" ~ print("Treasury yield is the return on investment, expressed as a percentage, on the
                                    U.S. government's debt obligations. Looked at another way, the Treasury yield
                                    is the effective interest rate that the U.S. government pays to borrow money for
                                    different lengths of time. When the U.S. government needs to raise capital to source
                                    projects, such as building new infrastructure, it issues debt instruments through the
                                    U.S. Treasury. The types of debt instruments that the government issues include
                                    Treasury bills (T-bills), Treasury notes (T-notes) and Treasury bonds (T-bonds),
                                    which come in different maturities up to 30 years. The T-bills are short-term bonds
                                    that mature within a year, the T-notes have maturity dates of 10 years or less, and
                                    the T-bonds are long-term bonds that offer maturities of 20 and 30 years."),
       input$stat == "DGS2" ~ print("Treasury yield is the return on investment, expressed as a percentage, on the
                                    U.S. government's debt obligations. Looked at another way, the Treasury yield
                                    is the effective interest rate that the U.S. government pays to borrow money for
                                    different lengths of time. When the U.S. government needs to raise capital to source
                                    projects, such as building new infrastructure, it issues debt instruments through the
                                    U.S. Treasury. The types of debt instruments that the government issues include
                                    Treasury bills (T-bills), Treasury notes (T-notes) and Treasury bonds (T-bonds),
                                    which come in different maturities up to 30 years. The T-bills are short-term bonds
                                    that mature within a year, the T-notes have maturity dates of 10 years or less, and
                                    the T-bonds are long-term bonds that offer maturities of 20 and 30 years."),
       input$stat == "DGS5" ~ print("Treasury yield is the return on investment, expressed as a percentage, on the
                                    U.S. government's debt obligations. Looked at another way, the Treasury yield
                                    is the effective interest rate that the U.S. government pays to borrow money for
                                    different lengths of time. When the U.S. government needs to raise capital to source
                                    projects, such as building new infrastructure, it issues debt instruments through the
                                    U.S. Treasury. The types of debt instruments that the government issues include
                                    Treasury bills (T-bills), Treasury notes (T-notes) and Treasury bonds (T-bonds),
                                    which come in different maturities up to 30 years. The T-bills are short-term bonds
                                    that mature within a year, the T-notes have maturity dates of 10 years or less, and
                                    the T-bonds are long-term bonds that offer maturities of 20 and 30 years."),
       input$stat == "DGS10" ~ print("Treasury yield is the return on investment, expressed as a percentage, on the
                                    U.S. government's debt obligations. Looked at another way, the Treasury yield
                                     is the effective interest rate that the U.S. government pays to borrow money for
                                     different lengths of time. When the U.S. government needs to raise capital to source
                                     projects, such as building new infrastructure, it issues debt instruments through the
                                     U.S. Treasury. The types of debt instruments that the government issues include
                                     Treasury bills (T-bills), Treasury notes (T-notes) and Treasury bonds (T-bonds),
                                     which come in different maturities up to 30 years. The T-bills are short-term bonds
                                     that mature within a year, the T-notes have maturity dates of 10 years or less, and
                                     the T-bonds are long-term bonds that offer maturities of 20 and 30 years."),
       input$stat == "DGS20" ~ print("Treasury yield is the return on investment, expressed as a percentage, on the
                                    U.S. government's debt obligations. Looked at another way, the Treasury yield
                                     is the effective interest rate that the U.S. government pays to borrow money for
                                     different lengths of time. When the U.S. government needs to raise capital to source
                                     projects, such as building new infrastructure, it issues debt instruments through the
                                     U.S. Treasury. The types of debt instruments that the government issues include
                                     Treasury bills (T-bills), Treasury notes (T-notes) and Treasury bonds (T-bonds),
                                     which come in different maturities up to 30 years. The T-bills are short-term bonds
                                     that mature within a year, the T-notes have maturity dates of 10 years or less, and
                                     the T-bonds are long-term bonds that offer maturities of 20 and 30 years."),
       input$stat == "DGS30" ~ print("Treasury yield is the return on investment, expressed as a percentage, on the
                                    U.S. government's debt obligations. Looked at another way, the Treasury yield
                                     is the effective interest rate that the U.S. government pays to borrow money for
                                     different lengths of time. When the U.S. government needs to raise capital to source
                                     projects, such as building new infrastructure, it issues debt instruments through the
                                     U.S. Treasury. The types of debt instruments that the government issues include
                                     Treasury bills (T-bills), Treasury notes (T-notes) and Treasury bonds (T-bonds),
                                     which come in different maturities up to 30 years. The T-bills are short-term bonds
                                     that mature within a year, the T-notes have maturity dates of 10 years or less, and
                                     the T-bonds are long-term bonds that offer maturities of 20 and 30 years."),
       input$stat == "T10Y2Y" ~ print("When the 10-Year minus the 2-Year is negative, it indicates an inverted yield curve.
                                      An inverted yield curve represents a situation in which long-term debt instruments
                                      have lower yields than short-term debt instruments of the same credit quality. The yield
                                      curve is a graphical representation of yields on similar bonds across a variety of
                                      maturities, also known as the term structure of interest rates. A normal yield curve
                                      slopes upward, reflecting the fact that short-term interest rates are usually lower
                                      than long-term rates. That is a result of increased risk and liquidity premiums for
                                      long-term investments. When the yield curve inverts, short-term interest rates become
                                      higher than long-term rates. This type of yield curve is the rarest of the three main
                                      curve types and is considered to be a predictor of economic recession."),
       input$stat == "IITTRHB" ~ print("A tax bracket refers to a range of incomes subject to a certain income tax rate.
                                       Tax brackets result in a progressive tax system, in which taxation progressively
                                       increases as an individual’s income grows: Low incomes fall into tax brackets with
                                       relatively low income tax rates, while higher earnings fall into brackets with
                                       higher rates."),
       input$stat == "IITTRLB" ~ print("A tax bracket refers to a range of incomes subject to a certain income tax rate.
                                       Tax brackets result in a progressive tax system, in which taxation progressively
                                       increases as an individual’s income grows: Low incomes fall into tax brackets with
                                       relatively low income tax rates, while higher earnings fall into brackets with
                                       higher rates.")
     )
   })
}

shinyApp(ui = ui, server = server)
