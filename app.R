# Please note that if you are not running this in R Studio's ShinyWebApp environment, you must uncomment
# the three lines at the bottom of this file and change the associated IP address to your own computer
# in order to properly run the application and view it.

# Import libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(fpp2)
library(readr)
library(forecast)
library(shinyjs)

# Import data
crypto <- read_csv("./data/crypto/crypto-markets.csv")
ford <- read_csv("./data/stocks/f.us.txt")
general_electric <- read_csv("./data/stocks/ge.us.txt")
microsoft <- read_csv("./data/stocks/msft.us.txt")
apple <- read_csv("./data/stocks/aapl.us.txt")
facebook <- read_csv("./data/stocks/fb.us.txt")
google <- read_csv("./data/stocks/googl.us.txt")
bp <- read_csv("./data/stocks/bp.us.txt")
amazon <- read_csv("./data/stocks/amzn.us.txt")
coke <- read_csv("./data/stocks/ko.us.txt")

# Clean data and add necessary information to tables for easier manipulation later on
crypto$date <- as.Date(crypto$date)
crypto$year <- year(crypto$date)
ford$Date <- as.Date(ford$Date)
ford$year <- year(ford$Date)
ford$title <- "ford"
general_electric$Date <- as.Date(general_electric$Date)
general_electric$year <- year(general_electric$Date)
general_electric$title <- "general electric"
microsoft$Date <- as.Date(microsoft$Date)
microsoft$year <- year(microsoft$Date)
microsoft$title <- "microsoft"
apple$Date <- as.Date(apple$Date)
apple$year <- year(apple$Date)
apple$title <- "apple"
facebook$Date <- as.Date(facebook$Date)
facebook$year <- year(facebook$Date)
facebook$title <- "facebook"
bp$Date <- as.Date(bp$Date)
bp$year <- year(bp$Date)
bp$title <- "bp"
google$Date <- as.Date(google$Date)
google$year <- year(google$Date)
google$title <- "google"
coke$Date <- as.Date(coke$Date)
coke$year <- year(coke$Date)
coke$title <- "coke"
amazon$Date <- as.Date(amazon$Date)
amazon$year <- year(amazon$Date)
amazon$title <- "amazon"

### Forecasting Models ###
bitcoin <- filter(crypto, crypto$slug == "bitcoin")
bitcoin_clean <- bitcoin[9]
bitcoin_subset <- ts(bitcoin_clean, start = c(2013, 4, 28), frequency = 365)
bitcoin_train <- subset(bitcoin_subset, end = 1690)
bitcoin_forecast <- ses(bitcoin_train, h = 31)
ford_clean <- ford[5]
ford_subset <- ts(ford_clean, start = c(1990, 1, 3), frequency = 260)
ford_forecast <- ses(ford_subset, h = 180)
microsoft_clean <- microsoft[5]
microsoft_subset <- ts(microsoft_clean, start = c(1986, 3, 13), frequency = 260)
microsoft_forecast <- ses(microsoft_subset, h = 180)

app <- shinyApp(
  # The UI is what is displayed in the browser for the shiny application. A fluid page indicates this is one page
  ui = fluidPage(
    useShinyjs(),
    # A fluid row represents a single row on the page, conforming to the Bootstrap framework constraints (I think?)
    fluidRow(
      # A column represents one placeholder on the page, and can be 1-12, assuming the dependency on Bootstrap assumption is correct
      column(12,
        # H1 for a specific title
        titlePanel("All Graphs"),
        # The "Show all years" checkbox will force all graphs to show all of their data across all years when selected
        checkboxInput("showAllYears", "Show all Years?", FALSE),
        # The "show Forecasts" checkbox will show some forecasts for some Stocks and Cryptos that have been created
        checkboxInput("showForecasts", "Show Forecasts for some Graphs?", FALSE),
        # The "Enable Globals" checkbox will enable filtering on every single graph on each tab when selected
        checkboxInput("enableGlobal", "Filter All at Once? (Use slider below)", FALSE),
        # The "All Years" slider controls the year that is shown if "Enable Globals" is selected above
        sliderInput("allYears", "Year", min = 2013, max = 2018, value = 2018, sep = "")
      )
    ),
    # Horizontal Rule, creates an HTML HR to split up the page nicely
    hr(),
    # Main panel represents a panel on the page (or the body?)
    mainPanel(
      # Tabset Panel creates clickable bootstrap panels on the page, for different embedded views
      tabsetPanel(type = "tabs",
        # Cryptocurrencies tab will be used to show historic cryptocurrency prices
        tabPanel("Cryptocurrencies",
          fluidRow(
            column(12,
              # Create the first panel for Bitcoin
              titlePanel("Bitcoin"),
              sidebarLayout(
                sidebarPanel(
                  # Create a slider for all years in which there is data. 
                  # N.B. sep = "" removes the `.` that gets added to the slider values
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("btcYear", "Year", min = 2011, max = 2018, value = 2018, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  # Plot Output identifies what the server reference will be, and will render the plot here
                  plotOutput("btcPlot")
                )
              )
            ),
            # All of the following columns setup different cryptocurrencies, very similar to the above code
            column(12,
              titlePanel("Bitcoin Cash"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("btccYear", "Year", min = 2017, max = 2018, value = 2018, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ), 
                mainPanel(
                  plotOutput("btccPlot")
                )
              )
            ),
            column(12,
              titlePanel("Ethereum"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("ethereumYear", "Year", min = 2015, max = 2018, value = 2018, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("ethereumPlot")
                )
              )
            ),
            column(12,
              titlePanel("Iota"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("iotaYear", "Year", min = 2017, max = 2018, value = 2018, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("iotaPlot")
                )
              )
            ),
            column(12,
              titlePanel("Litecoin"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("litecoinYear", "Year", min = 2013, max = 2018, value = 2018, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("litecoinPlot")
                )
              )
            )
          )
        ),
        # Create another tab that will show stock values
        tabPanel("Stocks",
          fluidRow(
            column(12,
              # Create the first stock plot, exactly like the cryptocurrency one above
              titlePanel("Ford"),
              sidebarLayout(
                sidebarPanel(
                  # Here the Years go back a bit further as the stock prices have been around much longer
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("fordYear", "Year", min = 1977, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("fordPlot")
                )
              )
            ),
            column(12,
              titlePanel("General Electric"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("geYear", "Year", min = 1962, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ), 
                mainPanel(
                  plotOutput("gePlot")
                )
              )
            ),
            column(12,
              titlePanel("Microsoft"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("msftYear", "Year", min = 1986, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("msftPlot")
                )
              )
            ),
            column(12,
              titlePanel("Apple"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("aaplYear", "Year", min = 1984, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("aaplPlot")
                )
              )
            ),
            column(12,
              titlePanel("Facebook"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("fbYear", "Year", min = 2012, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("fbPlot")
                )
              )
            ),
            column(12,
              titlePanel("Google"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("googlYear", "Year", min = 2004, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("googlPlot")
                )
              )
            ),
            column(12,
              titlePanel("British Petroleum (BP)"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("bpYear", "Year", min = 2005, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("bpPlot")
                )
              )
            ),
            column(12,
              titlePanel("Amazon"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("amznYear", "Year", min = 1997, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("amznPlot")
                )
              )
            ),
            column(12,
              titlePanel("Coca-Cola"),
              sidebarLayout(
                sidebarPanel(
                  conditionalPanel(
                    condition = "input.showAllYears == false",
                    sliderInput("koYear", "Year", min = 1970, max = 2017, value = 2017, sep = "", round = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.showAllYears == true",
                    p("You have enabled 'Show All Years', and therefore there is no filtering to do.")
                  )
                ),
                mainPanel(
                  plotOutput("koPlot")
                )
              )
            )
          )
        ),
        tabPanel("Comparisons - Crypto v. Stock",
           fluidRow(
             column(12,
                titlePanel("Bitcoin vs. Apple - (Bitcoin scaled to 1/10)"),
                mainPanel(
                  plotOutput("btcVapple")
                )
             ),
             column(12,
                titlePanel("Ethereum vs. Apple"),
                mainPanel(
                  plotOutput("ethVapple")
                )
             ),
             column(12,
                titlePanel("Ethereum vs. Facebook"),
                mainPanel(
                  plotOutput("ethVfb")
                )
             ),
             column(12,
                titlePanel("Iota vs. General Electric - (General Electric scaled to 1/10)"),
                mainPanel(
                  plotOutput("iotaVge")
                )
             ),
             column(12,
                titlePanel("Bitcoin vs. Google"),
                mainPanel(
                  plotOutput("btcVgoogle")
                )
             )
           )
        ),
        tabPanel("Comparisons - Stock v. Stock",
           fluidRow(
             column(12,
                    titlePanel("All Companies"),
                    mainPanel(
                      plotOutput("allCompanies")
                    )
             ),
             column(12,
                    titlePanel("Tech Giants"),
                    mainPanel(
                      plotOutput("techGiants")
                    )
             ),
             column(12,
                    titlePanel("Non-Tech Giants"),
                    mainPanel(
                      plotOutput("nonTech")
                    )
             ),
             column(12,
                    titlePanel("Amazon vs. Google"),
                    mainPanel(
                      plotOutput("amznVgoogle")
                    )
             ),
             column(12,
                    titlePanel("Apple vs. Google"),
                    mainPanel(
                      plotOutput("appleVgoogle")
                    )
             ),
             column(12,
                    titlePanel("Apple vs. Facebook"),
                    mainPanel(
                      plotOutput("appleVfb")
                    )
             )
           )
        ),
        tabPanel("Comparisons - Crypto v. Crypto",
           fluidRow(
             column(12,
                titlePanel("All Crypto (No Bitcoin)"),
                mainPanel(
                  plotOutput("allCrypto")
                )
             ),
             column(12,
                titlePanel("Ethereum vs. Litecoin - (Ethereum scaled to 1/10)"),
                mainPanel(
                  plotOutput("ethVltc")
                )
             ),
             column(12,
                titlePanel("Ethereum vs. Iota"),
                mainPanel(
                  plotOutput("ethViota")
                )
             ),
             column(12,
                titlePanel("Ripple vs. Iota"),
                mainPanel(
                  plotOutput("rippleViota")
                )
             )
           )
        )
      )
    )
  ),
  
  #
  # The server is what renders out the graphs and computes the logic behind them all. 
  # For each plot the same effect is happening.
  # The first plot is documented to explain what each row of code is doing.
  # All graphs share a similar code structure
  #
  #-------------------------------------------------------------------------------------|
  # TODO: Identify how to split these into functions. Many scopes are dealt with in     |
  # each code block so it will not be easy to decouple the code                         |
  #-------------------------------------------------------------------------------------|
  
  server = function(input, output) {
    # Bitcoin plot
    output$btcPlot <- renderPlot({
      # Filter the cryptocurrencies down to only bitcoin and store that
      bitcoin <- filter(crypto, crypto$slug == "bitcoin")
      if(!input$showAllYears) # If the user indicates that want all years to show in graphs don't filter
        # If the user has indicate that they want to globally control all years, show global selection, else select graph specific year
        filter(bitcoin, bitcoin$year == if(input$enableGlobal) input$allYears else input$btcYear)
      # Either show a chart with forecasted prices, or show a smooth plot of prices for the year and selection above
      if(input$showForecasts)
        autoplot(bitcoin_forecast)
      else {
        ggplot(data = bitcoin) +
          geom_smooth(mapping = aes(x = date, y = close))
      }
    })
  
    # Bitcoin Cash plot
    output$btccPlot <- renderPlot({
      bitcoinCash <- filter(crypto, crypto$slug == "bitcoin-cash")
      if(!input$showAllYears)
        filter(bitcoinCash, bitcoinCash$year == if(input$enableGlobal) input$allYears else input$btccYear)
      ggplot(data = bitcoinCash) +
        geom_smooth(mapping = aes(x = date, y = close))
    })
  
    # Ethereum plot
    output$ethereumPlot <-  renderPlot({
      ethereum <- filter(crypto, crypto$slug == "ethereum")
      if(!input$showAllYears)
        filter(ethereum, ethereum$year == if(input$enableGlobal) input$allYears else input$ethereumYear)
      ggplot(data = ethereum) +
        geom_smooth(mapping = aes(x = date, y = close))
    })
  
    # Iota plot
    output$iotaPlot <-  renderPlot({
      iota <- filter(crypto, crypto$slug == "iota")
      if(!input$showAllYears)
        filter(iota, iota$year == if(input$enableGlobal) input$allYears else input$iotaYear)
      ggplot(data = iota) +
        geom_smooth(mapping = aes(x = date, y = close))
    })
  
    # Litecoin plot
    output$litecoinPlot <- renderPlot({
      litecoin <- filter(crypto, crypto$slug == "litecoin")
      if(!input$showAllYears)
        filter(litecoin, litecoin$year == if(input$enableGlobal) input$allYears else input$litecoinYear)
      ggplot(data = litecoin) +
        geom_smooth(mapping = aes(x = date, y = close))
    })
  
    # Ford plot
    output$fordPlot <- renderPlot({
      if(!input$showAllYears)
        ford <- filter(ford, ford$year == if(input$enableGlobal) input$allYears else input$fordYear)
      if(input$showForecasts)
        autoplot(ford_forecast)
      else {
        ggplot(data = ford) +
          geom_smooth(mapping = aes(x = Date, y = Close))
      }
    })
  
    # General Electric plot
    output$gePlot <- renderPlot({
      if(!input$showAllYears)
        general_electric <- filter(general_electric, general_electric$year == if(input$enableGlobal) input$allYears else input$geYear)
      ggplot(data = general_electric) +
        geom_smooth(mapping = aes(x = Date, y = Close))
    })
  
    # Microsoft plot
    output$msftPlot <-  renderPlot({
      if(!input$showAllYears)
        microsoft <- filter(microsoft, microsoft$year == if(input$enableGlobal) input$allYears else input$msftYear)
      if(input$showForecasts)
        autoplot(microsoft_forecast)
      else {
        ggplot(data = microsoft) +
          geom_smooth(mapping = aes(x = Date, y = Close))
      }
    })
  
    # Apple plot
    output$aaplPlot <-  renderPlot({
      if(!input$showAllYears)
        apple <- filter(apple, apple$year == if(input$enableGlobal) input$allYears else input$aaplYear)
      ggplot(data = apple) +
        geom_smooth(mapping = aes(x = Date, y = Close))
    })
  
    # Facebook plot
    output$fbPlot <- renderPlot({
      if(!input$showAllYears)
        facebook <- filter(facebook, facebook$year == if(input$enableGlobal) input$allYears else input$fbYear)
      ggplot(data = facebook) +
        geom_smooth(mapping = aes(x = Date, y = Close))
    })
  
    # Google plot
    output$googlPlot <- renderPlot({
      if(!input$showAllYears)
        google <- filter(google, google$year == if(input$enableGlobal) input$allYears else input$googlYear)
      ggplot(data = google) +
        geom_smooth(mapping = aes(x = Date, y = Close))
    })
  
    # BP plot
    output$bpPlot <- renderPlot({
      if(!input$showAllYears)
        bp <- filter(bp, bp$year == if(input$enableGlobal) input$allYears else input$bpYear)
      ggplot(data = bp) +
        geom_smooth(mapping = aes(x = Date, y = Close))
    })
  
    # Amazon plot
    output$amznPlot <- renderPlot({
      if(!input$showAllYears)
        amazon <- filter(amazon, amazon$year == if(input$enableGlobal) input$allYears else input$amznYear)
      ggplot(data = amazon) +
        geom_smooth(mapping = aes(x = Date, y = Close))
    })
  
    # Coca-Cola plot
    output$koPlot <- renderPlot({
      if(!input$showAllYears)
        coke <- filter(coke, coke$year == if(input$enableGlobal) input$allYears else input$koYear)
      ggplot(data = coke) +
        geom_smooth(mapping = aes(x = Date, y = Close))
    })
    
    output$btcVapple <- renderPlot({
      bitcoin <- filter(crypto, crypto$slug == "bitcoin")
      bitcoin$date <- as.Date(bitcoin$date)
      apple$Date <- as.Date(apple$Date)
      apple <- filter(apple, apple$Date > "2013-04-28")
      apple <- filter(apple, apple$Date < "2017-06-01")
      bitcoin <- filter(bitcoin, bitcoin$date > "2013-04-28")
      bitcoin <- filter(bitcoin, bitcoin$date < "2017-06-01")
      bitcoin$close <- bitcoin$close / 10
      ggplot() + 
        geom_smooth(data = apple, mapping=aes(x = Date, y = Close, color = "Apple")) + 
        geom_smooth(data = bitcoin, mapping=aes(x = date, y = close, color = "Bitcoin")) +
        scale_x_date()
    })
    
    output$ethVfb <- renderPlot({
      ethereum <- filter(crypto, crypto$slug == "ethereum")
      ethereum$date <- as.Date(ethereum$date)
      facebook$Date <- as.Date(facebook$Date)
      facebook <- filter(facebook, facebook$Date > "2015-08-07")
      facebook <- filter(facebook, facebook$Date < "2017-06-01")
      ethereum <- filter(ethereum, ethereum$date > "2015-08-07")
      ethereum <- filter(ethereum, ethereum$date < "2017-06-01")
      ggplot() + 
        geom_smooth(data = facebook, mapping=aes(x = Date, y = Close, color = "Facebook")) + 
        geom_smooth(data = ethereum, mapping=aes(x = date, y = close, color = "Ethereum")) +
        scale_x_date()
    })
    
    output$ethVapple <- renderPlot({
      ethereum <- filter(crypto, crypto$slug == "ethereum")
      ethereum$date <- as.Date(ethereum$date)
      apple$Date <- as.Date(apple$Date)
      apple <- filter(apple, apple$Date > "2015-08-07")
      apple <- filter(apple, apple$Date < "2017-06-01")
      ethereum <- filter(ethereum, ethereum$date > "2015-08-07")
      ethereum <- filter(ethereum, ethereum$date < "2017-06-01")
      ggplot() + 
        geom_smooth(data = apple, mapping=aes(x = Date, y = Close, color = "Apple")) + 
        geom_smooth(data = ethereum, mapping=aes(x = date, y = close, color = "Ethereum")) +
        scale_x_date()
    })
    
    output$iotaVge <- renderPlot({
      iota <- filter(crypto, crypto$slug == "iota")
      iota$date <- as.Date(iota$date)
      general_electric$Date <- as.Date(general_electric$Date)
      general_electric <- filter(general_electric, general_electric$Date > "2017-06-13")
      general_electric <- filter(general_electric, general_electric$Date < "2017-11-10")
      iota <- filter(iota, iota$date > "2017-06-13")
      iota <- filter(iota, iota$date < "2017-11-10")
      general_electric$Close <- general_electric$Close / 10
      ggplot() + 
        geom_smooth(data = general_electric, mapping=aes(x = Date, y = Close, color = "General Electric")) + 
        geom_smooth(data = iota, mapping=aes(x = date, y = close, color = "IOTA")) +
        scale_x_date()
    })
    
    output$btcVgoogle <- renderPlot({
      bitcoin <- filter(crypto, crypto$slug == "bitcoin")
      bitcoin$date <- as.Date(bitcoin$date)
      google$Date <- as.Date(google$Date)
      google <- filter(google, google$Date > "2013-04-28")
      google <- filter(google, google$Date < "2017-11-10")
      bitcoin <- filter(bitcoin, bitcoin$date > "2013-04-28")
      bitcoin <- filter(bitcoin, bitcoin$date < "2017-11-10")
      ggplot() + 
        geom_smooth(data = google, mapping=aes(x = Date, y = Close, color = "Google")) + 
        geom_smooth(data = bitcoin, mapping=aes(x = date, y = close, color = "Bitcoin")) +
        scale_x_date()
    })
    
    output$ethVltc <- renderPlot({
      litecoin <- filter(crypto, crypto$slug == "litecoin")
      litecoin$date <- as.Date(litecoin$date)
      ethereum <- filter(crypto, crypto$slug == "ethereum")
      ethereum$date <- as.Date(ethereum$date)
      ethereum <- filter(ethereum, ethereum$date > "2016-12-21")
      ethereum <- filter(ethereum, ethereum$date < "2018-02-05")
      litecoin <- filter(litecoin, litecoin$date > "2016-12-21")
      litecoin <- filter(litecoin, litecoin$date < "2018-02-05")
      ethereum$close <- ethereum$close / 10
      ggplot() + 
        geom_smooth(data = ethereum, mapping=aes(x = date, y = close, color = "Ethereum")) + 
        geom_smooth(data = litecoin, mapping=aes(x = date, y = close, color = "Litecoin")) +
        scale_x_date()
    })
    
    output$amznVgoogle <- renderPlot({
      google$Date <- as.Date(google$Date)
      google <- filter(google, google$Date > "2004-08-19")
      google <- filter(google, google$Date < "2017-11-10")
      amazon$Date <- as.Date(amazon$Date)
      amazon <- filter(amazon, amazon$Date > "2004-08-19")
      amazon <- filter(amazon, amazon$Date < "2017-11-10")
      ggplot() + 
        geom_smooth(data = amazon, mapping=aes(x = Date, y = Close, color = "Amazon")) + 
        geom_smooth(data = google, mapping=aes(x = Date, y = Close, color = "Google")) +
        scale_x_date()
    })
    
    output$ethViota <- renderPlot({
      iota <- filter(crypto, crypto$slug == "iota")
      iota$date <- as.Date(iota$date)
      ethereum <- filter(crypto, crypto$slug == "ethereum")
      ethereum$date <- as.Date(ethereum$date)
      ethereum <- filter(ethereum, ethereum$date > "2016-12-21")
      ethereum <- filter(ethereum, ethereum$date < "2018-02-05")
      iota <- filter(iota, iota$date > "2016-12-21")
      iota <- filter(iota, iota$date < "2018-02-05")
      ethereum$close <- ethereum$close / 10
      ggplot() + 
        geom_smooth(data = ethereum, mapping=aes(x = date, y = close, color = "Ethereum")) + 
        geom_smooth(data = iota, mapping=aes(x = date, y = close, color = "Iota")) +
        scale_x_date()
    })
    
    output$appleVgoogle <- renderPlot({
      google$Date <- as.Date(google$Date)
      google <- filter(google, google$Date > "2004-08-19")
      google <- filter(google, google$Date < "2017-11-10")
      apple$Date <- as.Date(apple$Date)
      apple <- filter(apple, apple$Date > "2004-08-19")
      apple <- filter(apple, apple$Date < "2017-11-10")
      ggplot() + 
        geom_smooth(data = apple, mapping=aes(x = Date, y = Close, color = "Apple")) + 
        geom_smooth(data = google, mapping=aes(x = Date, y = Close, color = "Google")) +
        scale_x_date()
    })
    
    output$rippleViota <- renderPlot({
      iota <- filter(crypto, crypto$slug == "iota")
      iota$date <- as.Date(iota$date)
      ripple <- filter(crypto, crypto$slug == "ripple")
      ripple$date <- as.Date(ripple$date)
      ripple <- filter(ripple, ripple$date > "2017-06-13")
      ripple <- filter(ripple, ripple$date < "2018-02-05")
      iota <- filter(iota, iota$date > "2017-06-13")
      iota <- filter(iota, iota$date < "2018-02-05")
      ggplot() + 
        geom_smooth(data = ripple, mapping=aes(x = date, y = close, color = "Ripple")) + 
        geom_smooth(data = iota, mapping=aes(x = date, y = close, color = "Iota")) +
        scale_x_date()
    })
    
    output$allCrypto <- renderPlot({
      crypto <- filter(crypto, slug == "iota" | slug == "monero" | slug == "bitcoin-cash" | slug == "bitconnect" | 
                         slug == "litecoin" | slug == "ethereum" | slug == "ripple")
      ggplot() + 
        geom_smooth(data = crypto, mapping=aes(x = date, y = close, color = slug))
    })
    
    output$allCompanies <- renderPlot({
      coke <- filter(coke, Date > "1985-01-01")
      general_electric <- filter(general_electric, Date > "1985-01-01")
      ford <- filter(ford, Date > "1985-01-01")
      ggplot(mapping = aes(Date, Close, color = title)) + geom_smooth(data = ford) + 
        geom_smooth(data = general_electric) + geom_smooth(data = microsoft) + geom_smooth(data = apple) + 
        geom_smooth(data = google) + geom_smooth(data = facebook) + geom_smooth(data = amazon) + 
        geom_smooth(data = coke) + geom_smooth(data = bp) + scale_x_date()
    })
    
    output$techGiants <- renderPlot({
      apple <- filter(apple, Date > "1995-01-01")
      microsoft <- filter(microsoft, Date > "1995-01-01")
      ggplot(mapping = aes(Date, Close, color = title)) + geom_smooth(data = microsoft) + geom_smooth(data = apple) + 
        geom_smooth(data = google) + geom_smooth(data = facebook) + geom_smooth(data = amazon) + scale_x_date()
    })
    
    output$nonTech <- renderPlot({
      coke <- filter(coke, Date > "1985-01-01")
      general_electric <- filter(general_electric, Date > "1985-01-01")
      ford <- filter(ford, Date > "1985-01-01")
      ggplot(mapping = aes(Date, Close, color = title)) + geom_smooth(data = ford) + 
        geom_smooth(data = general_electric) + geom_smooth(data = coke) + geom_smooth(data = bp) + scale_x_date()
    })
    
    output$appleVfb <- renderPlot({
      facebook$Date <- as.Date(facebook$Date)
      facebook <- filter(facebook, facebook$Date > "2004-08-19")
      facebook <- filter(facebook, facebook$Date < "2017-11-10")
      apple$Date <- as.Date(apple$Date)
      apple <- filter(apple, apple$Date > "2004-08-19")
      apple <- filter(apple, apple$Date < "2017-11-10")
      ggplot() + 
        geom_smooth(data = apple, mapping=aes(x = Date, y = Close, color = "Apple")) + 
        geom_smooth(data = facebook, mapping=aes(x = Date, y = Close, color = "Facebook")) +
        scale_x_date()
    })
  }
)

# Finally, run the shiny app which will startup a server that will host the computation for the graphs, 
# and a UI which shows the view to the users
#options(shiny.port = 7775)
#options(shiny.host = "192.168.1.117")
#runApp(app)
