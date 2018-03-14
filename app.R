#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Import libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(fpp2)
library(readr)

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
ford$Date <- as_datetime(ford$Date)
ford$year <- year(ford$Date)
general_electric$Date <- as_datetime(general_electric$Date)
general_electric$year <- year(general_electric$Date)
microsoft$Date <- as_datetime(microsoft$Date)
microsoft$year <- year(microsoft$Date)
apple$Date <- as_datetime(apple$Date)
apple$year <- year(apple$Date)
facebook$Date <- as_datetime(facebook$Date)
facebook$year <- year(facebook$Date)
bp$Date <- as_datetime(bp$Date)
bp$year <- year(bp$Date)
google$Date <- as_datetime(google$Date)
google$year <- year(google$Date)
coke$Date <- as_datetime(coke$Date)
coke$year <- year(coke$Date)
amazon$Date <- as_datetime(amazon$Date)
amazon$year <- year(amazon$Date)

# The UI is what is displayed in the browser for the shiny application. A fluid page indicates this is one page
ui <- fluidPage(
  # A fluid row represents a single row on the page, conforming to the Bootstrap framework constraints (I think?)
  fluidRow(
    # A column represents one placeholder on the page, and can be 1-12, assuming the dependency on Bootstrap assumption is correct
    column(12,
      # H1 for a specific title
      titlePanel("All Graphs"),
      # The "Enable Globals" checkbox will enable filtering on every single graph on each tab when selected
      checkboxInput("enableGlobal", "Filter All?", FALSE),
      # The "Show all years" checkbox will force all graphs to show all of their data across all years when selected
      checkboxInput("showAllYears", "Show all Years?", FALSE),
      # The "All Years" slider controls the year that is shown if "Enable Globals" is selected above
      sliderInput("allYears",
        "Year",
        min = 2011,
        max = 2018,
        value = 2018,
        sep = ""
      )
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
                sliderInput("btcYear",
                  "Year",
                  min = 2011,
                  max = 2018,
                  value = 2018,
                  sep = ""
                )
              ),
              mainPanel(
                # Plot Output identifies what the server reference will be, and will render the plot here
                plotOutput("btcPlot")
              )
            )
          ),
          # All of the following columns setup different cryptocurrencies, very similar to the above code
          # TODO: Find a clean way to split this out into methods and clean up the code
          column(12,
            titlePanel("Bitcoin Cash"),
            sidebarLayout(
              sidebarPanel(
                sliderInput("btccYear",
                  "Year",
                  min = 2017,
                  max = 2018,
                  value = 2018,
                  sep = "")
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
                sliderInput("ethereumYear",
                  "Year",
                  min = 2015,
                  max = 2018,
                  value = 2018,
                  sep = ""
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
                sliderInput("iotaYear",
                  "Year",
                  min = 2017,
                  max = 2018,
                  value = 2018,
                  sep = ""
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
                sliderInput("litecoinYear",
                  "Year",
                  min = 2013,
                  max = 2018,
                  value = 2018,
                  sep = ""
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
                sliderInput("fordYear",
                  "Year",
                  min = 1977,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("geYear",
                  "Year",
                  min = 1962,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("msftYear",
                  "Year",
                  min = 1986,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("aaplYear",
                  "Year",
                  min = 1984,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("fbYear",
                  "Year",
                  min = 2012,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("googlYear",
                  "Year",
                  min = 2004,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("bpYear",
                  "Year",
                  min = 2005,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("amznYear",
                  "Year",
                  min = 1997,
                  max = 2017,
                  value = 2017,
                  sep = ""
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
                sliderInput("koYear",
                  "Year",
                  min = 1970,
                  max = 2017,
                  value = 2017,
                  sep = ""
                )
              ),
              mainPanel(
                plotOutput("koPlot")
              )
            )
          )
        )
      )
    )
  )
)

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

server <- function(input, output) {
  # Bitcoin plot
  output$btcPlot <- renderPlot({
    # Filter the cryptocurrencies down to only bitcoin and store that
    bitcoin <- filter(crypto, crypto$slug == "bitcoin")
    if(!input$showAllYears) # If the user indicates that want all years to show in graphs don't filter
      # If the user has indicate that they want to globally control all years, show global selection, else select graph specific year
      filter(bitcoin, bitcoin$year == if(input$enableGlobal) input$allYears else input$btcYear)
    # Show a smooth plot of prices for the year and selection above
    ggplot(data = bitcoin) +
      geom_smooth(mapping = aes(x = date, y = high))
  })

  # Bitcoin Cash plot
  output$btccPlot <- renderPlot({
    bitcoinCash <- filter(crypto, crypto$slug == "bitcoin-cash")
    if(!input$showAllYears)
      filter(bitcoinCash, bitcoinCash$year == if(input$enableGlobal) input$allYears else input$btccYear)
    ggplot(data = bitcoinCash) +
      geom_smooth(mapping = aes(x = date, y = high))
  })

  # Ethereum plot
  output$ethereumPlot <-  renderPlot({
    ethereum <- filter(crypto, crypto$slug == "ethereum")
    if(!input$showAllYears)
      filter(ethereum, ethereum$year == if(input$enableGlobal) input$allYears else input$ethereumYear)
    ggplot(data = ethereum) +
      geom_smooth(mapping = aes(x = date, y = high))
  })

  # Iota plot
  output$iotaPlot <-  renderPlot({
    iota <- filter(crypto, crypto$slug == "iota")
    if(!input$showAllYears)
      filter(iota, iota$year == if(input$enableGlobal) input$allYears else input$iotaYear)
    ggplot(data = iota) +
      geom_smooth(mapping = aes(x = date, y = high))
  })

  # Litecoin plot
  output$litecoinPlot <- renderPlot({
    litecoin <- filter(crypto, crypto$slug == "litecoin")
    if(!input$showAllYears)
      filter(litecoin, litecoin$year == if(input$enableGlobal) input$allYears else input$litecoinYear)
    ggplot(data = litecoin) +
      geom_smooth(mapping = aes(x = date, y = high))
  })

  # Ford plot
  output$fordPlot <- renderPlot({
    if(!input$showAllYears)
      ford <- filter(ford, ford$year == if(input$enableGlobal) input$allYears else input$fordYear)
    ggplot(data = ford) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # General Electric plot
  output$gePlot <- renderPlot({
    if(!input$showAllYears)
      general_electric <- filter(general_electric, general_electric$year == if(input$enableGlobal) input$allYears else input$geYear)
    ggplot(data = general_electric) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # Microsoft plot
  output$msftPlot <-  renderPlot({
    if(!input$showAllYears)
      microsoft <- filter(microsoft, microsoft$year == if(input$enableGlobal) input$allYears else input$msftYear)
    ggplot(data = microsoft) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # Apple plot
  output$aaplPlot <-  renderPlot({
    if(!input$showAllYears)
      apple <- filter(apple, apple$year == if(input$enableGlobal) input$allYears else input$aaplYear)
    ggplot(data = apple) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # Facebook plot
  output$fbPlot <- renderPlot({
    if(!input$showAllYears)
      facebook <- filter(facebook, facebook$year == if(input$enableGlobal) input$allYears else input$fbYear)
    ggplot(data = facebook) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # Google plot
  output$googlPlot <- renderPlot({
    if(!input$showAllYears)
      google <- filter(google, google$year == if(input$enableGlobal) input$allYears else input$googlYear)
    ggplot(data = google) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # BP plot
  output$bpPlot <- renderPlot({
    if(!input$showAllYears)
      bp <- filter(bp, bp$year == if(input$enableGlobal) input$allYears else input$bpYear)
    ggplot(data = bp) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # Amazon plot
  output$amznPlot <- renderPlot({
    if(!input$showAllYears)
      amazon <- filter(amazon, amazon$year == if(input$enableGlobal) input$allYears else input$amznYear)
    ggplot(data = amazon) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })

  # Coca-Cola plot
  output$koPlot <- renderPlot({
    if(!input$showAllYears)
      coke <- filter(coke, coke$year == if(input$enableGlobal) input$allYears else input$koYear)
    ggplot(data = coke) +
      geom_smooth(mapping = aes(x = Date, y = High))
  })
}

# Finally, run the shiny app which will startup a server that will host the computation for the graphs, 
# and a UI which shows the view to the users
shinyApp(ui, server)
