library(shiny)

tickers <- sp_symbols()

ui <- fluidPage(
  headerPanel('Candlestick Chart and Correlations'),
  sidebarPanel(
    dateInput('start_date','Start Date', min = "1957-01-01", value = "2018-01-01"),
    dateInput('end_date','End Date', max = Sys.Date() - 1, value = "2018-04-24"),
    
    selectInput('ticker', 'Ticker', tickers),
    textOutput('time_rng'),
    h4(""),
    hr(),
    actionButton('update', 'Update database'),
    h6("Update database allows users to expand current database up to the time range selected. May take a while."),
    hr(),
    code(textOutput('na_ticker'))
    
    ,width = 3
  ),
  mainPanel(
    plotOutput('candlestick'),
    hr(),
    h4(strong(textOutput('cov_title'))),
    verbatimTextOutput('corr')
    
    ,width = 9
  )
)