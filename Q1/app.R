
palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(xts)
library(dplyr)
source("data_import.R")

tickers <- sp_symbols()
stock_data <- readRDS(DB_NAME)
# tickers <- names(stock_data) ## for testing only

format_close_price <- function(stock_data = stock_data){
  
  df <- as.data.frame(do.call(cbind,stock_data))
  
  close_price_df <- df[grepl('Adjusted{1}$',colnames(df))]
  col_names <- colnames(close_price_df)
  # colnames(close_price_df) <- strsplit(colnames(close_price_df),".",fixed = TRUE)[[1]][1]
  colnames(close_price_df) <- lapply(col_names, function(u) strsplit(u,".",fixed = TRUE)[[1]][1])
  close_price_df[is.na(close_price_df)] <- 0
  
  return(close_price_df)
}

slicing_data <- function(df, start_date, end_date){
  
  time_frame <- rownames(df)
  biz_start <- time_frame[time_frame >= start_date][1]
  biz_end <- tail(time_frame[time_frame <= end_date],1)
  
  biz_start_idx <- which(time_frame == biz_start)
  biz_end_idx <- which(time_frame == biz_end)
  
  return(as.data.frame(t(t(df)[, biz_start_idx:biz_end_idx])))
  
}

correlation <- function(stock_data, start_date, end_date){
  
  clean_close_price <- format_close_price(stock_data)
  sliced_price_df <- slicing_data(clean_close_price, start_date, end_date)
  sliced_return_df <- sliced_price_df[-1,]/sliced_price_df[1:(dim(sliced_price_df)[1]-1),] - 1
  corr <- cor(sliced_return_df)
  
  return(corr)
}

ui <- fluidPage(
  headerPanel('Candlestick chart and correlations'),
  sidebarPanel(
    dateInput('start_date','Start Date', min = "1957-01-01", value = "2017-01-01"),
    dateInput('end_date','End Date', max = Sys.Date() - 1, value = "2018-04-24"),

    selectInput('ticker', 'Ticker', tickers),
    textOutput('time_rng'),
    h4(""),
    actionButton('update', 'Update database')
  ),
  mainPanel(
    plotOutput('candlestick'),
    textOutput('cov_title'),
    verbatimTextOutput('corr')
  )
)

server <- function(input, output) {
  
  values <- reactiveValues(default = 0)
  
  observeEvent(input$update,{
    values$default <- input$update
  })
  
  selectedData <- reactive({
    
    if(values$default != 0){
      stock_data <- update_db()
    }
    
    df <- as.data.frame(stock_data[[input$ticker]])
    sliced_df <- slicing_data(df, input$start_date, input$end_date)
    col_names <- colnames(sliced_df)
    colnames(sliced_df) <- lapply(col_names, function(u) strsplit(u,".",fixed = TRUE)[[1]][2])
    sliced_df$date <- rownames(sliced_df)
    final_data <- sliced_df
  })
  
  update_db <- eventReactive(input$update,{
    update_all_stocks(syms = tickers, 
                      existing_data = stock_data, 
                      start_date = input$start_date, 
                      end_date = input$end_date)
    })
  
  output$time_rng <- renderText({
    start_date <- rownames(selectedData())[1]
    end_date <- tail(rownames(selectedData()),1)
    paste("Range of data available for ", input$ticker, ": ", start_date, " to ", end_date)
  })
  
  output$candlestick <- renderPlot({
    selectedData() %>%
      ggplot(aes(x = date, y = Close)) +
      geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
      labs(title =  paste(input$ticker, "Candlestick Chart", sep=" "),
           y = "Closing Price", x = "") +
        theme_tq()
  })
  
  output$cov_title <- renderText({
    paste("Top five stocks of the highest return correlation with", input$ticker, sep=" ")
  })
  
  output$corr <- renderPrint({
    if(values$default != 0){
      stock_data <- update_db()
    }

    pair_corr <- correlation(stock_data, input$start_date, input$end_date)
    single_corr <- pair_corr[,input$ticker]
    top_5 <- names(sort(single_corr,decreasing = TRUE)[2:6])
    
    pair_corr[c(isolate(input$ticker),top_5), c(isolate(input$ticker),top_5)]
    
  })

}

shinyApp(ui = ui, server = server)
