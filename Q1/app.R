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
  colnames(close_price_df) <- lapply(col_names, function(u) strsplit(u,".",fixed = TRUE)[[1]][1])
  close_price_df[is.na(close_price_df)] <- -999.0
  
  ## correcting for missing - when cbind
  colnames(close_price_df)[which(names(close_price_df) == "BRK")] <- "BRK-B" 
  colnames(close_price_df)[which(names(close_price_df) == "BF")] <- "BF-B" 
  
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
  
  adj_close_price <- format_close_price(stock_data)
  sliced_price_df <- slicing_data(adj_close_price, start_date, end_date)
  sliced_return_df <- sliced_price_df[-1,]/sliced_price_df[1:(dim(sliced_price_df)[1]-1),] - 1
  sliced_return_df[is.na(sliced_return_df)] <- 0
  
  corr <- cor(sliced_return_df)
  
  return(corr)
}

ui <- fluidPage(
  headerPanel('Candlestick Chart and Correlations'),
  sidebarPanel(
    dateInput('start_date','Start Date', min = "1957-01-01", value = "2017-01-01"),
    dateInput('end_date','End Date', max = Sys.Date() - 1, value = "2018-04-24"),

    selectInput('ticker', 'Ticker', tickers),
    textOutput('time_rng'),
    h4(""),
    actionButton('update', 'Update database'),
    hr(),
    code(textOutput('na_ticker'))
  ),
  mainPanel(
    plotOutput('candlestick'),
    hr(),
    h4(strong(textOutput('cov_title'))),
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
  
  output$na_ticker <- renderText({
    if(values$default != 0){
      stock_data <- update_db()
    }
    
    err_tickers <- setdiff(tickers, names(stock_data))
    paste(c("Data not available for following tickers: \n", err_tickers))
  })
  
  output$time_rng <- renderText({
    if(values$default != 0){
      stock_data <- update_db()
    }
    
    start_date <- index(stock_data[[input$ticker]])[1]
    end_date <- tail(index(stock_data[[input$ticker]]),1)
    paste("Range of data available for ", 
          input$ticker, ": ", 
          start_date, " to ", end_date)
  })
  
  output$candlestick <- renderPlot({
    selectedData() %>%
      ggplot(aes(x = date, y = Close)) +
      geom_candlestick(aes(open = Open, high = High, low = Low, close = Close),
                       color_up = "darkgreen", color_down = "darkred", 
                       fill_up  = "darkgreen", fill_down  = "darkred") +
      labs(title =  paste(input$ticker, "Candlestick Chart", sep=" "),
           y = "Closing Price", x = "") +
      theme_tq()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$cov_title <- renderText({
    paste("Top five stocks of the highest return correlation with", input$ticker, sep=" ")
  })
  
  output$corr <- renderPrint({
    
    if(values$default != 0){
      stock_data <- update_db()
    }

    pair_corr <- correlation(stock_data, input$start_date, input$end_date)
    
    validate(
      need(try(isolate(input$ticker) %in% colnames(pair_corr)),
           paste("Price data not available for", input$ticker))
      )
    
    single_corr <- sort(pair_corr[,input$ticker],decreasing = TRUE)
    
    validate(
      need(try(length(single_corr) >= 6),
           paste("Correlation data incomplete or not available for", input$ticker))
    )

    top_5 <- names(single_corr[2:6])

    pair_corr[c(isolate(input$ticker),top_5), c(isolate(input$ticker),top_5)]

  })
}

shinyApp(ui = ui, server = server)
