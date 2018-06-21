library(shiny)
library(xts)
library(dplyr)
source("data_formatting.R")

tickers <- sp_symbols()
stock_data <- readRDS(DB_NAME)

server <- function(input, output) {
  
  values <- reactiveValues(default = 0)
  
  observeEvent(input$update,{
    #' to allow for default result display at initialization
    values$default <- input$update
  })
  
  selectedData <- reactive({
    #' selecting time series of ticker within start_date and end_date
    
    #to allow for default result display at initialization
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
    #' expand on the current database up to the time frame selected
    
    update_all_stocks(syms = tickers, 
                      existing_data = stock_data, 
                      start_date = input$start_date, 
                      end_date = input$end_date)
  })
  
  output$na_ticker <- renderText({
    #' display tickers with no data
    
    #to allow for default result display at initialization
    if(values$default != 0){
      stock_data <- update_db()
    }
    
    err_tickers <- setdiff(tickers, names(stock_data))
    paste(c("Data not available for following tickers: \n", err_tickers))
  })
  
  output$time_rng <- renderText({
    #' display range of data available for a particular ticker
    
    #to allow for default result display at initialization
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
    #' display candlestick chart
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
    #' Calculate top five correlation and display the result
    
    # to allow for default result display at initialization
    if(values$default != 0){
      stock_data <- update_db()
    }
    
    pair_corr <- correlation(stock_data, input$start_date, input$end_date)
    
    # if ticker is not in the correlation matrix, display warning
    validate(
      need(try(isolate(input$ticker) %in% colnames(pair_corr)),
           paste("Price data not available for", input$ticker))
    )
    
    single_corr <- sort(pair_corr[,input$ticker],decreasing = TRUE)
    
    # if ticker has imcomplete data to calculate correlation, display warning
    # happened for BKNG, WELL
    validate(
      need(try(length(single_corr) >= 6),
           paste("Correlation data incomplete or not available for", input$ticker))
    )
    
    top_5 <- names(single_corr[2:6])
    
    pair_corr[c(isolate(input$ticker),top_5), c(isolate(input$ticker),top_5)]
    
  })
}