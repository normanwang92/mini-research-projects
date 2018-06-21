library(shiny)
library(xts)
library(dplyr)
source("data_import.R")

format_close_price <- function(stock_data = stock_data){
  #' formatting adjusted close price of s&p500 universe
  #' 
  #' @param stock_data A list of time series data in xts
  #' 
  #' @return A dataframe of adjusted close prices of s&p 500 universe
  
  # retrieve adjusted close prices and format column names
  df <- as.data.frame(do.call(cbind,stock_data))
  close_price_df <- df[grepl('Adjusted{1}$',colnames(df))]
  col_names <- colnames(close_price_df)
  colnames(close_price_df) <- lapply(col_names, function(u) strsplit(u,".",fixed = TRUE)[[1]][1])
  close_price_df[is.na(close_price_df)] <- -999.0
  
  # correcting for missing - when cbind
  colnames(close_price_df)[which(names(close_price_df) == "BRK")] <- "BRK-B" 
  colnames(close_price_df)[which(names(close_price_df) == "BF")] <- "BF-B" 
  
  return(close_price_df)
}

slicing_data <- function(df, start_date, end_date){
  #' slicing data beginning start_date and end_date 
  #' if either date is non business day, dataframe 
  #' is sliced to be longest time span within the specified time frame, 
  #' starting and ending with business day
  #' 
  #' @param df A dataframe to be sliced
  #' @param start_date A string
  #' @param end_date A string
  #' 
  #' @return A sliced dataframe
  
  time_frame <- rownames(df)
  biz_start <- time_frame[time_frame >= start_date][1]
  biz_end <- tail(time_frame[time_frame <= end_date],1)
  
  biz_start_idx <- which(time_frame == biz_start)
  biz_end_idx <- which(time_frame == biz_end)
  
  return(as.data.frame(t(t(df)[, biz_start_idx:biz_end_idx])))
  
}

correlation <- function(stock_data, start_date, end_date){
  #' A wrapper to calculate return and then correlation
  #' 
  #' @param stock_data A list of time series data in xts
  #' @param start_date A string
  #' @param end_date A string
  #' 
  #' @return A correlation matrix of the s&p 500 universe across the time frame specified
  
  adj_close_price <- format_close_price(stock_data)
  sliced_price_df <- slicing_data(adj_close_price, start_date, end_date)
  sliced_return_df <- sliced_price_df[-1,]/sliced_price_df[1:(dim(sliced_price_df)[1]-1),] - 1
  sliced_return_df[is.na(sliced_return_df)] <- 0
  
  corr <- cor(sliced_return_df)
  
  return(corr)
}