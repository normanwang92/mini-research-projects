library(Quandl)
library(rvest)
library(chron)
library(timeDate)

# Set the working directory
setwd("/Users/Norman/Documents/GitHub/INVESCO/Q1")
DB_NAME = "sp_data.rds"

## get s&p500 constituents 
sp_symbols <- function(){
  #' data scraper to get the s&p 500 constituents
  #' 
  #' @return A list of tickers of s&p 500 universe
  
    sp500_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
    
    symbols_table <- sp500_wiki %>%
      html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
      html_table(fill = TRUE)
    
    symbols_table <- symbols_table[[1]]
    symbols <- as.character(symbols_table$`Ticker symbol`)
    symbols <- gsub(".","-",symbols,fixed = TRUE)
    
  return(symbols)
  }

get_single_stock <-
  function(sym, start_date, end_date) {
    #' ## getting daily prices and volume for one stock
    #' 
    #' @param sym A string representing the ticker/symbol
    #' @param start_date A string
    #' @param end_date A string
    #' 
    #' @return An xts object containing time series data for sym
    
    # make sure start_date is no greater than end_date 
    #   if start_date is same as end_date, make sure it's business day
    stopifnot(start_date <= end_date) 
    tf <- seq(as.Date(start_date), as.Date(end_date),by = 1)
    tf <- tf[isBizday(timeDate(tf), holidays = holidayNYSE(1975:2018), wday = 1:5)]
    
    if(length(tf) == 0){return()}
    
    # get business start and end date within the specified time frame
    start_date <- tf[1]
    end_date <- tail(tf,1)
    
    require(devtools)
    
    tryCatch(
      getSymbols(sym,
                 auto.assign=FALSE,
                 from=start_date,
                 to=as.Date(end_date)+1,
                 src="yahoo"
      ),
      error=function(cond){ 
        # ignoring error flag, error mostly caused by unavailable data from server
      }
    )
  }


update_single_stock <-
  function(sym, ts_series, start_date, end_date){
    #' expand on current zoo series of daily prices and volume for ticker 'sym'
    #' 
    #' @param sym A string representing the ticker/symbol
    #' @param ts_series A xts object containing existing data for sym
    #' @param start_date A string
    #' @param end_date A string
    #' 
    #' @return An xts object containing updated time series data for sym
    
    # get the existing start and end date of ts_series
    previous_start <- index(ts_series)[1]
    previous_end <- tail(index(ts_series),1)
    
    # if new start date goes back further, expand ts_series from new start date to
    #   existing start date
    if(start_date < previous_start){
      end_date_to_bind <- as.Date(previous_start, format="%Y-%m-%d")-1
      data_to_bind <-
        get_single_stock(sym, start_date = start_date, end_date = end_date_to_bind)
      ts_series <- rbind(data_to_bind, ts_series)
    }

    # similarly, if new end date goes beyond, expand ts_series from existing end date to
    #   new end date
    if(end_date > previous_end){
      start_date_to_bind <- as.Date(previous_end, format="%Y-%m-%d")+1
      data_to_bind <-
        get_single_stock(sym, start_date = start_date_to_bind, end_date = end_date)
      ts_series <- rbind(ts_series, data_to_bind)
    }

    return(ts_series)
  }

get_all_stocks <-
  function(syms = sp_symbols(), start_date, end_date, save_file = TRUE, return_file = FALSE) {
    #' wrapper to get daily prices and volumes for all stocks with options either to 
    #' save to database or return the data
    #' 
    #' @param syms A list of string representing the ticker/symbol, default to 
    #' @param start_date A string
    #' @param end_date A string
    #' @param save_file A boolean, save to database if TRUE
    #' @param save_file A boolean, return result if TRUE
    #' 
    #' @return A list of xts objects containing time series data for whole universe
    
    # make sure start date no greater than end date
    stopifnot(start_date <= end_date)
    all_data <- list()
    
    for(s in syms){
      single_data <- get_single_stock(sym = s, start_date = start_date, end_date = end_date)
      
      if(!is.null(single_data)) {
        all_data[[s]] <- single_data
      }
    }
    
    if(save_file){
      saveRDS(all_data, file = DB_NAME)
    }
    
    if(return_file){
      return(all_data)
    }
  }

update_all_stocks <- 
  function(syms = sp_symbols(), existing_data = readRDS(DB_NAME), start_date, end_date) {
    #' wrapper to update daily prices and volumes for all stocks up to time horizon specified
    #' 
    #' @param syms A list of string representing the ticker/symbol, default to 
    #' @param existing_data A list of xts objects containing time series data
    #' @param start_date A string
    #' @param end_date A string
    #' 
    #' @return A list of xts objects containing updated time series data
    
    # load existing data, look for difference, add ticker or delete if necessary
    existing_names <- names(existing_data)
    names_to_add <- setdiff(syms, existing_names)
    names_to_remove <- setdiff(existing_names, syms)
    
    ## removing data if name no longer listed
    if (length(names_to_remove) != 0){
      existing_data <- existing_data[!existing_names %in% names_to_remove]
      existing_names <- names(existing_data)
    }
    
    ## expanding data for existing names
    for(s in existing_names) {
      cat(".")
      existing_data[[s]] <- update_single_stock(s,existing_data[[s]],start_date,end_date)
    }
    
    ## adding data for new names
    if (length(names_to_add) != 0){
      new_data <- get_all_stocks(names_to_add, 
                                 start_date, 
                                 end_date, 
                                 save_file = FALSE, 
                                 return_file = TRUE)
      
      existing_data <- c(existing_data, new_data)
    }
    
    # updating database
    saveRDS(existing_data, file = DB_NAME)

    return(existing_data)
  }