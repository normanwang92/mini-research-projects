library(Quandl)
library(rvest)
library(chron)
library(timeDate)

# Set the working directory
setwd("/Users/Norman/Documents/GitHub/INVESCO/Q1")
DB_NAME = "sp_data.rds"

## getting sp 500 constituents 
sp_symbols <- function(){
  
    sp500_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
    
    symbols_table <- sp500_wiki %>%
      html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
      html_table(fill = TRUE)
    
    symbols_table <- symbols_table[[1]]
    symbols <- as.character(symbols_table$`Ticker symbol`)
    symbols <- gsub(".","-",symbols,fixed = TRUE)
    
  return(symbols)
  }

## getting daily prices and volume for one stock
get_single_stock <-
  function(sym, start_date, end_date) {
    
    # make sure start_date is no greater than end_date 
    #   if start_date is same as end_date, make sure it's business day
    stopifnot(start_date <= end_date) 
    tf <- seq(as.Date(start_date), as.Date(end_date),by = 1)
    tf <- tf[isBizday(timeDate(tf), holidays = holidayNYSE(1975:2018), wday = 1:5)]
    
    if(length(tf) == 0){
      return()
    }
    
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
      error=function(cond){ ## ignoring error flag
        
        # col_names <- c(".Open",".High",".Low",".Close",".Volume",".Adjusted")
        # col_names <- paste(sym,col_names,sep = "")
        # 
        # empty_df <- as.data.frame(matrix(data = -999.0, ncol = 6, nrow = length(tf)), 
        #                           row.names = tf)
        # 
        # empty_df <- set_names(empty_df,col_names)
        # 
        # return(empty_df)
      }
    )
  }

## expand on current zoo series of daily prices and volume for single stock
update_single_stock <-
  function(sym, ts_series, start_date, end_date){
    previous_start <- index(ts_series)[1]
    previous_end <- tail(index(ts_series),1)
    
    if(start_date < previous_start){

      end_date_to_bind <- as.Date(previous_start, format="%Y-%m-%d")-1
      data_to_bind <-
        get_single_stock(sym, start_date = start_date, end_date = end_date_to_bind)
      ts_series <- rbind(data_to_bind, ts_series)
    }

    if(end_date > previous_end){
      start_date_to_bind <- as.Date(previous_end, format="%Y-%m-%d")+1
      data_to_bind <-
        get_single_stock(sym, start_date = start_date_to_bind, end_date = end_date)
      ts_series <- rbind(ts_series, data_to_bind)
    }

    return(ts_series)
  }

## get daily prices and volumes for all stocks
get_all_stocks <-
  function(syms = sp_symbols(), start_date, end_date, save_file = TRUE, return_file = FALSE) {
    
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
    
    # existing_data <- load(file)
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