#' Get Key Statistics
#' 
#' This function get the information from the key statistics page from yahoo finance. 
#' For individual statistics or statistics to calculate with, it is better to use 
#' `quantmod::getQuote` together with `quantmod::yahooQF`. 
#'
#' @param symbol a stock symbol
#'
#' @return Returns a data.frame containing the key statistics from the statistics page
#' from Yahoo Finance. Available fields are symbol, statistic_group, statistic and value.
#' All values are character values are only as good as the data on Yahoo.
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # returns the statistics for AAPL
#' aapl_stats <- get_statistics("AAPL")
#' 
#' # get all the profitability stats
#' aapl_stat %>% 
#'   filter(statistic_group == "Profitability")
#'  
#' # returns the statistics for AAPL and FB
#' symbols <- c("AAPL", "FB")
#' symbol_stats <- get_statistics(symbols)
#' 
#' symbol_stats %>% 
#'   filter(statistic == "Forward Annual Dividend Rate")
#' }
get_statistics <- function(symbol){
  
  if(is.na(symbol) | !is.character(symbol)){
    stop("symbol must be a single symbol or a vector of symbols.",
         call. = FALSE)
  }
  
  get_stats <- function(symbol){
    statistic_group <- c("Valuation Measures", "Stock Price History", "Share Statistics", 
                         "Dividends & Splits", "Fiscal Year", "Profitability", 
                         "Management Effectiveness", "Income Statement", "Balance Sheet", 
                         "Cash Flow Statement")
    url <- paste0("https://finance.yahoo.com/quote/",symbol,"/key-statistics?p=", symbol)
    dat <- rvest::read_html(url) 
    dat <- rvest::html_table(dat, header = FALSE) 
    names(dat) <- statistic_group
    df_dat <- purrr::map_df(dat, dplyr::bind_rows, .id = "statistic_group")
    
    names(df_dat) <- c("statistic_group", "statistic", "value")
    # remove footnote numbers
    df_dat$statistic <-  gsub(" [0-9]$", "", df_dat$statistic)
    # add symbol to table
    df_dat <- cbind(symbol, df_dat)
  }

  # error function to use in case there is no data returned from yahoo.  
  catch_error <- function(.f, otherwise=NULL) {
    function(...) {
      tryCatch({
        .f(...)  
      }, error = function(e) otherwise(...))
    }
  }

  out <- purrr::map_df(symbol, 
                       catch_error(get_stats, 
                                   otherwise = function(x) {
                                     dplyr::tibble(stock = x,
                                               statistic = NA_character_,
                                               valuation_measures = NA_character_, 
                                               value = NA_character_, 
                                               error = "error in getting data")
                                   }))
  out
}
