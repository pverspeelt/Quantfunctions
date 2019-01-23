#' Heikin-Ashi
#' 
#' Calculates the Heikin-Ashi values needed for plotting in a candlestick chart.
#' 
#'  1. The Heikin-Ashi Close is an average of the open, high, low and 
#'  close for the current period. 
#'  
#'  2. The Heikin-Ashi Open is the average of the prior Heikin-Ashi open and the 
#'  prior Heikin-Ashi close
#'  
#'  3. The Heikin-Ashi High is the maximum of: 
#'  the current period's high, the current Heikin-Ashi open or the current 
#'  Heikin-Ashi close.
#'  
#'  4. The Heikin-Ashi low is the minimum : 
#'  the current period's low, the current Heikin-Ashi open or the current 
#'  Heikin-Ashi close.
#'  
#'  More information can be found at \href{https://www.investopedia.com/trading/heikin-ashi-better-candlestick/}{Investopedia}
#'
#' @param x an xts object that contains OHLC data 
#'
#' @return The heikin_ashi function will return a OHLC dataset with the calculated 
#' heikin ashi values needed for plotting in a candlestick chart.
#' 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ADM <- getSymbols("ADM", 
#'                   from = "2018-01-01", 
#'                   to = "2018-07-01", 
#'                   auto.assign = FALSE)
#' ha_ADM <- heikin_ashi(ADM)
#' chartSeries(ha_ADM)
#' }
#'  
heikin_ashi <- function(x) {
  
  if(!quantmod::is.OHLC(x)) stop("data must contain OHLC columns")
  
  if(any(is.na(x))) 
    stop("data contains NA values, either remove these records or fix them")
  
  heikin_close <- xts::xts(rowMeans(quantmod::OHLC(x)), 
                           order.by = zoo::index(x))
  heikin_open  <- quantmod::Op(x)
  
  # need a loop: heiki ashi open is dependent on the previous value
  for(i in 2:nrow(x)) {
    heikin_open[i] <- (heikin_open[i-1] + heikin_close[i-1]) / 2
  }
  
  heikin_high <- xts::xts(apply(cbind(quantmod::Hi(x), heikin_open, heikin_close), 1, max), 
                          order.by = zoo::index(x))
  heikin_low <- xts::xts(apply(cbind(quantmod::Lo(x), heikin_open, heikin_close), 1, min), 
                         order.by = zoo::index(x))
  
  out <- merge(heikin_open, heikin_high, heikin_low, heikin_close)
  names(out) <- c("Open", "High", "Low", "Close")
  
  return(out)
}



