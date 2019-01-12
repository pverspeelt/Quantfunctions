#' Stochastic RSI
#' 
#' This stochastic RSI matches IB's stoch RSI.
#' Needs a minimum of (2 * n) 28 days of price action before the numbers start 
#' working, but this is about 2.5 times faster than using
#' (RSI(adm$close, 14) - runMin(RSI(adm$close, 14))) / (runMax(RSI(adm$close, 14)) - runMin(RSI(adm$close, 14)))
#' and gives only 4 days extra info. 
#' 
#' It is a measure of RSI relative to its own high/low range over a user defined period of time. 
#' The Stochastic RSI is an oscillator that calculates a value between 0 and 1 which can be plotted as a line. 
#' This indicator is primarily used for identifying overbought and oversold conditions.
#' 
#' @param x Price series that is coercible to xts or matrix
#' @param n Number of periods. Default is 14.
#'
#' @return The stochRSI function will return a timeseries with values ranging between 0 and 1
#' @export
#'
#' @examples
#' \dontrun{
#' getSymbols("ADM", from = "2018-01-01", to = "2018-07-01")
#' chartSeries(ADM, TA = NULL) # without volume
#' addTA(stochRSI(Cl(ADM)))
#' }
stochRSI <- function(x, n = 14L){
  
  if (n < 1 || n > NROW(x)) 
    stop(sprintf("n = %d is outside valid range: [1, %d]", 
                 n, NROW(x)))
  
  x <- xts::try.xts(x, error = as.matrix)
  
  rsi <- TTR::RSI(x, n)
  rsi_out <- (rsi - TTR::runMin(rsi, n)) / (TTR::runMax(rsi, n) - TTR::runMin(rsi, n))
  return(setNames(rsi_out, "stochRSI"))
}



#' Moving Average Envelope
#' 
#' This function creates a moving average envelope. 
#'
#' @param x an xts object that contains OHLC data
#' @param maType moving average type. Default is "EMA". But any 
#' @param n Number of periods. Default is 22.
#' @param p percentage of moving the upper and lower bounderies away from the 
#' midpoint of the moving average. Default is 2.5. Needs to be between 0 and 100.
#' @param ... any other passthrough parameters
#'
#' @return Returns a timeseries with the lower and upper bound and the midpoint of the chosen moving average.
#' @export
#'
#' @examples
#' \dontrun{
#' getSymbols("ADM", from = "2018-01-01", to = "2018-07-01")
#' chartSeries(ADM)
#' addTA(envelope(ADM), on = 1)
#' }
#' @references
#' \url{https://www.investopedia.com/terms/e/envelope.asp}
#' 
envelope <- function(x, maType = "EMA", n = 22, p = 2.5, ...){
  
  if (n < 1 || n > NROW(x)) 
    stop(sprintf("n = %d is outside valid range: [1, %d]", 
                 n, NROW(x)))
  
  if (p > 100 || p < 0) 
    stop("invalid value of p. p needs to be between 0 and 100") 
                 
  if(!quantmod::is.OHLC(x)) stop("x must contain OHLC columns")
  
  movavg <- do.call(maType, list(quantmod::Cl(x), n = n, ...))
  envelope <- cbind(movavg * (1 - p/100), movavg, movavg * (1 + p/100))
  
  return(setNames(envelope, c("lower_bound", "midpoint", "upper_bound")))
}



