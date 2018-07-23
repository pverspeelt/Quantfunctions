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
#' @param price Price series that is coercible to xts or matrix
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
stochRSI <- function(price, n = 14L){
  rsi <- TTR::RSI(price, n)
  rsi_out <- (rsi - TTR::runMin(rsi, n)) / (TTR::runMax(rsi, n) - TTR::runMin(rsi, n))
  return(setNames(rsi_out, "stochRSI"))
}


