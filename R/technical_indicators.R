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
#' library(quantmod)
#' ADM <- getSymbols("ADM", 
#'                   from = "2018-01-01", 
#'                   to = "2018-07-01",
#'                   auto.assign = FALSE)
#' chartSeries(ADM, TA = NULL) # without volume
#' addTA(stochRSI(Cl(ADM)))
#' }
stochRSI <- function(x, n = 14L){
  
  if (n < 1 || n > NROW(x)) 
    stop(glue("n = {n} is outside valid range: [1, {NROW(X)}]"),
         call. = FALSE)

  x <- xts::try.xts(x, error = as.matrix)
  
  if(!quantmod::has.Cl(x)) 
    stop("x must contain a close column.",
         call. = FALSE)
  
  rsi <- TTR::RSI(quantmod::Cl(x), n)
  rsi_out <- (rsi - TTR::runMin(rsi, n)) / (TTR::runMax(rsi, n) - TTR::runMin(rsi, n))
  names(rsi_out) <- "stochRSI"
  
  rsi_out
}



#' Moving Average Envelope
#' 
#' This function creates a moving average envelope. 
#'
#' @param x an xts object that contains OHLC data
#' @param ma Type of moving average used. Default is "EMA". Options are EMA or SMA. 
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
#' library(quantmod)
#' ADM <- getSymbols("ADM", 
#'                   from = "2018-01-01",
#'                   to = "2018-07-01",
#'                   auto.assign = FALSE)
#' chartSeries(ADM)
#' addTA(envelope(ADM), on = 1)
#' }
#' @references
#' \url{https://www.investopedia.com/terms/e/envelope.asp}
#' 
envelope <- function(x, ma = "EMA", n = 22, p = 2.5, ...){
  
  # check input parameters
  if (n < 1 || n > NROW(x)) 
    stop(glue("n = {n} is outside valid range: [1, {NROW(x)}]"),
         call. = FALSE)
  
  if (p > 100 || p < 0) 
    stop(glue("invalid value of p. p needs to be between 0 and 100.
              You supplied: {p}"),
         call. = FALSE)
                 
  if(!quantmod::is.OHLC(x)) 
    stop("x must contain OHLC columns.",
         call. = FALSE)
  
  if (!ma %in% c("EMA", "SMA")) 
    stop(glue('Type of moving average (ma) should be "EMA" or "SMA".
              You supplied: {ma}.'),
         call. = FALSE)
  
  # functional code
  if (ma == "SMA") {
    movavg <- TTR::SMA(quantmod::Cl(x), n = n)
  } else {
    movavg <- TTR::EMA(quantmod::Cl(x), n = n)
  }

  envelope <- cbind(movavg * (1 - p/100), movavg, movavg * (1 + p/100))
  names(envelope) <- c("lower_bound", "midpoint", "upper_bound")
  
  envelope
}



