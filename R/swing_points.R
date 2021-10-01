#' Swing high swing low
#' 
#' This indicator adds the swing high and swing low points on a chartSeries.
#'  
#' The indicator checks historical candles to detect whether there was a 
#' swing point. This will work by inputting a number to select the range of historical
#' candles to check to see if there is a swing point in the selected trading period. 
#' If a candle has the lowest low or highest high in the selected range, then we 
#' know it was a swing point.
#'
#' @param x an xts object that contains OHLC data
#' @param n Number of periods. Default is 7.
#'
#' @return Adds swing points to a chartSeries
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
#' add_swing_high_low(ADM, n = 1)
#' }
#' @references
#' swing high: \url{https://www.investopedia.com/terms/s/swinghigh.asp}
#' swing low: \url{https://www.investopedia.com/terms/s/swinglow.asp}
#' 
add_swing_high_low <- function(x, n = 7){
  
  # check input parameters
  if(!quantmod::is.OHLC(x)) 
    stop("x must contain OHLC columns.")
  
  if (n < 1 || n > NROW(x)) 
    stop(glue("n = {n} is outside valid range: [1, {NROW(x)}]"),
         call. = FALSE)
  
  # calculate the low swings
  x_low <- zoo::rollapply(quantmod::Lo(x), n, min)
  y_low <- ifelse(x_low == quantmod::Lo(x), 1, 0)
  
  z_low <- ifelse(x_low < stats::lag(quantmod::Lo(x),-1) &
                    x_low < stats::lag(quantmod::Lo(x),-2) &
                    stats::lag(quantmod::Lo(x),-1) < stats::lag(quantmod::Lo(x),-2) &
                    y_low == 1,
                  1,
                  0)
  
  swing_low <- ifelse(z_low == 1, quantmod::Lo(x), NA)
  
  # calculate the high swings
  x_high <- zoo::rollapply(quantmod::Hi(x), n, max)
  y_high <- ifelse(x_high == quantmod::Hi(x), 1, 0)
  
  z_high <- ifelse(x_high > stats::lag(quantmod::Hi(x),-1) &
                     x_high > stats::lag(quantmod::Hi(x),-2) &
                     stats::lag(quantmod::Hi(x),-1) > stats::lag(quantmod::Hi(x),-2) &
                     y_high == 1,
                   1,
                   0)
  
  swing_high <- ifelse(z_high == 1, quantmod::Hi(x), NA)
  
  # set colours
  swings <- ifelse(!is.na(swing_low), swing_low, ifelse(!is.na(swing_high), swing_high, NA))
  swing_cols <- ifelse(swings == quantmod::Lo(x), "green", NA)
  swing_cols <- ifelse(swings == quantmod::Hi(x), "red", swing_cols)
  
  # set pch  
  swing_pch <- ifelse(swing_cols == "green", 24, 
                      ifelse(swing_cols == "red", 25, NA))
  
  # add points to chart
  quantmod::addPoints(1:nrow(swings), swings, col = swing_cols, pch = swing_pch, cex = 0.75, on = 1)
  
}