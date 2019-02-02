#' Alert Marker
#' 
#' The alert marker was developed by Alexander Elder. The alert marker is part 
#' of the impulse system described in his book "Come Into My Trading Room" on 
#' pages 158-162.
#'
#' @param x an xts object that contains OHLC data.
#' @param n Number of periods for the ema period. Default is 13.
#'
#' @return returns an xts object with marker values.
#' @export
#' 
#' @family Alexander Elder functions
#' 
#' @examples
#' \dontrun{
#' library(quantmod)
#' ADM <- getSymbols("ADM", 
#'                   from = "2018-01-01", 
#'                   to = "2018-07-01", 
#'                   auto.assign = FALSE)
#'                   
#' chartSeries(ADM) 
#' markers <- alert_marker(ADM)
#' 
#' # define colours
#' cols <- ifelse(markers == quantmod::Hi(ADM), "green", "red")
#' 
#' addPoints(1:nrow(markers), markers, col= cols, pch=19, cex=0.5, on = 1)
#' }
alert_marker <- function(x, n = 13){
  
  # check input parameters
  if (n < 1 || n > NROW(x)) 
    stop(sprintf("n = %d is outside valid range: [1, %d]", 
                 n, NROW(x)))
  
  if(!quantmod::is.OHLC(x)) 
    stop("x must contain OHLC columns.")
  
  # function code
  out_ema <- TTR::EMA(quantmod::Cl(x), n = n)
  out_macd <- TTR::MACD(quantmod::Cl(x))
  hist <- out_macd$macd - out_macd$signal
  
  up_marker <- ifelse(out_ema > quantmod::Lag(out_ema) & 
                        hist > quantmod::Lag(hist), 1, 0)
  
  down_marker <- ifelse(out_ema < quantmod::Lag(out_ema) & 
                          hist < quantmod::Lag(hist), -1, 0)
  
  alert_marker <- ifelse(up_marker == 1, quantmod::Hi(x), ifelse(down_marker == -1, quantmod::Lo(x), NA))
  names(alert_marker) <- "alert_marker"
  alert_marker
}
