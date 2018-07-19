#' Chandelier Exit
#' 
#' Calculates the chandelier exits in an up or down trend. The calculated exit 
#' points are used as a stop loss in a trend following trading strategy.
#' 
#' In an up-trend the default formula is:
#' Highest High in last 22 days - 3 * ATR for 22 days
#' 
#' In a down-trend the formula is reversed:
#' Lowest Low in last 22 days + 3 * ATR for 22 days
#'
#' @param HLC Object that is coercible to xts or matrix and contains High-Low-Close prices.
#' @param n Number of periods for chandelier period. Default is 22.
#' @param coef ATR coefficient. Default is 3
#' @param trend Indicates if chandelier should be calculated for the up-trend or down-trend. Default is up.
#'
#' @return Returns the chandelier exit points which can be used as stop loss in a trend following strategy.
#' @export
#'
#' @examples
#' \dontrun{
#' getSymbols("ADM", from = "2018-01-01", to = "2018-07-01")
#' chartSeries(ADM)
#' addTA(chandelier(ADM), on = 1)
#' }
#' 
#' 
chandelier <- function(HLC, n = 22, coef = 3, trend = "up"){
  HLC <- try.xts(HLC, error = as.matrix)
  if(!trend %in% c("up", "down")) stop("trend should be up or down", call. = FALSE)
  
  if(trend == "down"){  
    chandelier <- runMin(Lo(HLC), n) + coef * ATR(HLC(HLC), n)[,"atr"]  
  } else {
    chandelier <- runMax(Hi(HLC), n) - coef * ATR(HLC(HLC), n)[,"atr"]
  }
  return(chandelier)
}