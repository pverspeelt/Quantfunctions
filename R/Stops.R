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
#' @param trend Indicates if chandelier should be calculated for the up-trend or down-trend. 
#' Default is up. Possible values are "up" or "down".
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
    chandelier <- TTR::runMin(quantmod::Lo(HLC), n) + coef * TTR::ATR(quantmod::HLC(HLC), n)[,"atr"]  
  } else {
    chandelier <- TTR::runMax(quantmod::Hi(HLC), n) - coef * TTR::ATR(quantmod::HLC(HLC), n)[,"atr"]
  }
  return(setNames(chandelier, "chandelier_stop"))
}



#' Safezone Stop
#' 
#' The safezone stop was developed by Alexander Elder. The safezone stop is described in his book "Come Into My Trading Room". 
#' 
#' Once you have defined a trend, you need to choose the length of the lookback period for measuring noise level. 
#' It has to be long enough to track recent behavior but short enough to be relevant for current trading. 
#' 
#' If the trend is up, mark all downside penetrations during the lookback period, 
#' add their depths, and divide the sum by the number of penetrations. This gives you
#' the average downside penetration for the selected lookback period. Multiply 
#' the average downside penetration by a coefficient, to place stops farther away from the market 
#' than the average level of noise. The results are subtracted from yesterday's low to create the stop. 
#' To prevent the stop from lowering too fast, the prevent lookback option will keep the maximum calculated 
#' stop for the specified lookback. The rules are reversed for down trend calculations.
#'  
#'
#' @param HL An object that is coercible to xts or matrix and contains High-Low prices.
#' @param n Number of lookback periods for the safezone period. Default is 10.
#' @param coef Coefficient for multiplying the average downside (or upside) penetration. Default is 2.
#' @param prevent Prevents the stop from lowering for x days. Default is 5.
#' @param trend Indicates if the safezone stop should be calculated for the up-trend or down-trend. 
#' Default is "up". Possible values are "up" or "down".
#'
#' @return Returns the safezone exit points which can be used as stop loss in a trend following strategy.
#' @export
#'
#' @examples
#' \dontrun{
#' # show safezone stop on chart
#' getSymbols("ADM", from = "2018-01-01", to = "2018-07-01")
#' chartSeries(ADM)
#' addTA(safezone(ADM), on = 1)
#' }
safezone <- function(HL, n = 10, coef = 2, prevent = 5, trend = "up"){
  
  if(!trend %in% c("up", "down")) stop("trend should be up or down", call. = FALSE)
  
  if(trend == "down"){
    high <- Hi(HL)
    up_pen <- ifelse(high > quantmod::Lag(high), high - quantmod::Lag(high), 0)
    up_sum <- TTR::runSum(up_pen, n = n)
    up_sumYN <- TTR::runSum(up_pen > 0)
    up_avg <- ifelse(up_sumYN == 0, 0, up_sum / up_sumYN)
    up_stop <- quantmod::Lag(high) + coef * quantmod::Lag(up_avg)
    protection <- TTR::runMin(up_stop, n = prevent)
  } else {
    low <- Lo(HL)
    down_pen <- ifelse(quantmod::Lag(low) > low, quantmod::Lag(low) - low, 0)
    down_sum <- TTR::runSum(down_pen, n = n)
    down_sumYN <- TTR::runSum(down_pen > 0)
    down_avg <- ifelse(down_sumYN == 0, 0, down_sum / down_sumYN)
    short_stop <- quantmod::Lag(low) - coef * quantmod::Lag(down_avg)
    protection <- TTR::runMax(short_stop, n = prevent)
  }
  
  return(setNames(protection, "safezone_stop"))
}

