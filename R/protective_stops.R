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
#' @param x Object that is coercible to xts or matrix and contains High-Low-Close prices.
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
chandelier <- function(x, n = 22, coef = 3, trend = "up"){
  
  # input tests
  x <- xts::try.xts(x, error = as.matrix)
  
  if(n < 1 || n > NROW(x)) 
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  
  if(coef <= 0) stop("ATR coefficient should have a positive value")
  
  if(!trend %in% c("up", "down")) 
    stop("trend should be up or down", call. = FALSE)
  
  if(trend == "down"){  
    chandelier <- TTR::runMin(quantmod::Lo(x), n) + coef * TTR::ATR(quantmod::HLC(x), n)[,"atr"]  
  } else {
    chandelier <- TTR::runMax(quantmod::Hi(x), n) - coef * TTR::ATR(quantmod::HLC(x), n)[,"atr"]
  }
  
  names(chandelier) <- "chandelier_stop"
  
  return(chandelier)
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
#' @param x An object that is coercible to xts or matrix and contains High-Low prices.
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
safezone <- function(x, n = 10, coef = 2, prevent = 5, trend = "up"){
  
  # input tests
  if(n < 1 || n > NROW(x)) 
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  
  if(coef <= 0) stop("The coefficient should have a positive value")
  
  if(prevent <= 0) stop("The prevent value should have a positive value")
  
  if(!trend %in% c("up", "down")) stop("trend should be up or down", call. = FALSE)
  
  if(trend == "down"){
    high <- quantmod::Hi(x)
    up_pen <- ifelse(high > quantmod::Lag(high), high - quantmod::Lag(high), 0)
    up_sum <- TTR::runSum(up_pen, n = n)
    up_sumYN <- TTR::runSum(up_pen > 0)
    up_avg <- ifelse(up_sumYN == 0, 0, up_sum / up_sumYN)
    up_stop <- quantmod::Lag(high) + coef * quantmod::Lag(up_avg)
    protection <- TTR::runMin(up_stop, n = prevent)
  } else {
    low <- quantmod::Lo(x)
    down_pen <- ifelse(quantmod::Lag(low) > low, quantmod::Lag(low) - low, 0)
    down_sum <- TTR::runSum(down_pen, n = n)
    down_sumYN <- TTR::runSum(down_pen > 0)
    down_avg <- ifelse(down_sumYN == 0, 0, down_sum / down_sumYN)
    short_stop <- quantmod::Lag(low) - coef * quantmod::Lag(down_avg)
    protection <- TTR::runMax(short_stop, n = prevent)
  }
  
  names(protection) <- "safezone_stop"
  
  return(protection)
}



#' Average True Range Trailing Stop
#' 
#' This average true range trailing stop was developed by Sylvain Vervoort. 
#' 
#' Based on \href{https://stackoverflow.com/questions/5554220/r-statistics-average-true-range-trailing-stop-indicator}{this question} 
#' on stackoverflow and \href{http://marintrading.com/106VERV.PDF}{this article} by Sylvain Vervoort.
#'
#' @param x an xts object that contains OHLC data
#' @param n Number of lookback periods for the ATR Stop. Default is 5
#' @param coef ATR coefficient. Default is 3.5
#'
#' @return Returns an average true range trailing stop which can be used as a stop loss in a trend following system.
#' @export
#'
#' @examples
#' \dontrun{
#' # show ATR stop on chart
#' getSymbols("ADM", from = "2018-01-01", to = "2018-07-01")
#' chartSeries(ADM)
#' addTA(ATR_stop(ADM), on = 1)
#' }
ATR_stop <- function(x, n = 5, coef = 3.5){
  
  # input tests
  if(n < 1 || n > NROW(x)) 
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))
  
  if(coef <= 0) stop("The ATR coefficient should have a positive value")
  
  if(!quantmod::is.OHLC(x)) stop("x must contain OHLC columns")
  
  if(any(is.na(x))) 
    stop("data contains NA values, either remove these records or fix them")
  
  
  x$ATR_stop <- coef * TTR::ATR(quantmod::HLC(x), n)[, "atr"]
  x$ATR_trail_stop <- 0
  x$lag_cl <- stats::lag(quantmod::Cl(x))
  
  for(i in seq.int(n + 1L, nrow(x))){
    trail1 <- zoo::coredata(x$ATR_trail_stop[i-1])
    if(quantmod::Cl(x)[i] > trail1 && x$lag_cl[i] > trail1) {
      x$ATR_trail_stop[i] <- max(trail1, zoo::coredata(quantmod::Cl(x)[i] - x$ATR_stop[i]))
    } else
      if(quantmod::Cl(x)[i] < trail1 && x$lag_cl[i] < trail1) {
        x$ATR_trail_stop[i] <- min(trail1, zoo::coredata(quantmod::Cl(x)[i] + x$ATR_stop[i]))
      } else
        if(quantmod::Cl(x)[i] > trail1) {
          x$ATR_trail_stop[i] <- zoo::coredata(quantmod::Cl(x)[i] - x$ATR_stop[i])
        } else {
          x$ATR_trail_stop[i] <- zoo::coredata(quantmod::Cl(x)[i] + x$ATR_stop[i])
        }
  }
  
  # clean up:
  x$ATR_trail_stop[1:n] <- NA
  
  return(x$ATR_trail_stop)
}  

