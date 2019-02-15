#' Market Thermometer
#' 
#' The market thermometer was developed by Alexander Elder. The market
#' thermometer is described in his book "Come Into My Trading Room" on pages 
#' 162-164. 
#'
#' @param x an xts object that contains OHLC data.
#' @param n Number of periods for the ema period. Default is 22.
#'
#' @return returns an xts object containing the columns:
#'  \itemize{
#'    \item temperature contains the calculated temperature values
#'    \item temp_ema Ema values based on the temperature values.}
#' 
#' @family Alexander Elder functions
#' @export
#' 
#'
#' @examples
#' \dontrun{
#' library(quantmod)
#' ADM <- getSymbols("ADM", 
#'                   from = "2018-01-01", 
#'                   to = "2018-07-01", 
#'                   auto.assign = FALSE)
#'                   
#' chartSeries(ADM, TA = NULL) # without volume
#' addTA(thermometer(ADM))
#' }
thermometer <- function(x, n = 22){
  
  # check input parameters
  if (n < 1 || n > NROW(x)) 
    stop(glue("n = {n} is outside valid range: [1, {NROW(x)}]"), 
         call. = FALSE)
  
  if(!quantmod::is.OHLC(x)) 
    stop("x must contain OHLC columns.", 
         call. = FALSE)
  
  # function code
  lows <- quantmod::Lo(x)
  highs <- quantmod::Hi(x)
  temperature <- ifelse(highs < quantmod::Lag(highs) & lows > quantmod::Lag(lows), 
                        0, 
                        ifelse((highs - quantmod::Lag(highs)) > (quantmod::Lag(lows) - lows),
                               highs - quantmod::Lag(highs),
                               quantmod::Lag(lows) - lows)
                        )
  temp_ema <- TTR::EMA(temperature, n = 22)
  thermometer <- merge(temperature, temp_ema)
  names(thermometer) <- c("temperature", "temp_ema")
  thermometer
}

