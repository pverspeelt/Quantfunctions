# matches IB's stoch RSI
# Needs a minimum of (2 * n) 28 days of price action before the numbers start 
# working, but this is about 2.5 times faster than using 
# (RSI(adm$close, 14) - runMin(RSI(adm$close, 14))) / (runMax(RSI(adm$close, 14)) - runMin(RSI(adm$close, 14)))
# and gives only 4 days extra info. 

# price Price series that is coercible to xts or matrix
# n Number of periods
stochRSI <- function(price, n = 14L){
  rsi <- RSI(price, n)
  rsi_out <- (rsi - runMin(rsi, n)) / (runMax(rsi, n) - runMin(rsi, n))
  return(rsi_out)
}

# example
# getSymbols("ADM", from = "2018-01-01", to = "2018-07-01")
# chartSeries(ADM, TA = NULL) # without volume
# addTA(stochRSI(Cl(ADM)))

