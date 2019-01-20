# Script to recreate ADM data to be used in testing. 
# Gets 6 months of 2018 AMD stock data from yahoo. 

library(quantmod)
ADM <- getSymbols("ADM", from = "2018-01-01", to = "2018-06-30", auto.assign = FALSE)

save(ADM, file = "tests/testthat/ADM.rda")
