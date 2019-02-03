context("Test alert marker")

load(file.path("ADM.rda"))

test_that("Test alert marker inputs", {
  expect_error(alert_marker(ADM, n = 0))
  expect_error(alert_marker(mtcars))
})


test_that("Test alert marker calculations", {
  # calculate separate pieces
  out_ema <- TTR::EMA(quantmod::Cl(ADM), n = 13)
  out_macd <- TTR::MACD(quantmod::Cl(ADM))
  hist <- out_macd$macd - out_macd$signal
  
  up_marker <- ifelse(out_ema > quantmod::Lag(out_ema) & 
                        hist > quantmod::Lag(hist), 1, 0)
  
  down_marker <- ifelse(out_ema < quantmod::Lag(out_ema) & 
                          hist < quantmod::Lag(hist), -1, 0)
  
  outcome <- ifelse(up_marker == 1, 
                         quantmod::Hi(ADM), 
                         ifelse(down_marker == -1, 
                                quantmod::Lo(ADM), 
                                NA))
  # test
  expect_equivalent(alert_marker(ADM), outcome)
})


test_that("Test alert marker outputs", {
  expect_s3_class(alert_marker(ADM), "xts")
})
