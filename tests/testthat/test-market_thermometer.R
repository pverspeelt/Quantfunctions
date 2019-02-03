context("Test market thermometer")

load(file.path("ADM.rda"))

test_that("Test market thermometer inputs", {
  expect_error(thermometer(ADM, n = 0))
  expect_error(thermometer(mtcars))
})


test_that("Test alert marker calculations", {
  # calculate separate pieces
  lows <- quantmod::Lo(ADM)
  highs <- quantmod::Hi(ADM)
  temperature <- ifelse(highs < quantmod::Lag(highs) & lows > quantmod::Lag(lows), 0, 
                        ifelse((highs - quantmod::Lag(highs)) > (quantmod::Lag(lows) - lows),
                               highs - quantmod::Lag(highs),
                               quantmod::Lag(lows) - lows)
  )
  
  # use test equivalent: colum names are not compared
  expect_equivalent(thermometer(ADM)$temperature, temperature)
  expect_equivalent(thermometer(ADM)$temp_ema, TTR::EMA(temperature, n = 22))
})


test_that("Test market thermometer outputs", {
  expect_s3_class(thermometer(ADM), "xts")
})


