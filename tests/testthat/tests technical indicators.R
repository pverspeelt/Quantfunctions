context("Testing equality techical indicators")

load(file.path("ADM.rda"))

test_that("Test stochRSI: values should be between 0 and 1", {
  x <- stochRSI(ADM$ADM.Close)
  expect_equal(1, max(x, na.rm = TRUE))
  expect_equal(0, min(x, na.rm = TRUE))
})

test_that("Test envelope: test outcome ema vs envelope", {
  ema_adm <- TTR::EMA(ADM$ADM.Close, 22)    
  expect_equivalent(envelope(ADM, p = 2.5)$midpoint, ema_adm)
  expect_equivalent(envelope(ADM, p = 2.5)$lower_bound, ema_adm * (1 - 2.5/100))
})


context("Testing incorrect parameters techical indicators")

test_that("Test stochRSI: periods incorrect", {
  expect_error(stochRSI(ADM$ADM.Close, n = 0))
})

test_that("Test envelope: periods, percentage or x is incorrect", {
  expect_error(envelope(ADM, n = 0))
  expect_error(envelope(ADM, p = 101))
  expect_error(envelope(ADM, p = -5))
  expect_error(envelope(mtcars))
})



