context("Test chandelier stop")

load(file.path("ADM.rda"))

test_that("Test chandelier calculations", {
  
  # calculate separate pieces
  max10 <- TTR::runMax(quantmod::Hi(ADM), 10)
  min10 <- TTR::runMin(quantmod::Lo(ADM), 10)
  atr10 <- TTR::ATR(quantmod::HLC(ADM), 10)[,"atr"]
  coef <- 3
  # combine calculations
  outcome_up <- max10 - 3 * atr10
  outcome_down <-  min10 + 3 * atr10

  # test calculations against function
  expect_equivalent(chandelier(ADM, 10), outcome_up)
  expect_equivalent(chandelier(ADM, 10, trend = "down"), outcome_down)

  # on default settings the number of NA's should be 22
  expect_equal(sum(is.na(chandelier(ADM))), 22)
  expect_equal(sum(is.na(chandelier(ADM, trend = "down"))), 22)
  
  # output should have the same number of rows as the input
  expect_length(chandelier(ADM), length(ADM$ADM.Close))
})


test_that("Test chandelier inputs", {
  expect_error(chandelier(ADM, n = 0))
  expect_error(chandelier(ADM, coef = 0))
  expect_error(chandelier(ADM, trend = "abc"))
})



