context("Test safezone stop")

load(file.path("ADM.rda"))

test_that("Test safezone calculations", {
  
  # calculate separate pieces
  low <- quantmod::Lo(ADM)
  down_pen <- ifelse(quantmod::Lag(low) > low, quantmod::Lag(low) - low, 0)
  down_sum <- TTR::runSum(down_pen, n = 10)
  down_sumYN <- TTR::runSum(down_pen > 0)
  down_avg <- ifelse(down_sumYN == 0, 0, down_sum / down_sumYN)
  short_stop <- quantmod::Lag(low) - 2 * quantmod::Lag(down_avg)
  protection <- TTR::runMax(short_stop, n = 5)
  
  # test calculations against function
  expect_equivalent(safezone(ADM), protection)
  
  # on default settings the number of NA's should be 15
  expect_equal(sum(is.na(safezone(ADM))), 15)
  expect_equal(sum(is.na(safezone(ADM, trend = "down"))), 15)
  
  # output should have the same number of rows as the input
  expect_length(safezone(ADM), length(ADM$ADM.Close))
})


test_that("Test safezone inputs", {
  expect_error(safezone(ADM, n = 0))
  expect_error(safezone(ADM, coef = 0))
  expect_error(safezone(ADM, prevent = 0))
  expect_error(safezone(ADM, trend = "abc"))
})