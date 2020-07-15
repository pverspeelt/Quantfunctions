context("Test ATR stop")

load(file.path("ADM.rda"))

test_that("Test ATR_stop inputs", {
  expect_error(ATR_stop(ADM, n = 0))
  expect_error(ATR_stop(ADM, coef = 0))
  expect_error(ATR_stop(mtcars))
  
  # test NA values
  ADM$ADM.Close[1] <- NA
  expect_error(ATR_stop(ADM))
})

test_that("Test ATR_stop calculations",{
  
  out <- ATR_stop(ADM)
  
  # check first calculated value
  expect_equal(as.numeric(out[6]), 38.309, tolerance = .0001)
 
  # on default settings the number of NA's should be 5 
  expect_equal(sum(is.na(out)), 5)   
  
  # output should have the same number of rows as the input
  expect_length(out, length(ADM$ADM.Close))
})

