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

rm(ADM)

# why is this sometimes needed and most of the times not?
load(file.path("ADM.rda"), envir = .GlobalEnv)

test_that("Test ATR_stop calculations",{
  
  # using hash as loop calculations might take time
  expect_known_hash(ATR_stop(ADM), "536d720574")
 
  # on default settings the number of NA's should be 
  expect_equal(sum(is.na(ATR_stop(ADM))), 5)  
  
  expect_length(ATR_stop(ADM), length(ADM$ADM.Close))
})

