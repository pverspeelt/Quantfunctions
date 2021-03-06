context("Heikin Ashi tests")

load(file.path("ADM.rda"))

test_that("test ha class", {
  ha_ADM <- heikin_ashi(ADM)
  expect_s3_class(ha_ADM, "xts")
})

test_that("Test HA Close", {
  mean <- as.vector((ADM$ADM.Open + ADM$ADM.High + ADM$ADM.Low + ADM$ADM.Close) / 4)
  expect_equal(rowMeans(quantmod::OHLC(ADM)), mean)
})

test_that("Test HA parameters", {
  expect_error(heikin_ashi(mtcars))
  expect_error(heikin_ashi(ADM$ADM.Close))
  
  # test error on NA values
  ADM$ADM.Open[1] <- NA
  expect_error(heikin_ashi(ADM))
})




