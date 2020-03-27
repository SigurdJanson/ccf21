library(testthat)
source("../R/ccf21.R")
source("../R/summaryccf21.R")


test_that("summary.ccf21: preconditions", {
  expect_null(summary.ccf(NULL))
})
  

test_that("summary.ccf21: formal structure", {
  result <- ccf( 1:10, 1:10, shiftaction = "cut", 
                 lag.max = 5, plot = FALSE )
  expect_s3_class(result, "ccf")
  # defaults
  e_names <- c("type", "smethod", "lagrange", "range", "nacount",
               "min", "minpos", "minci",
               "max", "maxpos", "maxci")
  sumry  <- summary(result)
  expect_s3_class(sumry, "summaryccf")
  expect_length(sumry, length(e_names))
  expect_named(sumry, e_names)
})


test_that("summary.ccf21: content", {
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, type = "covariance" )
  expect_s3_class(result, "ccf")
  sumry  <- summary(result)
  
  expect_equal(sumry$range, c(2.5, 9.166666666667))
  expect_equal(sumry$max, 9.166666666667)
  expect_equal(sumry$maxpos, 0)
  # maxci
  expect_equal(sumry$min, 2.5)
  expect_equal(sumry$minpos, c(-5, +5))
  # minci
  expect_equal(sumry$nacount, 0)
  expect_equal(sumry$smethod, list("cut", NA_real_))
  expect_equal(sumry$type, "covariance")
  expect_equal(sumry$lagrange, c(-5, +5))
})


## 
test_that("print.summary.ccf21", {
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, type = "covariance" )
  expect_output()
})



test_that("print.summary.ccf21", {
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, type = "covariance" )
  
})
