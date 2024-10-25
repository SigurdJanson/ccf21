#library(testthat)
#source("../R/ccf21.R")
#source("../R/summaryccf21.R")


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

  expect_equal(sumry$range, c(2.0, 8.25))
  expect_equal(sumry$max, 8.25)
  expect_equal(sumry$maxpos, 0)
  # maxci
  expect_equal(sumry$min, 2.0)
  expect_equal(sumry$minpos, c(-5, +5))
  # minci
  expect_equal(sumry$nacount, 0)
  expect_equal(sumry$smethod, list("cut", NA_real_))
  expect_equal(sumry$type, "covariance")
  expect_equal(sumry$lagrange, c(-5, +5))
})


##
test_that("print.summary.ccf21", {
  # Covariance: to check if labels are correct
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, type = "covariance", stationary = TRUE )
  sumry <- summary(result)
  expect_output(print(sumry), "Definition:")
  expect_output(print(sumry), "Type          Cov")
  expect_output(print(sumry), "Shift method  cut")
  expect_output(print(sumry), "Range of lags -5 to 5")
  expect_output(print(sumry), "Values:")
  expect_output(print(sumry), "Range       -2.125 to 8.25")
  expect_output(print(sumry), "Minimum Cov -0.65 at lag -4, 4")
  expect_output(print(sumry), "Maximum Cov 8.25 at lag 0")

  # Correlation: to check if labels are correct
  # I do not have exact value, for now
  result <- ccf( 1:10, 1:10, shiftaction = "replace", replaceby = 0,
                 lag.max = 5, type = "correlation" )
  sumry <- summary(result)
  expect_output(print(sumry), "Definition:")
  expect_output(print(sumry), "Type          Cor")
  expect_output(print(sumry), "Shift method  replace")
  expect_output(print(sumry), "Range of lags -5 to 5")
  expect_output(print(sumry), "Values:")
  expect_output(print(sumry), "Range       ")
  expect_output(print(sumry), "Minimum Cor ")
  expect_output(print(sumry), "Maximum Cor 1")
})


