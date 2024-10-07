#library(testthat)
# source("../R/ccf21.R")
# source("../R/printccf21.R")


test_that("print.ccf21", {
  # non-stationary result: all r = 1
  # note: for some unknown reason print.default prints only
  # as much decimals as needed. It ignores 'digits'. It
  # does the final printing.
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5,
                 plot = FALSE )
  expect_output(print(result), "Cross-Correlation")
  expect_output(print(result), "lag cor")
  expect_output(print(result), "-5  1")
  expect_output(print(result), "0  1")
  expect_output(print(result), "5  1")

  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5,
                 type = "covariance", plot = FALSE )
  expect_output(print(result), "Cross-Covariance")
  expect_output(print(result), "lag cov")
  #
  expect_output(print(result, digits = 2), "-5  2.5")
  expect_output(print(result, digits = 2),  "0  9.2")
  expect_output(print(result, digits = 2),  "5  2.5")
  # default for 'digits' is (7-3)
  expect_output(print(result), "-5  2.500")
  expect_output(print(result),  "0  9.167")
  expect_output(print(result),  "5  2.500")
  #
  expect_output(print(result, digits = 6), "-5  2.50000")
  expect_output(print(result, digits = 6),  "0  9.16667")
  expect_output(print(result, digits = 6),  "5  2.50000")
})

