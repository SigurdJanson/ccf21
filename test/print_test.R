library(testthat)
source("../R/ccf21.R")
source("../R/printccf21.R")


test_that("print.ccf21", {
  # non-stationary result: all r = 1
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, 
                 plot = FALSE )
  expect_output(print(result), "Cross-Correlation")
  expect_output(print(result), "lag cor")
  expect_output(print(result), "-5  1.000")
  expect_output(print(result), "0  1.000")
  expect_output(print(result), "5  1.000")
  
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, 
                 type = "covariance", plot = FALSE )
  expect_output(print(result), "Cross-Covariance")
  expect_output(print(result), "lag cov")
  expect_output(print(result), "-5  2.500")
  expect_output(print(result),  "0  9.167")
  expect_output(print(result),  "5  2.500")
})