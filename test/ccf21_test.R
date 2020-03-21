library(testthat)
source("../R/ccf21.R")


test_that("ccf21: PRECONDITIONS", {
  # Check if matrices throw an error
  expect_error(ccf( cbind(1:2, 1:2), 1:2 ), 
               "Only univariate data is allowed")
  expect_error(ccf( 1:2, cbind(1:2, 1:2) ), 
               "Only univariate data is allowed")
  
  # Check na.action
  expect_error(ccf( 1:2, 1:2, na.action = na.fail, replaceby = NA ), 
               "Cannot use 'replacement' NA when 'na.action' is fail")
  # 
  expect_error(ccf( 1:2, 1:2, lag.max = -1 ), 
               "'lag.max' must be at least 0")
  
  # 
  expect_error(ccf( 1:5, 1:2, shiftaction = "wrap" ), 
               "Unequal length of 'x' and 'y' works only with 'cut' or 'imprison'")
  expect_error(ccf( 1:2, 1:5, shiftaction = "replace" ), 
               "Unequal length of 'x' and 'y' works only with 'cut' or 'imprison'")
})


test_that("ccf21: shiftaction = \"cut\"", {
  # Simple test: test with max.lag = 0: cc21 == cor
  
  # Test with time series at at least two different frequencies
  
})


test_that("ccf21: shiftaction = \"wrap\"", {
  
})


test_that("ccf21: shiftaction = \"replace\"", {
  
})


test_that("ccf21: shiftaction = \"imprison\"", {
  
})