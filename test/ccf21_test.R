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
  # Simple test: test with max.lag = 0: ccf21 == cor / cov
  for(i in 1:20) {
    x <- rnorm(20)
    y <- rnorm(20)
    result <- ccf( x, y, shiftaction = "cut", lag.max = 0 )
    expect_equal(result$acf[,,1], cor(x, y))
    
    result <- ccf( x, y, shiftaction = "cut", lag.max = 0, type = "covariance" )
    expect_equal(result$acf[,,1], cov(x, y))
  }
  
  # Simple test: nonstationary correlation is 1 between 1:n x 1:n
  # - correlation, lag = 5
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5 )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], rep(1, 11))
  expect_equal(result$lag[,,1], -5:5)
  expect_identical(result$stationary, FALSE)
  expect_null(result$replaceby)
  expect_identical(result$n.used, 10L)
  expect_identical(result$type, "correlation")

  # - covariance, lag = 5
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, type = "covariance" )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], 
               c( c(2.5, 3.5, 4.66666666666667, 6, 7.5), # negative lag
                  9.166666666667,                        # lag = 0
                  rev(c(2.5, 3.5, 4.66666666666667, 6, 7.5)))) # positive
  expect_equal(result$lag[,,1], -5:5)
  expect_identical(result$stationary, FALSE)
  expect_null(result$replaceby)
  expect_identical(result$n.used, 10L)
  expect_identical(result$type, "covariance")
  
  # - correlation, lag = 3
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 3 )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], rep(1, 7))
  expect_equal(result$lag[,,1], -3:3)
  expect_identical(result$stationary, FALSE)
  expect_null(result$replaceby)
  expect_identical(result$n.used, 10L)
  expect_identical(result$type, "correlation")
  
  # - covariance, lag = 3
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 3, type = "covariance" )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], 
               c( c(4.66666666666667, 6, 7.5),       # negative lag
                  9.166666666667,                    # lag = 0
                  rev(c(4.66666666666667, 6, 7.5)))) # positive
  expect_equal(result$lag[,,1], -3:3)
  expect_identical(result$stationary, FALSE)
  expect_null(result$replaceby)
  expect_identical(result$n.used, 10L)
  expect_identical(result$type, "covariance")
  

  # Time series with at least two different frequencies
  # - Non-stationary - frequency > 1
  x <- ts(1:10, frequency = 2)
  y <- ts(1:10, frequency = 2)
  result <- ccf( x, y, shiftaction = "cut", lag.max = 5, stationary = FALSE )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], rep(1, 11))
  expect_equal(result$lag[,,1], seq(-2.5, 2.5, by = 0.5))
  # - Non-stationary - frequency < 1
  x <- ts(1:10, frequency = 0.1)
  y <- ts(1:10, frequency = 0.1)
  result <- ccf( x, y, shiftaction = "cut", lag.max = 5, stationary = FALSE )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], rep(1, 11))
  expect_equal(result$lag[,,1], seq(-50, 50, by = 10))
})


test_that("ccf21: shiftaction = \"wrap\"", {
  expect_equal(0, 0)
})


test_that("ccf21: shiftaction = \"replace\"", {
  expect_equal(0, 0)
  
})


test_that("ccf21: shiftaction = \"imprison\"", {
  expect_equal(0, 0)
  
})