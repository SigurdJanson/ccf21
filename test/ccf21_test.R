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
  
  # time series with different frequencies
  expect_error(ccf( ts(1:5, frequency = 0.9), 
                    ts(1:5, frequency = 0.9+getOption("ts.eps")+.Machine$double.eps), 
                    shiftaction = "wrap" ), 
               "The time series have different frequencies")
  
})


# ONLY non-stationary + cut is being tested, so far

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
  expect_null(result$replacedby)
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
  expect_null(result$replacedby)
  expect_identical(result$n.used, 10L)
  expect_identical(result$type, "covariance")
  
  # - correlation, lag = 3
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 3 )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], rep(1, 7))
  expect_equal(result$lag[,,1], -3:3)
  expect_identical(result$stationary, FALSE)
  expect_null(result$replacedby)
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
  expect_null(result$replacedby)
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
  # Correlation = 0 for all wraps, maximum possible lag (i.e. lag = len/2 - 1)
  result <- ccf( rep(1:2, 10), rep(c(1, 1, 2, 2), 5), shiftaction = "wrap", lag.max = 9 )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], rep(0, 19))
  expect_equal(result$lag[,,1], -9:9)
  expect_identical(result$stationary, FALSE)
  expect_null(result$replacedby)
  expect_identical(result$n.used, 20L)
  expect_identical(result$type, "correlation")
  
  # Same test, stationarity assumption should not make a difference, 
  # same results expected
  result <- ccf( rep(1:2, 10), rep(c(1, 1, 2, 2), 5), shiftaction = "wrap", 
                 lag.max = 9, stationary = TRUE)
  expect_equal(result$acf[,,1], rep(0, 19))
  
  # cor(c(1:2, 3, 1:2), c(1:2, 3, 1:2)), maximum possible lag
  e <- c(-0.071428571428571425, -0.42857142857142849, 1, 
         -0.42857142857142849, -0.071428571428571425)
  result <- ccf( c(1:2, 3, 1:2), c(1:2, 3, 1:2), shiftaction = "wrap", lag.max = 2 )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], e)
  
  # Same test, stationarity assumption should not make a difference, 
  # same results expected
  e <- c(-0.071428571428571425, -0.42857142857142849, 1, 
         -0.42857142857142849, -0.071428571428571425)
  result <- ccf( c(1:2, 3, 1:2), c(1:2, 3, 1:2), shiftaction = "wrap", 
                 lag.max = 2, stationary = TRUE )
  expect_equal(result$acf[,,1], e)
})


test_that("ccf21: shiftaction = \"replace\"", {
  e <- c(-0.53452248382484879, -0.1336306209562122, 0.045834924851410566, -0.36689969285267132, 
         1, 
         0.10482848367219182, 0.045834924851410566, -0.1336306209562122, 0.1336306209562122)
  result <- ccf( c(1:2, 3, 1:2), c(1:2, 3, 1:2), lag.max = 4, 
                 shiftaction = "replace", replaceby = 0L )
  expect_equal(result$acf[,,1], e)
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$lag[,,1], -4:4)
  expect_identical(result$stationary, FALSE)
  expect_identical(result$replacedby, 0L)
  expect_identical(result$n.used, 5L)
  expect_identical(result$type, "correlation")
  
  # replace = NA should return NA for all lags except 0
  e <- c(rep(NA, 8), 0, rep(NA, 8))
  result <- ccf( rep(1:2, 10), rep(c(1, 1, 2, 2), 5), shiftaction = "replace", 
            lag.max = 8, replaceby = NA, na.action = na.pass )
  expect_equal(result$acf[,,1], e)
  
  # Negative correlation -1, correlation
  e <- c(rep(NA, 9), -1, rep(NA, 9))
  result <- ccf( 1:10, 10:1, shiftaction = "replace", 
                 lag.max = 9, replaceby = NA, na.action = na.pass )
  expect_equal(result$acf[,,1], e)
  # Negative correlation -1, Covariance
  e <- c(rep(NA, 9), -9.16666666666667, rep(NA, 9))
  result <- ccf( 1:10, 10:1, shiftaction = "replace", 
                 lag.max = 9, replaceby = NA, na.action = na.pass,
                 type = "covariance")
  expect_equal(result$acf[,,1], e)
})


test_that("ccf21: shiftaction = \"imprison\"", {
  # lag.max parameter is not required - should be ignored
  # Correlation is 0
  result <- ccf( rep(1:2, 10), rep(c(1, 1, 2, 2)), shiftaction = "imprison", lag.max = 9 )
  expect_s3_class(result, c("ccf", "acf"), exact = TRUE)
  expect_equal(result$acf[,,1], rep(0, 16))
  expect_equal(result$lag[,,1], 0:15)
  expect_identical(result$stationary, FALSE)
  expect_null(result$replacedby)
  expect_identical(result$n.used, 20L)
  expect_identical(result$type, "correlation")
  
  # lag.max parameter is not required - should be ignored
  # Correlation = 1
  result <- ccf( 1:10, 1:4, shiftaction = "imprison", lag.max = 0 )
  expect_equal(result$acf[,,1], rep(1, 6))
  expect_equal(result$lag[,,1], 0:5)
  
  # Add stationarity, correlation = 1
  result <- ccf( 1:10, 1:4, shiftaction = "imprison", lag.max = 1 )
  expect_equal(result$acf[,,1], rep(1, 6))
  expect_equal(result$lag[,,1], 0:5)
})


test_that("ccf21: compare with stats::ccf", {
  # Sinus
  t <- seq(0, 4*pi, length.out = 100)
  x <- sin(t)
  y <- sin(t - pi/4)
  e <- stats::ccf(x, y, plot = FALSE)
  o <- ccf(x, y, stationary = TRUE, plot = FALSE) # set stationary explicitely!
  
  expect_s3_class(o, c("ccf", "acf"), exact = TRUE)
  expect_equal(o$acf[,,1], e$acf[,,1])
  expect_equal(o$lag[,,1], e$lag[,,1])
  expect_identical(o$stationary, TRUE)
  expect_null(o$replacedby)
  expect_identical(o$n.used, e$n.used)
  expect_identical(o$type, e$type)
  
  # Sinus, covariance
  e <- stats::ccf(x, y, type = "covariance", plot = FALSE)
  o <- ccf(x, y, type = "covariance", stationary = TRUE, plot = FALSE)
  expect_equal(o$acf[,,1], e$acf[,,1])
  
  # Sinus, correlation, frequency != 1
  x <- ts(sin(t), frequency = 0.34858)
  y <- ts(sin(t - pi/4), frequency = 0.34858)
  e <- stats::ccf(x, y, plot = FALSE)
  o <- ccf(x, y, plot = FALSE) # set stationary implicitely!
  expect_equal(o$acf[,,1], e$acf[,,1])
  expect_equal(o$lag[,,1], e$lag[,,1])
  
  
  # Random data
  x <- rnorm(100)
  y <- rnorm(100)
  e <- stats::ccf(x, y, plot = FALSE)
  o <- ccf(x, y, stationary = TRUE, plot = FALSE) # set stationary explicitely!
  
  expect_equal(o$acf[,,1], e$acf[,,1])
  expect_equal(o$lag[,,1], e$lag[,,1])

  # Random data, covariance
  e <- stats::ccf(x, y, type = "covariance", plot = FALSE)
  o <- ccf(x, y, type = "covariance", stationary = TRUE, plot = FALSE)
  expect_equal(o$acf[,,1], e$acf[,,1])
})

