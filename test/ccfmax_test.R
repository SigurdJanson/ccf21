source("../R/ccf21.R")
source("../R/ccfmax.R")


test_that("max.ccf Preconditions", {
  expect_error(max.ccf(list()),
               "Argument 'x' is not an object of class 'acf' or 'ccf'")
})
  


test_that("max.ccf", {
  # Using vectors
  t <- seq(0, 4*pi, length.out = 25)
  x <- sin(t)
  y <- sin(t - pi/4)
  
  # Test
  o <- ccf(x, y, ci = 0.95, plot = FALSE)
  e <- matrix(c(0.96804555752481436, -2), ncol = 2, 
              dimnames = list(character(), c("Cor", "Lag")))
  expect_equal(max.ccf(o), e)
  
  # Using time series to check if lag index works
  t <- seq(0, 4*pi, length.out = 25)
  x <- ts(sin(t), frequency = 0.34858)
  y <- ts(sin(t - pi/4), frequency = 0.34858)
  
  o <- ccf(x, y, ci = 0.95, plot = FALSE)
  e <- matrix(c(0.94716819940984487, -2.8687819151988068), ncol = 2, 
              dimnames = list(character(), c("Cor", "Lag")))
  expect_equal(max.ccf(o), e)
  
  # Check for a negative correlation as maximum
  t <- seq(0, 4*pi, length.out = 25)
  x <- ts(sin(t), frequency = 0.34858)
  y <- ts(-sin(t - pi/4), frequency = 0.34858)
  
  o <- ccf(x, y, ci = 0.95, plot = FALSE)
  e <- matrix(c(-0.94716819940984487, -2.8687819151988068), ncol = 2, 
              dimnames = list(character(), c("Cor", "Lag")))
  expect_equal(max.ccf(o), e)
})
