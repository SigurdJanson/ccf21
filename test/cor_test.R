library(testthat)
source("../R/ccf21.R")

# Correlations
test_that(".Cor_ccf", {
  expect_error(.Cor_ccf(1:10, 1:9),
               "Correlations assume two vectors of equal length")
  expect_error(.Cor_ccf(1:10, 10:1, type = "xyz"),
               "'arg' should be one of")
  
  # For standard normal variables: correlation = covariance
  # Mean and sd have to be set: great way to test this
  for(i in 1:100) {
    # Create two random vectors
    N     <- runif(1, 4, 100)
    Rho   <- runif(1, -0.99, +0.99)
    Sigma <- 1 #runif(1, 0.5, 20)
    Mu    <- 0 #runif(1, -10, -10)
    Comm  <- rnorm(N, Mu, Sigma)
    ErrX  <- rnorm(N, Mu, Sigma)
    ErrY  <- rnorm(N, Mu, Sigma)
    X <- Comm * sqrt(abs(Rho)) + ErrX * sqrt(1-abs(Rho))
    Y <- Comm * sqrt(abs(Rho)) + ErrY * sqrt(1-abs(Rho))
    
    # 
    o <- .Cor_ccf(X, Y, "correlation", N, Mu, Sigma)
    e <- .Cor_ccf(X, Y, "covariance",  N, Mu, Sigma)
    expect_identical(o, e)
  }
  
  # Tests using predefined distributions
  o <- .Cor_ccf(1:10, 21:30, "correlation")
  e <- 1
  expect_equal(o, e)
  
  o <- .Cor_ccf(10:1, 21:30, "correlation")
  e <- -1
  expect_equal(o, e)
  
  # Test by comparison to R-functions cor and cov
  for (i in 1:100) {
    N     <- runif(1, 4, 100)
    Rho   <- runif(1, -0.99, +0.99)
    Sigma <- runif(1, 0.5, 20)
    Mu    <- runif(1, -10, -10)
    Comm  <- rnorm(N, Mu, Sigma)
    ErrX  <- rnorm(N, Mu, Sigma)
    ErrY  <- rnorm(N, Mu, Sigma)
    X <- Comm * sqrt(abs(Rho)) + ErrX * sqrt(1-abs(Rho))
    Y <- Comm * sqrt(abs(Rho)) + ErrY * sqrt(1-abs(Rho))
    
    o <- .Cor_ccf(X, Y, "correlation")
    expect_equal(o, cor(X, Y))
    o <- .Cor_ccf(X, Y, "covariance")
    expect_equal(o, cov(X, Y))
  }
})