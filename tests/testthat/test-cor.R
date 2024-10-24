library(testthat)
#source("../R/ccf21.R")


test_that(".Cor_ccf checks assumptions", {
  expect_error(.Cor_ccf(1:10, 1:9),
               "Correlations assume two vectors of equal length")
  expect_error(.Cor_ccf(1:10, 10:1, type = "xyz"),
               "'arg' should be one of")
})


test_that(".Cor_ccf gives correct results with simple predefined (but complete) series", {
  # Tests using simple predefined distributions

  #
  # Correlation = 1, no error variance
  x <- 1:10
  y <- 21:30

  o <- .Cor_ccf(x, y, "covariance")
  e <- 82.5/10 # the product sum divided by n
  expect_equal(o, e)

  o <- .Cor_ccf(x, y, "correlation")
  e <- 1
  expect_equal(o, e)

  #
  # Correlation = -1, no error variance
  x <- 10:1
  y <- 21:30

  o <- .Cor_ccf(x, y, "covariance")
  e <- -82.5/10 # the product sum divided by n
  expect_equal(o, e)

  o <- .Cor_ccf(x, y, "correlation")
  e <- -1
  expect_equal(o, e)


  #
  # Correlation = -0.912
  x <- 10:1
  y <- c(-5:-1, 26:30)

  o <- .Cor_ccf(x, y, "covariance")
  e <- -407.5/10 # the product sum divided by n
  expect_equal(o, e)

  o <- .Cor_ccf(x, y, "correlation")
  e <- e / sd(x) / sd(y)  *10/9  # correct the denominator
  expect_equal(o, e)
})



test_that(".Cor_ccf gives correct results with predefined and incomplete series", {
  # Tests using simple predefined distributions

  #
  # Stationary data
  x <- 1:14
  y <- 21:34

  o <- .Cor_ccf(x[3:12], y[3:12], "covariance", n = 14, mean = c(mean(x), mean(y)), sd = c(sd(x), sd(y)))
  e <- 82.5/14 # the product sum divided by n
  expect_equal(o, e)

  o <- .Cor_ccf(x[3:12], y[3:12], "correlation", n = 14, mean = c(mean(x), mean(y)), sd = c(sd(x), sd(y)))
  e <- 82.5/13 / sd(x) / sd(y) # the product sum divided by (n-1) divided by sd
  expect_equal(o, e)

  #
  # Non-stationary data
  x <- 1:14
  y <- 21:34

  o <- .Cor_ccf(x[3:12], y[3:12], "covariance")
  e <- 82.5/10 # the product sum divided by n
  expect_equal(o, e)

  o <- .Cor_ccf(x[3:12], y[3:12], "correlation")
  e <- 1.00
  expect_equal(o, e)

})


test_that(".Cor_ccf gives identical results when correlation = covariance", {

  # For standard normal variables: correlation = covariance
  # - Mean and sd have to be set (great way to test this)
  # - Systematically check increasing vector lengths
  # Note: stats::ccf uses `n-1` for correlations and `n` for covariances in the denominator
  for(N in (2:25)^2) {
    # Create two random vectors
    #N     <- runif(1, 4, 100)
    Rho   <- runif(1, -0.99, +0.99)
    Sigma <- 1
    Mu    <- 0
    Comm  <- rnorm(N, Mu, Sigma)
    ErrX  <- rnorm(N, Mu, Sigma)
    ErrY  <- rnorm(N, Mu, Sigma)
    X <- Comm * sqrt(abs(Rho)) + ErrX * sqrt(1-abs(Rho))
    Y <- Comm * sqrt(abs(Rho)) + ErrY * sqrt(1-abs(Rho))

    #
    o <- .Cor_ccf(X, Y, "correlation", N, Mu, Sigma)
    e <- .Cor_ccf(X, Y, "covariance",  N-1, Mu, Sigma) # use (N-1) to correct denominator
    expect_identical(o, e)
  }

})



test_that(".Cor_ccf gives same results as stats::cor() and stats::cov()", {

  # Test by comparison to R-functions cor and cov
  # - Systematically check increasing vector lengths
  for (N in (2:25)^2) {
    #N     <- runif(1, 4, 100)
    Rho   <- runif(1, -0.99, +0.99)
    Sigma <- runif(1, 0.5, 20)
    Mu    <- runif(1, -10, -10)
    Comm  <- rnorm(N, Mu, Sigma)
    ErrX  <- rnorm(N, Mu, Sigma)
    ErrY  <- rnorm(N, Mu, Sigma)
    X <- Comm * sqrt(abs(Rho)) + ErrX * sqrt(1-abs(Rho))
    Y <- Comm * sqrt(abs(Rho)) + ErrY * sqrt(1-abs(Rho))

    o <- .Cor_ccf(X, Y, "correlation")
    expect_equal(o, stats::cor(X, Y))
    o <- .Cor_ccf(X, Y, "covariance")
    expect_equal(o, stats::cov(X, Y) *(N-1)/N) # cov uses a (n-1) denominator, ccf uses n
  }
})
