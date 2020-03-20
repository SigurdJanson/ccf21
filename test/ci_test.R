library(testthat)
source("../R/ccf21.R")


#.CorConf_Fisher( r, n, ci = 0.95 )
test_that("", {
  # Check range of definition
  expect_error(.CorConf_Fisher( n = 100, ci = 0.95 ), 
               "Pearson correlation 'r' required")
  expect_error(.CorConf_Fisher( r = 0, ci = 0.95 ), 
               "Sample size 'n' required")

  expect_error(.CorConf_Fisher( r = +1+.Machine$double.eps, n = 100 ), 
               "Correlations are only defined between -1 and 1")
  expect_error(.CorConf_Fisher( r = -1-.Machine$double.eps, n = 100 ), 
               "Correlations are only defined between -1 and 1")

  expect_error(.CorConf_Fisher( r = +1, n = 100, ci = 1+.Machine$double.eps ), 
               "Confidence is only defined between 0 and 1")
  expect_error(.CorConf_Fisher( r = +1, n = 100, ci = 0-sqrt(.Machine$double.eps) ), 
               "Confidence is only defined between 0 and 1")
  
  # Confidence range for increasing N must become monotonously smaller
  N <- 20
  for(i in 1:100) {
    r <- runif(1, -0.99, 0.99)
    ci <- runif(1, -0.99, 0.99)
    o <- .CorConf_Fisher( r = rep(0.5, N), n = seq(N, by = N, length.out = N), ci = 0.95 )
    o <- o[,2] - o[,1]
    expect_identical(o[2:N] <= o[1:(N-1)], rep(TRUE, N-1))
  }
  
  # Confidence range for increasing ci must become monotonously larger
  N <- 20
  for(i in 1:100) {
    r <- runif(1, -0.99, 0.99)
    n <- runif(1, 1, 1000)
    o <- .CorConf_Fisher( r = r, n = 50, ci = seq(0.01, to = 0.99, length.out = N) )
    o <- o[,2] - o[,1]
    expect_identical(o[2:N] >= o[1:(N-1)], rep(TRUE, N-1))
  }
  
  # Confidence of r = 0 only depends on width of normal distribution
  for(i in 1:20) {
    ci <- runif(1, 0.0001, 0.9999)
    n <- runif(1, 4, 1000)
    # High precision version because it uses same algorithm
    o <- .CorConf_Fisher( r = 0, n = n, ci = ci )
    e <- qnorm( (ci+1)*0.5, sd = 1 )
    e <- cbind(-e, +e) # 
    e <- e * sqrt(1/(n-3)) # scale to std dev. of normal distribution
    e <- (exp(2*e)-1) / (exp(2*e)+1) # inverse Fisher transform
    expect_identical(o, e)
    # Less accurate version that uses the sd of the normal distr.
    o <- .CorConf_Fisher( r = 0, n = n, ci = ci )
    sd <- 1 / sqrt(n-1.5)
    e <- qnorm( (ci+1)*0.5, sd = sd )
    e <- cbind(-e, +e) # 
    e <- (exp(2*e)-1) / (exp(2*e)+1) # inverse Fisher transform
    expect_equal(o, e, tolerance = 1E-3)
    #
    expect_equal(o[,1], -o[,2]) # must be symmetrical around 0
  }

  
  # Confidence interval for 1/-1 is [1,1] / [-1,-1]
  N <- 5  
  expect_equal(.CorConf_Fisher( r = rep(1, N), n = 100, ci = 0.95 ),
               cbind(rep(1, N), rep(1, N)))
  expect_equal(.CorConf_Fisher( r = rep(-1, N), n = 100, ci = 0.95 ),
               cbind(rep(-1, N), rep(-1, N)))
})