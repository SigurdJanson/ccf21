library(testthat)
#source("../R/ccf21.R")

test_that(".Shift_ccf", {
  # Errors
  expect_error(.Shift_ccf(1:2, 1, replace = "FALSE"),
               "'replace' must be numeric")
  expect_error(.Shift_ccf(1, -1, replace = 0),
               "When |k| is greater or equal than the length of y")
  expect_error(.Shift_ccf(1, 1, replace = 0),
               "When |k| is greater or equal than the length of y")

  # Cycle-Shift forward
  s <- 1:20
  for ( k in 0:(length(s)-1) ) {
    o <- .Shift_ccf(s, k, replace = TRUE)
    e <- c(seq(20-k+1, 20, length.out = k), seq(1, 20-k))
    expect_equal(o, e)
  }

  # Cycle-Shift backward
  s <- 1:20
  for ( k in 0:(-length(s)+1) ) {
    o <- .Shift_ccf(s, k, replace = TRUE)
    e <- c(seq(-k+1, 20), seq(1, -k, length.out = -k))
    expect_equal(o, e)
  }

  # Replace-Shift forward
  for(r in c(0, 99, NA)) {
    s <- 1:20
    for ( k in 0:(length(s)-1) ) {
      o <- .Shift_ccf(s, k, replace = r)
      e <- c(rep(r, k), seq(1, 20-k))
      expect_equal(o, e)
    }
  }

  # Replace-Shift backward
  for(r in c(0, 99, NA)) {
    s <- 1:20
    for ( k in 0:(-length(s)+1) ) {
      o <- .Shift_ccf(s, k, replace = r)
      e <- c(seq(-k+1, 20), rep(r, -k))
      expect_equal(o, e)
    }
  }

})
