#library(testthat)
# source("../R/ccf21.R")
# source("../R/printccf21.R")


test_that("print.ccf21", {
  # non-stationary result: all r = 1
  # note: for some unknown reason print.default prints only
  # as much decimals as needed. It ignores 'digits'. It
  # does the final printing.
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, plot = FALSE )
  expect_output(print(result), "Cross-Correlation")
  expect_output(print(result), "lag cor")
  for (lag in -5:5)
    expect_output(print(result), paste0(lag, "\\s*", 1))

  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5,
                 type = "covariance", plot = FALSE )
  expect_output(print(result), "Cross-Covariance")
  expect_output(print(result), "lag cov")
  #
  e <- c(2.0, 2.9166666, 4.0, 5.25, 6.666666, 8.25, 6.666666, 5.25, 4.0, 2.9166666, 2.0)
  # default for 'digits' is (7-3)
  for (lag in -5:5)
    expect_output(print(result), paste0(lag, "\\s*", format(e[lag+6L], digits=4)))

  for (lag in -5:5)
    expect_output(print(result, digits = 2), paste0(lag, "\\s*", format(e[lag+6L], digits=2)))

  for (lag in -5:5)
    expect_output(print(result, digits=6), paste0(lag, "\\s*", format(e[lag+6L], digits=6L)))
})

