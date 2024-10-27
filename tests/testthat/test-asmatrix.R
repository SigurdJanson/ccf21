

test_that("as.matrix.ccf provides a matrix with correct dimensions", {
  .lags <- 5

  ##
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = .lags, type = "correlation" )
  o <- as.matrix(result)
  expect_type(o, "double")
  expect_equal(class(o), c("matrix", "array"))
  expect_equal(ncol(o), 2L*.lags+1L)
  expect_equal(nrow(o), 3L)

  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = .lags, type = "correlation", ci=0.95)
  o <- as.matrix(result)
  expect_type(o, "double")
  expect_equal(class(o), c("matrix", "array"))
  expect_equal(ncol(o), 2L*.lags+1L)
  expect_equal(nrow(o), 5L)
})


test_that("as.matrix.ccf uses argument `rownames.force`", {
  .lags <- 5
  result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = .lags, type = "correlation", ci=0.95)

  ##
  o <- as.matrix(result, rownames.force = TRUE)
  expect_equal(row.names(o), c("lag", "ccf", "ci.up_0.95", "ci.lo_0.95", "n"))

  o <- as.matrix(result, rownames.force = FALSE)
  expect_equal(row.names(o), NULL)
})
