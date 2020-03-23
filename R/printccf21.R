print.ccf <- function (x, digits = 4L, ...) {
  type <- match(x$type, c("correlation", "covariance"))
  msg <- c("Cross-Correlations", "Cross-Covariances")
  cat("\n", msg[type], " of series ", sQuote(x$series), " by lag\n\n", sep = "")
  
  x$acf <- round(x$acf, digits)
  acfs <- setNames(drop(x$acf), format(drop(x$lag), digits = digits))
  print(acfs, digits = digits, ...)
  invisible(x)
}




 
print.summaryccf <- function (x, digits = 4L, ...) {

}
