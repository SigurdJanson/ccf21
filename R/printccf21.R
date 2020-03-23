#' print.ccf
#' Prints a cross-correlation object.
#' @param x an object to be printed.
#' @param digits minimal number of significant digits
#' @param ... further arguments passed to print methods.
#'
#' @export
#' @seealso  [base::print()]
#' @examples print(ccf(1:10, 10:1))
print.ccf <- function (x, digits = max(3, getOption("digits") - 3L), ...) {
  type <- match(x$type, c("correlation", "covariance"))
  msg <- c("Cross-Correlations", "Cross-Covariances")
  cat("\n", msg[type], " of series ", sQuote(x$series), " by lag\n\n", sep = "")
  
  x$acf <- round(x$acf, digits)
  ccfs <- setNames(drop(x$acf), format(drop(x$lag), digits = digits))
  print(ccfs, digits = digits, ...)
}

