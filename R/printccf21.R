#' print.ccf
#' Prints a cross-correlation object.
#' @param x an object to be printed.
#' @param digits minimal number of significant digits
#' @param ... further arguments passed to print methods.
#'
#' @export
#' @seealso  [base::print,data.frame()]
#' @examples print(ccf(1:10, 10:1))
print.ccf <- function (x, digits = max(3, getOption("digits") - 3L), ...) {
  type <- match(x$type, c("correlation", "covariance"))
  msg <- c("Cross-Correlations", "Cross-Covariances")
  cat("\n", msg[type], " of series ", sQuote(x$series), " by lag\n\n", sep = "")
  
  # is 0 outside of confidence interval?
  type <- c("cor", "cov")[type]
  asterisk <- apply(x$acf.ci, 1, function(x) ifelse(prod(x) > 0, "*", "") )
  ccfs <- data.frame(drop(x$lag), drop(x$acf), asterisk)
  colnames(ccfs) <- c("lag", type, "sign.")
  print(ccfs, digits = digits, right = FALSE, row.names = FALSE, ...)
}
