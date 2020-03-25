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
  cat("\n", msg[type], " by lag\n\n", sep = "")
  
  type <- c("cor", "cov")[type]
  # is 0 outside of confidence interval?
  if(!is.na(x$acf.ci)) {
    asterisk <- apply(x$acf.ci, 1, function(x) ifelse(prod(x) > 0, "*", "") )
    ccfs <- data.frame(drop(x$lag), drop(x$acf), asterisk)
    colnames(ccfs) <- c("lag", type, "sign.")
  } else{
    ccfs <- data.frame(drop(x$lag), drop(x$acf))
    colnames(ccfs) <- c("lag", type)
  }
  
  print(ccfs, digits = digits, right = FALSE, row.names = FALSE, ...)
  invisible(x)
}

#result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, plot = FALSE )
#print(result)