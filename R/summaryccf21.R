
#' summary.ccf
#' Summary for a cross-correlation object `ccf`.
#' @param object an object of class `ccf`.
#' @param digits integer, used for number formatting with `signif()`.
#'
#' @return This method returns an object of class `c("table")`.
#' @export
#' @examples summary(ccf(1:10, 10:1))
summary.ccf <- function(object, digits) {
  if (is.null(object)) return(NULL)
  if (missing(digits)) digits = 4L
  
  type <- match(object$type, c("correlation", "covariance"))
  abbrev <- c("Cor", "Cov")
  
  # List of information
  info <- c("= Definition =", "Type", "Shift method", 
            "",
            "= Values =", "NAs", "Range", paste("Maximum", abbrev[type]))
  # Table object as template
  sumry <- array("", c(length(info), 1L), list(info, c("Details")))
  # Correlations or Covariance
  sumry[2L, 1L] <- object$type   
  if (object$shiftaction == "replace") # Method Replace with ...
    sumry[3L, 1L] <- paste0("replace with \"", object$replacedby, "\"") 
  else # Method: Wrap, Cut, Imprison
    sumry[3L, 1L] <- object$shiftaction 
  # Has NAs? Count them.
  sumry[6L, 1L] <- sum(is.na(object$acf)) 
  # Range of correlations
  sumry[7L, 1L] <- paste(signif(range(object$acf), digits), 
                         collapse = " to ")
  # Maximum absolute correlation at which lags?
  sumry[8L, 1L] <- paste0(signif(max(abs(object$acf)), digits), 
                        " at position(s) ", 
                        toString( which.max(abs(object$acf))) )
  
  class(sumry) <- c("summaryccf", "table")
  sumry
}
