
#' summary.ccf
#' Summary for a cross-correlation object `ccf`.
#' @param obj an object of class `ccf`.
#' @param digits the number of significant digits to use when printing.
#'
#' @return This function computes and returns a list of summary statistics:
#' \describe{
#'   \item{type}{Used statistics correlation or covariance.}
#'   \item{smethod}{The shifting method used in call to `ccf`.}
#'   \item{nacount}{The number of NAs in the statistics.}
#'   \item{range}{The number of observations in sequence.}
#'   \item{min}{Minimum correlation/covariance found.}
#'   \item{minpos}{Index positions where the maxima were found.}
#'   \item{max}{Maximum correlation/covariance found.}
#'   \item{maxpos}{Index positions where the maxima were found.}
#' }
#' 
#' @export
#' @author Jan Seifert
#' @seealso  [base::summary()]
#' @examples summary(ccf(1:10, 10:1))
summary.ccf <- function(obj, digits) {
  if (is.null(obj)) return(NULL)
  if (missing(digits)) digits = 4L
  
  # Correlations or Covariance
  sumry <- list()
  sumry$type    <- obj$type   
  sumry$smethod <- list(obj$shiftaction, 
                        ifelse(obj$shiftaction == "replace", 
                               obj$replacedby, 
                               NA_real_)) 
  # Count NAs
  sumry$nacount <- sum(is.na(obj$acf)) 
  # Range of correlations
  sumry$range  <- range(obj$acf)
  # Maximum/minimum correlation at which lags?
  absval <- abs(obj$acf)
  sumry$min    <- obj$acf[ which.min(absval) ]
  sumry$minpos <- obj$lag[ which.min(absval) ]
  sumry$minci  <- obj$acf.ci[ which.min(absval), ]
  sumry$max    <- obj$acf[ which.max(absval) ]
  sumry$maxpos <- obj$lag[ which.max(absval) ]
  sumry$maxci  <- obj$acf.ci[ which.max(absval), ]

  class(sumry) <- c("summaryccf")
  return(sumry)
}



#' print.summaryccf
#' Prints the summary of a `ccf` object.
#' @param x an object of class "`summaryccf`", usually, a result of 
#' a call to `summary.ccf`.
#' @param digits the number of significant digits to use when printing.
#'
#' @export
#'
#' @seealso  `[base::print()]`
#' @author Jan Seifert
#' @examples print( summary(ccf(1:10, 10:1)) )
print.summaryccf <- function (x, digits = max(3, getOption("digits") - 3L)) {
  type <- match(x$type, c("correlation", "covariance"))
  abbrev <- c("Cor", "Cov")
  
  # = Definition section =
  # 1. Compile it
  info <- c("Type", "Shift method")
  sumry <- array("", c(length(info), 1L), list(info, c("")))
  sumry[1L, 1L] <- type
  if (x$smethod[[1]] == "replace") # Method Replace with ...
    sumry[2L, 1L] <- paste0("replace with \"", x$smethod[[2]], "\"") 
  else # Method: Wrap, Cut, Imprison
    sumry[2L, 1L] <- x$smethod[[1]]
  
  # 2. Print it
  cat("Definition:\n")
  print.table(sumry)
  
  # = Content section =
  # 1. Compile it
  info <- c("NAs", "Range", paste("Minimum", abbrev[type]), 
            paste("Maximum", abbrev[type]))
  sumry <- array("", c(length(info), 1L), list(info, c("")))
  sumry[1L, 1L] <- x$nacount
  sumry[2L, 1L] <- paste(signif(x$range, digits), collapse = " to ")
  # is outside confidence interval? Mark maximum with '*'.
  chrsig <- ifelse(prod(x$minci) > 0, "*", "")
  sumry[3L, 1L] <- paste0( signif(x$min, digits), chrsig,
                           " at lag ", 
                           toString(signif(x$minpos, digits)) )
  # is outside confidence interval? Mark maximum with '*'.
  chrsig <- ifelse(prod(x$maxci) > 0, "*", "")
  sumry[4L, 1L] <- paste0( signif(x$max, digits), chrsig,
                           " at lag ", 
                           toString(signif(x$maxpos, digits)) )
  
  # 2. Print it
  cat("\n\nValues:\n")
  print.table(sumry)
  invisible(x)
}

# y <- summary(o)
# print(y)