
#' summary.ccf
#' Summary for a cross-correlation object `ccf`.
#' @param object an object of class `ccf`.
#' @param ... additional arguments to be passed to or from methods (unused).
#'
#' @return This function computes and returns a list of summary statistics:
#' \describe{
#'   \item{type}{Used statistics correlation or covariance.}
#'   \item{smethod}{The shifting method used in call to `ccf`.}
#'   \item{lagrange}{The range of the lags.}
#'   \item{nacount}{The number of NAs in the statistics.}
#'   \item{range}{The range of values in sequence.}
#'   \item{min}{Minimum correlation/covariance found.}
#'   \item{minpos}{Index positions where the maxima were found.}
#'   \item{minci}{Lower confidence limit at `minpos`.}
#'   \item{max}{Maximum correlation/covariance found.}
#'   \item{maxpos}{Index positions where the maxima were found.}
#'   \item{maxci}{Upper confidence limit at `maxpos`.}
#' }
#'
#' @export
#' @author Jan Seifert
#' @seealso  `[base::summary()]`
#' @examples summary(ccf(1:10, 10:1))
summary.ccf <- function(object, ...) {
  if (is.null(object)) return(NULL)

  # Correlations or Covariance
  sumry <- list()
  sumry$type    <- object$type
  sumry$smethod <- list(object$shiftaction,
                        ifelse(object$shiftaction == "replace",
                               object$replacedby,
                               NA_real_))
  # Range of lags
  sumry$lagrange  <- range(object$lag)
  # Range of correlations
  sumry$range  <- range(object$acf)
  # Count NAs
  sumry$nacount <- sum(is.na(object$acf))
  # Maximum/minimum correlation at which lags?
  absval <- abs(drop(object$acf))
  sumry$min    <- object$acf[ which.min(absval) ]
  sumry$minpos <- object$lag[ which(absval == min(absval)) ]
  if(!is.na(object$acf.ci))
    sumry$minci  <- object$acf.ci[ which.min(absval), 1 ]
  else
    sumry$minci <- NA
  sumry$max    <- object$acf[ which.max(absval) ]
  sumry$maxpos <- object$lag[ which(absval == max(absval)) ]
  if(!is.na(object$acf.ci))
    sumry$maxci  <- object$acf.ci[ which(absval == max(absval)), 2 ]
  else
    sumry$maxci <- NA

  class(sumry) <- c("summaryccf")
  return(sumry)
}



#' print.summaryccf
#' Prints the summary of a `ccf` object.
#' @param x an object of class "`summaryccf`", usually, a result of
#' a call to `summary.ccf`.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to print methods (here `[base::print.table()]`).
#'
#' @export
#'
#' @seealso  `[base::print.table()]`, `[base::print()]`
#' @author Jan Seifert
#' @examples print( summary(ccf(1:10, 10:1)) )
print.summaryccf <- function (x, digits = max(3, getOption("digits") - 3L), ...) {
  type <- match(x$type, c("correlation", "covariance"))
  abbrev <- c("Cor", "Cov")

  # = Definition section =
  # 1. Compile it
  info <- c("Type", "Shift method", "Range of lags")
  sumry <- array("", c(length(info), 1L), list(info, c("")))
  sumry[1L, 1L] <- abbrev[type]
  if (x$smethod[[1]] == "replace") # Method Replace with ...
    sumry[2L, 1L] <- paste0("replace with \"", x$smethod[[2]], "\"")
  else # Method: Wrap, Cut, Imprison
    sumry[2L, 1L] <- x$smethod[[1]]
  sumry[3L, 1L] <- paste(signif(x$lagrange, digits), collapse = " to ")

  # 2. Print it
  cat("Definition:\n")
  print.table(sumry, ...)

  # = Content section =
  # 1. Compile it
  info <- c("NAs", "Range", paste("Minimum", abbrev[type]),
            paste("Maximum", abbrev[type]))
  sumry <- array("", c(length(info), 1L), list(info, c("")))
  sumry[1L, 1L] <- x$nacount
  sumry[2L, 1L] <- paste(signif(x$range, digits), collapse = " to ")
  # is outside confidence interval? Mark maximum with '*'.
  if (!is.na(x$minci))
    chrsig <- ifelse(prod(x$minci) > 0, "*", "")
  else
    chrsig <- ""
  sumry[3L, 1L] <- paste0( signif(x$min, digits), chrsig,
                           " at lag ",
                           toString(signif(x$minpos, digits)) )
  # is outside confidence interval? Mark maximum with '*'.
  if (!is.na(x$maxci))
    chrsig <- ifelse(prod(x$maxci) > 0, "*", "")
  else
    chrsig <- ""
  sumry[4L, 1L] <- paste0( signif(x$max, digits), chrsig,
                           " at lag ",
                           toString(signif(x$maxpos, digits)) )

  # 2. Print it
  cat("\n\nValues:\n")
  print.table(sumry, ...)
  invisible(x)
}

