
#' max.ccf
#'
#' @param x An object of class `acf` as returned from `stats::ccf()` or `ccf` as
#' returned from the ccf21 implementation.
#' @param tol A tolerance below the maximum. When `tol != 0` not only the absolute
#' maximum will be returned but all values within the interval `[max+tol, max-tol]`.
#'
#' @return A matrix with two columns. First column contains the correlation / covariances.
#' The second column gives the according lags.
#' @export
max.ccf <- function(x, tol = 0) {
  # PRECONDITIONS
  ClassX <- !is.na(match(c("ccf", "acf"), class(x)))
  if( !ClassX[1] ) # not class "ccf"
    if( !ClassX[2] ) # not class "acf"
      stop("Argument 'x' is not an object of class 'acf' or 'ccf'")
  tol <- abs(tol)

  #
  AbsCor <- abs(x$acf[,,1])
  MaxCor <- max(AbsCor)
  MaxIdx <- which(AbsCor >= MaxCor - MaxCor*tol &
                  AbsCor <= MaxCor + MaxCor*tol)

  Result <- matrix( c( (x$acf[,,1])[MaxIdx], (x$lag[,,1])[MaxIdx] ),
                    byrow = FALSE, ncol = 2,
                    dimnames = list(character(), c("Cor", "Lag")) )
  return(Result)
}


