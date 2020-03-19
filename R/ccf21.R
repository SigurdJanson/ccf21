

.Cor_ccf <- function(x, y, n = NA, mean = NA, sttdev = NA) {
  # if mean and stddev are given, treat x and y as stationary time series
  # if not, compute your own mean and stddev
}


.HandleLost_ccf <- function() {
}



#' .Shift_ccf
#' Shifts the elements of a vector by k positions. Empty positions can be replaced
#' by an arbitrary value or the shifting is treated cyclic, i.e. elements shifted out
#' on the one end will come back on the other.
#' @param y 
#' @param k How many elements 'y' to be shifted. k < 0 shifts to the left, k > 0 to the right.
#' @param replace How to treat vector elements that get lost. See details
#' @details Shifting to the left leaves open elements on the right 
#' and vice versa. This function offers three ways to handle the fact that 
#' elements get lost through shifting. 
#' * \code{replace = FALSE}: cut it (today's default), 
#' * \code{replace = TRUE}:  wrap it back assuming the sequence is circular.
#' * \code{replace = as.double(anyhting)}: or fill it with data.
#' @return A new shifted vector. 
.Shift_ccf <- function(y, k, replace = FALSE) {
  # PRECONDITIONS
  if(k == 0) return(y)
  
  ly <- length(y)
  if(abs(k) >= ly) stop("|k| is greater or equal than length of y.
                        There are no values left.")
  if (!is.na(replace) && !isTRUE(replace) && !isFALSE(replace) ) 
    if (!is.numeric(replace)) stop("'replace' must be numeric")

  # OPERATIONS
  y2 <- y[seq(max(-k+1, 1), min(ly, ly-k))]
  if (!isFALSE(replace)) {
    if (isTRUE(replace)) {
      if(k > 0)
        rp <- y[seq(ly-k+1, ly)]
      else
        rp <- y[1:(-k)]
    } else {
      rp <- rep(replace, abs(k))
    }
    if (k > 0)
      y2 <- c(rp, y2)
    else
      y2 <- c(y2, rp)
  }

  # FINISH
  return(y2)
}


#'
#'
#'
#' 
ccf <- function (x, y, lag.max = NULL, type = c("correlation", "covariance"), 
                 stationary = TRUE, 
                 protrusionaction = NULL, 
                 windowaction = c("cut", "imprison"), # ignored if length(x) == length(y)
                 plot = TRUE, na.action = na.fail, ...)  {
  # PRECONDITIONS
  if (is.matrix(x) || is.matrix(y)) 
    stop("univariate time series only.")
  if ( (is.ts(x) || is.ts(y)) && !stationary )
    stop("Conflicting arguments x/y and 'stationary': time series objects shall be assumed to be stationarity.")
  
  LenX <- length(x)
  LenY <- length(y)
  if (LenX != LenY && missing(windowaction)) 
    stop("'windowaction' has been set despite equal length of 'x' and 'y'.")
  
  # PREPARE
  if (missing(windowaction))
    windowaction <- "cut"
  else
    windowaction <- match.arg(windowaction)
  
  # Classic ccf call
  if(stationary && missing(protrusionaction) && windowaction == "cut") {
    X <- ts.intersect(as.ts(x), as.ts(y))
    colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
    acf.out <- stats::acf(X, lag.max = lag.max, plot = FALSE, type = type, 
                   na.action = na.action)
    lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
    y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
    acf.out$acf <- array(y, dim = c(length(y), 1L, 1L))
    acf.out$lag <- array(lag, dim = c(length(y), 1L, 1L))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
  }
  
  
  
  # Finish up
  # acf.out <- structure(list(acf = acf, type = type, n.used = sampleT, 
  #                           lag = lag, series = series, snames = colnames(x)), class = "acf")
  if (plot) {
    plot.acf(acf.out, ...)
    invisible(acf.out)
  }
  else 
    return(acf.out)
}