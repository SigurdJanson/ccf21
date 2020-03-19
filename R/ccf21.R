

#' .CorConf_Fisher
#' Computes confidence intervals for correlation coefficients.
#' @param r Correlation coefficient (pearson)
#' @param n Sample size of the data underlying r
#' @param ci Confidence limit. Default is 0.95.
#' @details The function uses the Fisher z transform to get approximately normal
#' distributed correlations, computes the confidence range and transforms the
#' values back to correlations.
#' @return A two-comlumn matrix with each row holding the range of confidence.
#' @author Jan Seifert
#' @references Bonett, D. G. & Wright, T. A. (2000). Sample Size Requirements 
#' for Estimating Pearson, Kendall and Spearman Correlations.” Psychometrika, 
#' 65 (1), p. 23-28.
#' @examples
.CorConf_Fisher <- function( r, n, ci = 0.95 ) {
  if(missing(r)) return(NA)
  if(any(r > 1) || any(r < -1)) 
    stop("Fisher Z transformation is only defined for correlations (i.e. -1 <= r <= 1)")
  
  # Assume two-sided limits, i.e. use 0.975 instead 0.95
  cutoff <- qnorm( (ci+1)*0.5 )
  
  # Identify +/-1 because transformation cannot handle that
  rp1 <- which(r == 1)
  rm1 <- which(r == -1)
  # Fisher z transform
  z <- log( (1+r) / (1-r) ) / 2
  z[rp1] <- Inf  # Set r= 1/-1 to Inf/-Inf
  z[rm1] <- -Inf
  #
  zci <- cbind( (z - cutoff * sqrt(1/(n-3))),
                (z + cutoff * sqrt(1/(n-3))) )
  # inverse fisher transform to get r back
  rci <- (exp(2*zci)-1) / (exp(2*zci)+1)
  rci[rp1] <- c(1, 1)
  rci[rm1] <- c(-1, -1)
  
  return(rci)
}



#' .Cor_ccf
#' Correlation two vectors x and y. Mean, standard deviation and sample size can 
#' deviate from sample estimates.
#' @param x,y Numeric vectors
#' @param type Character string giving the type of acf to be computed. 
#' Allowed values are "correlation" (the default), "covariance". Can be abbreviated.
#' @param n Sample size of x and y.
#' @param mean Means of x and y. Either a single value for both or one mean each.
#' @param sd Standard deviations of x and y. Either a single value for 
#' both or one mean each.
#'
#' @details If mean, sd, or n are given, treat x and y as stationary time 
#' series. To be treated as non-stationary, n, mean and sd must ALL be NA.
#' In this case, the functions determines mean, sd, and n based on x and y.
#' @return The correlation between x and y
.Cor_ccf <- function(x, y, type = c("correlation", "covariance"),
                     n = NA, mean = c(NA, NA), sd = c(NA, NA)) {
  # PRECONDITIONS
  if (length(x) != length(y)) stop("Correlations assume two vectors of equal length.")
  type <- match.arg(type)
  
  # CODE
  if (anyNA(mean) && anyNA(stddev) && anyNA(n)) {
    if (type == "correlation")
      test <- cor(x, y) 
    else
      test <- cov(x, y)
  } else {
    # make sure mean & sd have the format 'c(x, y)' without NAs
    if (length(mean) == 1) mean <- c(mean, mean)
    if (length(sd)   == 1) sd   <- c(sd, sd)
    mean[which(is.na(mean))] <- c(mean(x), mean(y))[which(is.na(mean))]
    sd  [which(is.na(sd))]   <- c(sd(x), sd(y))[which(is.na(sd))]
    if (is.na(n))  n <- length(x)
    #
    cov <- sum( (x - mean[1]) * (y - mean[2]) ) / (n-1)
    if (type == "correlation")
      test <- cov / sd[1] / sd[2]
    else
      test <- cov
  }
  # Finish
  return(test)
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