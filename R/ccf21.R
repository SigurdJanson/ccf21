

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
  if(missing(r)) stop("Pearson correlation 'r' required")
  if(missing(n)) stop("Sample size 'n' required")
  if(any(r > 1) || any(r < -1)) 
    stop("Correlations are only defined between -1 and 1")
  if(any(ci > 1) || any(ci < 0)) 
    stop("Confidence is only defined between 0 and 1")
  
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
  rci[rp1,] <- c(1, 1)
  rci[rm1,] <- c(-1, -1)
  
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
  if (anyNA(mean) && anyNA(sd) && anyNA(n)) {
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



#' ccf
#' ## Open topics ###
#' Confidence
#' "not all series have the same frequency"
#' 
#' @param x,y a univariate numeric vector or time series object.
#' @param lag.max maximum lag at which to calculate the acf. 
#' Default is 10*log10(N/m) where N is the number of observations 
#' and m the number of series. Will be automatically limited 
#' to one less than the number of observations in the series.
#' @param type character string giving the type of ccf to be 
#' computed. Allowed values are "correlation" (the default) or "covariance". 
#' Will be partially matched.
#' @param stationary Shall vectors be treated as if they were a stationary time
#' series. If \code{FALSE} plain correlations will be used.
#' @param shiftaction How to handle values that are shifted out of range. 
#' See details.
#' @param plot logical. If TRUE (the default) the ccf is plotted.
#' @param na.action function to be called to handle missing values.
#' na.pass can be used.
#' @param ... further arguments to be passed to 'plot'
ccf <- function (x, y, lag.max = NULL, type = c("correlation", "covariance"), 
                 stationary = NULL, 
                 shiftaction = c("cut", "wrap", "replace", "imprison"),
                 replaceby = NULL,
                 plot = TRUE, na.action = na.fail, ...)  {
  
  # PRECONDITIONS & PREPARATIONS
  if (is.matrix(x) || is.matrix(y)) 
    stop("Only univariate data is allowed.")
  if (is.ts(x) && is.ts(y)) 
    if(abs(frequency(x) - frequency(y)) > getOption("ts.eps")*0.5)
      stop("The time series have different frequencies")
  x <- na.action(x)
  y <- na.action(y)
  if (identical(na.action, na.fail) && identical(replaceby, NA)) 
    stop("Cannot use 'replacement' NA when 'na.action' is fail.")
  if (missing(stationary))
    stationary <- (is.ts(x) || is.ts(y))
  
  type <- match.arg(type)
  
  LenX <- length(x)
  LenY <- length(y)
  if(missing(shiftaction)) { # set defaults
    if (LenX != LenY)
      shiftaction <- "imprison"
    else
      shiftaction <- "cut"
  } else {
    shiftaction <- match.arg(shiftaction)
    if(any(c("wrap", "replace") == shiftaction) && LenX != LenY)
      stop("Unequal length of 'x' and 'y' works only with 'cut' or 'imprison'.")
  }
  if (shiftaction == "replace" && (missing(replaceby) || is.null(replaceby)))
    stop("If empty positions shall be 'replace'd by specified value, specify the value")
  
  if (is.null(lag.max) || is.na(lag.max)) {
    lag.max <- floor(10 * (log10(min(LenX, LenY)) - log10(2)))
  } else {
    if (lag.max < 0)
      stop("'lag.max' must be at least 0.")
  }
  lag.max <- as.integer(min(lag.max, min(LenX, LenY) - 1L))

  
  # RUN: Cross-Correlate
  x.freq <- frequency(x)

  
  if(shiftaction == "cut") {
    if(stationary) {
      st_n    <- length(x)
      st_mean <- c(mean(x), mean(y))
      st_sd   <- c(sd(x),   sd(y))
    } else {
      st_n <- NA
      st_mean <- st_sd <- c(NA, NA)
    }
    lags <- (-lag.max):(+lag.max)
    r <- numeric(length(lags))
    rindex <- 1
    x0 <- x[ seq(1, min(length(x), length(y))) ]
    y0 <- y[ seq(1, min(length(x), length(y))) ]
    for(l in lags) {
      ys <- .Shift_ccf(y0, l, replace = FALSE)
      minx <- 1          + ifelse(l <= 0, 0L, l)
      maxx <- length(x0) + ifelse(l <= 0, l, 0L)
      xs <- x0[minx:maxx]
      r[rindex] <- .Cor_ccf(xs, ys, type, n = st_n, mean = st_mean, sd = st_sd)
      rindex <- rindex +1
    }
    .lag   <- array(lags, dim = c(length(lags), 1L, 1L)) / x.freq
    n.used <- length(x0)
  }
  
  
  if(any(c("wrap", "replace") == shiftaction)) {
    if(stationary) {
      st_n    <- length(x)
      st_mean <- c(mean(x), mean(y))
      st_sd   <- c(sd(x),   sd(y))
    } else {
      st_n <- NA
      st_mean <- st_sd <- c(NA, NA)
    }
    lags <- (-lag.max):(+lag.max)
    r <- numeric(length(lags))
    rindex <- 1
    replace <- ifelse(shiftaction == "wrap", TRUE, replaceby)
    for(l in lags) {
      ys <- .Shift_ccf(y, l, replace = replace)
      r[rindex] <- .Cor_ccf(x, ys, type, n = st_n, mean = st_mean, sd = st_sd)
      rindex <- rindex +1
    }
    .lag <- array(lags, dim = c(length(lags), 1L, 1L)) / x.freq
    n.used  <- length(x)
  }

  
  
  if("imprison" == shiftaction) {
    if(stationary) {
      st_n    <- length(x)
      st_mean <- c(mean(x), mean(y))
      st_sd   <- c(sd(x),   sd(y))
    } else {
      st_n <- NA
      st_mean <- st_sd <- c(NA, NA)
    }
    # x is assumed to be the longer vectors. Switch if necessary
    if(length(x) < length(y)) {
      xo <- x
      x  <- y
      y  <- xo
    }
    lags <- seq(1, length(x) - length(y), by = 1)
    r <- numeric(length(lags))
    rindex <- 1
    for(l in lags) {
      #ys <- y#[seq(l, l+length(ys), by = 1)]
      xs <- x[seq(l, l+length(y)-1, by = 1)]
      r[rindex] <- .Cor_ccf(xs, y, type, n = st_n, mean = st_mean, sd = st_sd)
      rindex <- rindex +1
    }
    .lag    <- array(lags-1, dim = c(length(lags), 1L, 1L)) / x.freq # correct lags by 1
    n.used  <- length(x)
  }
  
  
  # FINISH
  acf.out <- structure( list(acf = array(r, dim = c(length(r), 1L, 1L)), 
                             type = type, 
                             n.used = n.used,
                             lag = .lag, 
                             series = "X", 
                             snames = paste(c(deparse(substitute(x))[1L], 
                                              deparse(substitute(y))[1L]), collapse = " & "),
                             shiftaction = shiftaction, 
                             stationary = stationary,
                             replacedby = replaceby),
                        class = c("ccf", "acf"))
  
  if (plot) {
    plot(acf.out, ...) 
    invisible(acf.out)
  }
  else 
    return(acf.out)
}

# x <- ccf( rep(1:2, 10), rep(c(1, 1, 2, 2), 5), shiftaction = "replace", 
#           lag.max = 8, replaceby = NA, na.action = na.pass )
#plot.acf(x)