as.matrix.ccf <- function(x, rownames.force = TRUE) {
  # PRECONDITIONS - none that need to be checked extra
  # RESULTS
  result <- drop(x$lag)
  result <- rbind(result, drop(x$acf))
  rows   <- c("lag", "ccf")

  # Add confidence, if available
  if(!any(is.na(x$acf.ci))) {
    result <- rbind(result, x$acf.ci[,1])
    result <- rbind(result, x$acf.ci[,2])
    cilevel <- format(x$ci.level, digits = 2)
    rows <- c(rows, paste(c("ci.up", "ci.low"), cilevel, sep = "_"))
  }
  
  result <- rbind(result, x$acf.n)
  rows <- c(rows, "n")
  
  result <- as.matrix(result)
  colnames(result) <- drop(x$lag)
  if(isTRUE(rownames.force))
    rownames(result) <- rows
  else
    rownames(result) <- NULL
  
  return(result)
}


# result <- ccf( 1:10, 1:10, shiftaction = "cut", lag.max = 5, plot = FALSE,
#                ci = 0.95)
# x <- as.matrix(result)