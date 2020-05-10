
max.ccf <- function(x, tol = 0) {
  # PRECONDITIONS
  ClassX <- !is.na(match(c("ccf", "acf"), class(x)))
  if( !ClassX[1] ) # not class "ccf"
    if( !ClassX[2] ) # not class "acf"
      stop("Argument 'x' is not an object of class 'acf' or 'ccf'")
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


