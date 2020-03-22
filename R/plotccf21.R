#' ggacf
#' @author Alex A. (https://stackoverflow.com/users/3005513/alex-a)
#' @source https://stackoverflow.com/questions/28857241/r-combine-plots-that-use-parmfrow-internally
ccf_gg_plot <- function(x, ci=0.95, type="correlation", xlab="Lag", ylab=NULL,
                  ylim=NULL, main=NULL, ci.col="blue", lag.max=NULL) {
  
  x <- as.data.frame(x)
  
  x.acf <- acf(x, plot=F, lag.max=lag.max, type=type)
  
  ci.line <- qnorm((1 - ci) / 2) / sqrt(x.acf$n.used)
  
  d.acf <- data.frame(lag=x.acf$lag, acf=x.acf$acf)
  
  g <- ggplot(d.acf, aes(x=lag, y=acf)) +
    geom_hline(yintercept=0) +
    geom_segment(aes(xend=lag, yend=0)) +
    geom_hline(yintercept=ci.line, color=ci.col, linetype="dashed") +
    geom_hline(yintercept=-ci.line, color=ci.col, linetype="dashed") +
    theme_bw() +
    xlab("Lag") +
    ggtitle(ifelse(is.null(main), "", main)) +
    if (is.null(ylab))
      ylab(ifelse(type=="partial", "PACF", "ACF"))
  else
    ylab(ylab)
  
  g
}