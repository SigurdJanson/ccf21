# Open questions
# - how to handle 'type'?
# - how to handle 'ask'?
# - same for ci-type?
# - ask?
# - margins?
# - verbose?

#' plot.ccf
#'
#' @param x an object of class `ccf`.
#' @param ci coverage probability for confidence interval. 
#' Plotting of the confidence interval is suppressed if ci is zero or negative.
#' @param type the type of plot to be drawn, default to histogram like 
#' vertical lines. See `[plot()]` for details.
#' @param xlab the x label of the plot.
#' @param ylab the y label of the plot.
#' @param ylim numeric of length 2 giving the y limits for the plot.
#' @param main overall title for the plot.
#' @param ci.col colour to plot the confidence interval lines.
#' @param ci.type should the confidence limits assume a white noise 
#' input or for lag k an MA(k-1) input? Can be abbreviated.
#' @param max.mfrow 
#' @param ask 
#' @param mar 
#' @param oma 
#' @param mgp 
#' @param xpd 
#' @param cex.main 
#' @param verbose 
#' @param ... graphics parameters to be passed to the plotting routines.
#'
#' @author Jan Seifert
#' @source https://stackoverflow.com/questions/28857241/r-combine-plots-that-use-parmfrow-internally 
#' by Alex A. (https://stackoverflow.com/users/3005513/alex-a)
plot.ccf <- function(x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
                     ylim = NULL, main = NULL, ci.col = "blue", ci.type = c("white", "ma"), 
                     max.mfrow = 6, ask = Npgs > 1 && dev.interactive(),
                     mar = if(nser > 2) c(3,2,2,0.8) else par("mar"),
                     oma = if(nser > 2) c(1,1.2,1,1) else par("oma"),
                     mgp = if(nser > 2) c(1.5,0.6,0) else par("mgp"),
                     xpd = par("xpd"),
                     cex.main = if(nser > 2) 1 else par("cex.main"),
                     verbose = getOption("verbose"),
                     ...) {
  d.ccf <- data.frame(lag = x$lag, acf = x$acf)
  ci.line <- qnorm((1 - ci) / 2) / sqrt(x$n.used)
  
  g <- ggplot(d.acf, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    geom_segment(aes(xend = lag, yend=0)) +
    geom_hline(yintercept = +ci.line, color = ci.col, linetype = "dashed") +
    geom_hline(yintercept = -ci.line, color = ci.col, linetype = "dashed") +
    theme_bw() +
    xlab(xlab) +
    ggtitle(ifelse(is.null(main), "", main)) +
    if (is.null(ylab))
      ylab(ifelse(type=="partial", "PACF", "ACF"))
  else
    ylab(ylab)
  
  g
}