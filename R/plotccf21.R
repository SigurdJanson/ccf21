library(ggplot2)

# Open questions
# - same for ci-type?
# - ...?
# - sub

#' plot.ccf
#' Plot method for objects of class "`ccf`".
#' @param x an object of class `ccf`.
#' @param ci coverage probability for confidence interval. 
#' Plotting of the confidence interval is suppressed if `ci` is zero or negative.
#' @param type the type of plot to be drawn, default to histogram like 
#' vertical lines. See `[plot()]` for details.
#' @param xlab the x label of the plot.
#' @param ylab the y label of the plot.
#' @param ylim numeric of length 2 giving the y limits for the plot.
#' @param main overall title for the plot.
#' @param ci.col colour to plot the confidence interval lines.
#' @param ci.type should the confidence limits assume a white noise 
#' input or for lag `k` an `MA(k-1)` input? Can be abbreviated.
#' @param max.mfrow Obsolete.
#' @param ask Obsolete.
#' @param mar Numerical vector of the form `c(bottom, left, top, right)` 
#' which gives the number of text lines of margin.
#' @param oma Obsolete.
#' @param mgp Obsolete.
#' @param xpd Obsolete.
#' @param cex.main Default size for the title will be multiplied with `cex.main`
#' to adjust the size.
#' @param verbose Obsolete.
#' @param ... graphics parameters to be passed to the plotting routines.
#' @note The white noise option for confidence intervals is probably misleading.
#' It is based on an uncorrelated series and should be treated with appropriate caution. 
#' It is only kept here for compatibility reasons. 
#' 
#' This implementation dropped the `verbose` option. It is in the list 
#' of arguments only for compatibility reasons.
#' @seealso `[stats::plot.acf()]` `[ggplot2::ggplot()]` 
#' `[grapics::plot()]` for type argument.
#' @author Jan Seifert
#' @source <https://stackoverflow.com/questions/28857241/r-combine-plots-that-use-parmfrow-internally> 
#' by Alex A. (<https://stackoverflow.com/users/3005513/alex-a>)
plot.ccf <- function(x, ci = 0.95, type = "h", 
                     xlab = "Lag", ylab = NULL, sub = NULL,
                     ylim = NULL, main = NULL, 
                     ci.col = "#3366FF", ci.type = c("white", "ma"), 
                     cex.main = 1, mar = NULL, 
                     max.mfrow = NULL, ask = NULL, # obsolete args
                     oma = NULL, mgp = NULL, xpd = NULL, # obsolete args
                     verbose = NULL, # obsolete args
                     ...) {
  # PRECONDITIONS
  ci.type <- match.arg(ci.type)
  if (ci > 0 && x$type != "covariance") {
    if(ci.type == "white") {
      ci.line <- qnorm((1+ci)/2) / sqrt(x$n.used)
    } else {
      if(is.null(x$ci)) {
        stop("not implemented, yet") #TODO: compute ci
      }
      stop("not implemented, yet") #TODO: create ci-line from data
    }
  }

  # RUN
  if (is.null(ylim)) 
    ylim <- c( -max(abs(range(x$acf))), max(abs(range(x$acf))) )
  else {
    # Range has two values. Make sure first one is negative.
    if (length(ylim) == 1) ylim <- c(-ylim, ylim) * sign(ylim) 
  }
  # 
  if (is.null(ylab)) ylab <- "CCF"
    
  
  d.ccf <- data.frame(lag = x$lag, acf = x$acf)
  g <- ggplot(d.ccf, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    ylim(low = ylim[1], high = ylim[2]) +
    #theme_bw() +
    theme(
      plot.title = element_text(size = (11*1.2)*cex.main)
    ) +
    labs(x = xlab, y = ylab, subtitle = sub,
         caption = paste("Shifting action =", x$shiftaction)) +
    ggtitle(ifelse(is.null(main), x$snames, main))
  
  # Add values
  if (type == "h")
    g <- g + geom_segment(aes(xend = lag, yend = 0))
  else if (type == "l")
    g <- g + geom_line()
  else if (type == "b")
    g <- g + geom_line() + geom_point()
  else if (type == "s")
    g <- g + geom_step()
  else if (type == "S")
    g <- g + geom_step(direction = "vh")

  # Add confidence intervals
  if(!is.null(ci.line)) {
    if (length(ci.line) == 1) {
      g <- g  + 
        geom_hline(yintercept = +ci.line, 
                   color = ci.col, linetype = "dashed") +
        geom_hline(yintercept = -ci.line, 
                   color = ci.col, linetype = "dashed")
    } else {
      #TODO: Put correct confidence right here
    }
  }
  
  # Add margins
  if (!is.null(mar)) {
    g <- g  + theme(plot.margin = unit(mar, "lines"))
  }
  if (!is.null(oma)) {
    # not implemented, yet
  }
  #
  
  
  
  invisible(g)
}

p <- plot(o, ci.type = "white", type = "h", sub = "YEAH!")
p
