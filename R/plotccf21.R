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
#' @param sub subtitle for the plot.
#' @param ci.col colour to plot the confidence interval lines.
#' @param ci.type should the confidence limits assume a white noise
#' input or for lag `k` an `MA(k-1)` input? Can be abbreviated.
#' @param mar Numerical vector of the form `c(bottom, left, top, right)`
#' which gives the number of text lines of margin.
#' @param cex.main Default size for the title will be multiplied with `cex.main`
#' to adjust the size.
#' @param max.mfrow Unused. Available only for compatibility reasons.
#' @param ask Unused. Available only for compatibility reasons.
#' @param oma Unused. Available only for compatibility reasons.
#' @param mgp Unused. Available only for compatibility reasons.
#' @param xpd Unused. Available only for compatibility reasons.
#' @param verbose Unused. Available only for compatibility reasons.
#' @param ... Unused. Available only for compatibility reasons.
#' @note The white noise option for confidence intervals is probably misleading.
#' It is based on an uncorrelated series and should be treated with appropriate caution.
#' It is only kept here for compatibility reasons.
#'
#' This implementation drops several arguments. They stay only for
#' only for compatibility reasons but will be ignored.
#' @seealso `[stats::plot.acf()]` `[ggplot2::ggplot()]`
#' `[grapics::plot()]` for arguments: `type`, `mar`.
#' @author Jan Seifert
#' @source <https://stackoverflow.com/questions/28857241/r-combine-plots-that-use-parmfrow-internally>
#' by Alex A. (<https://stackoverflow.com/users/3005513/alex-a>)
#'
plot.ccf <- function(x, ci = 0.95, type = "h",
                     xlab = "Lag", ylab = NULL, sub = NULL,
                     ylim = NULL, main = NULL,
                     ci.col = "#3366FF", ci.type = c("white", "ma"),
                     cex.main = 1, mar = NULL,
                     max.mfrow = NULL, ask = NULL, # obsolete args
                     oma = NULL, mgp = NULL, xpd = NULL, # obsolete args
                     verbose = NULL, ...) { # obsolete args
  # PRECONDITIONS
  ci.type <- match.arg(ci.type)
  if (ci > 0 && x$type != "covariance") {
    if(ci.type == "white") {
      ci.line <- stats::qnorm((1+ci)/2) / sqrt(x$n.used)
      ci.line <- cbind(lower = drop(x$acf) - ci.line,
                       upper = drop(x$acf) + ci.line)
      #TODO: create around r
    } else { #ci.type = ma
      if(is.null(x$acf.ci) || is.na(x$acf.ci)) {
        ci.line <- .CorConf_Fisher( drop(x$acf), x$acf.n, ci )
      } else if(x$ci.level != ci) {
        ci.line <- .CorConf_Fisher( drop(x$acf), x$acf.n, ci )
      } else {
        ci.line <- x$acf.ci
      }
    }
  }
  else
  {
    ci.line <- NULL
  }

  if(!is.numeric(cex.main)) cex.main <- 1

  # RUN
  if (is.null(ylim))
    ylim <- c( -max(abs(range(x$acf))), max(abs(range(x$acf))) )
    if(!is.null(ci.line)) ylim <- ylim * 1.05
  else {
    # Range has two values. Make sure first one is negative.
    if (length(ylim) == 1) ylim <- c(-ylim, ylim) * sign(ylim)
  }
  #
  if (is.null(ylab)) ylab <- "CCF"


  d.ccf <- data.frame(lagval = x$lag, acfval = x$acf)
  g <- ggplot2::ggplot(d.ccf, ggplot2::aes(x = lagval, y = acfval)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::ylim(low = ylim[1], high = ylim[2]) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = (11*1.2)*cex.main)
    ) +
    ggplot2::labs(x = xlab, y = ylab, subtitle = sub,
         caption = paste("Shifting action =", x$shiftaction)) +
    ggplot2::ggtitle(ifelse(is.null(main), x$snames, main))

  # Add values
  if (type == "h")
    g <- g + ggplot2::geom_segment(ggplot2::aes(xend = lagval, yend = 0))
  else if (type == "l")
    g <- g + ggplot2::geom_line()
  else if (type == "b")
    g <- g + ggplot2::geom_line() + ggplot2::geom_point()
  else if (type == "s")
    g <- g + ggplot2::geom_step()
  else if (type == "S")
    g <- g + ggplot2::geom_step(direction = "vh")

  # Add confidence intervals
  if(!is.null(ci.line)) {
    ci.line <- data.frame(lagval = x$lag, ci.line)
    g <- g + ggplot2::geom_line(data = ci.line, mapping = ggplot2::aes(x = lagval, y = .data$upper),
                       inherit.aes = FALSE,
                       color = ci.col, linetype = "dashed")
    g <- g + ggplot2::geom_line(data = ci.line, mapping = ggplot2::aes(x = lagval, y = .data$lower),
                       inherit.aes = FALSE,
                       color = ci.col, linetype = "dashed")
  }

  # Add margins
  if (!is.null(mar)) {
    g <- g  + ggplot2::theme(plot.margin = ggplot2::unit(mar, "lines"))
  }
  if (!is.null(oma)) {
    # not implemented, yet
  }
  #

  invisible(g)
}
