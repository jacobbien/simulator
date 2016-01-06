
#' Make a boxplot of a metric for each method using ggplot2
#'
#' @param evals_df output of \code{\link{as.data.frame.Evals}}
ggplot_eval <- function(evals_df, metric_name, method_name, method_label, main,
                        ylab) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("To use this function, ggplot2 must be installed.", call. = FALSE)
  }
  ggplot2::ggplot(evals_df, ggplot2::aes_string("Method", metric_name)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Method", y = ylab, title = main) +
    ggplot2::scale_x_discrete(breaks = method_name, labels = method_label)
}

#' Make a boxplot of a metric for each method
#'
#' @param evals an object of class \code{\link{Evals}}
#' @param metric_name the name of a metric to plot
#' @param use_ggplot2 whether to use \code{ggplot2} (requires installation
#'        of \code{ggplot2})
#' @param main title of plot (default is \code{model_label})
#' @param sub subtitle (only when \code{use_ggplot2 = FALSE})
#' @param ylab the y-axis label (default is \code{metric_label})
#' @param angle angle of labels (only when \code{use_ggplot2 = FALSE})
#' @param ... additional arguments to pass to \code{boxplot} (only when
#'        \code{use_ggplot2 = FALSE}).
#' @export
plot_eval <- function(evals, metric_name, use_ggplot2 = TRUE, main, sub,
                      ylab, angle = 0, ...) {
  stopifnot(metric_name %in% evals@metric_name)
  evals_df <- as.data.frame(evals)
  if (missing(main)) {
    main <- evals@model_label
    sub <- ""
  } else if (missing(sub)) sub <- evals@model_label
  if (missing(ylab))
    ylab <- evals@metric_label[evals@metric_name == metric_name]
  if (use_ggplot2) return(ggplot_eval(evals_df, metric_name, evals@method_name,
                                      evals@method_label, main = main,
                                      ylab = ylab))
  if (angle == 0) {
    boxplot(as.formula(paste0(metric_name, "~ Method")), data = evals_df,
            names = evals@method_label, ylab = ylab, xlab = "Method",
            main = main, sub = sub, ...)
  } else {
    boxplot(as.formula(paste0(metric_name, "~ Method")), data = evals_df,
            names = evals@method_label, xaxt = "n", ylab = ylab,
            xlab = "Method",
            main = main, sub = sub, ...)
    axis(1, labels=FALSE)
    text(x =  seq_along(evals@method_label),
         y = par("usr")[3] - 0.03 * diff(par("usr"))[3],
         srt = angle,
         adj = 1, labels = evals@method_label, xpd = TRUE)
  }
}

#' Plot one metric versus another for each method
#'
#' @param evals an object of class \code{\link{Evals}}
#' @param metric_name_x the name of metric to plot on x axis
#' @param metric_name_y the name of metric to plot on y axis
#' @param use_ggplot2 whether to use \code{ggplot2} (requires installation
#'        of \code{ggplot2})
#' @param main title of plot (default is \code{model_label})
#' @param sub subtitle (only when \code{use_ggplot2 = FALSE})
#' @param xlab the x-axis label (default is \code{metric_label_x})
#' @param ylab the y-axis label (default is \code{metric_label_y})
#' @param xlim the limits of the x-axis
#' @param ylim the limits of the y-axis
#' @param legend_location location of legend.  Set to NULL to remove legend.
#' @param method_col color to use for each method
#' @param method_lty line style to use for each method
#' @param method_lwd line thickness to use for each method
#' @param method_pch point style to use for each method (default is that no
#'        points, only lines are drawn)
#' @param ... additional arguments to pass to \code{boxplot} (only when
#'        \code{use_ggplot2 = FALSE}).
#' @export
plot_evals <- function(evals, metric_name_x, metric_name_y, use_ggplot2 = TRUE,
                       main, sub, xlab, ylab, xlim, ylim,
                       legend_location = "topright",
                       method_col = seq(num_methods),
                       method_lty = rep(1, num_methods),
                       method_lwd = rep(1, num_methods),
                       method_pch = rep(NA, num_methods),...) {
  stopifnot(metric_name_x %in% evals@metric_name)
  stopifnot(metric_name_y %in% evals@metric_name)
  evals_df <- as.data.frame(evals)
  num_methods <- length(evals@method_name)
  if (missing(main)) {
    main <- evals@model_label
    sub <- ""
  } else if (missing(sub)) sub <- evals@model_label
  if (missing(xlab))
    xlab <- evals@metric_label[evals@metric_name == metric_name_x]
  if (missing(ylab))
    ylab <- evals@metric_label[evals@metric_name == metric_name_y]
  if (missing(xlim)) xlim <- range(evals_df[, metric_name_x])
  if (missing(ylim)) ylim <- range(0, evals_df[, metric_name_y])
  if (use_ggplot2) return(ggplot_evals(evals_df, metric_name_x, metric_name_y,
                                       method_labels = evals@method_label,
                                       main = main, xlab = xlab, ylab = ylab,
                                       xlim = xlim, ylim = ylim))
  stopifnot(length(method_col) == num_methods)
  stopifnot(length(method_lty) %in% c(1, num_methods))
  if (length(method_lty) == 1) method_lty <- rep(method_lty, num_methods)
  stopifnot(length(method_lwd) %in% c(1, num_methods))
  if (length(method_lwd) == 1) method_lwd <- rep(method_lwd, num_methods)
  stopifnot(length(method_pch) == num_methods)
  plot(0, 0, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, main = main,
       sub = sub, type = "n", ...)
  palette(options("simulator.color_palette")[[1]])
  for (r in unique(evals_df[["Draw"]])) {
    for (m in seq_along(evals@method_name)) {
      ii <- evals_df$Method == evals@method_name[m] & evals_df$Draw == r
      points(evals_df[ii, metric_name_x], evals_df[ii, metric_name_y],
             xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, main = main,
             sub = sub, col = method_col[m], lty = method_lty[m],
             lwd = method_lwd[m], pch = method_pch[m], type = "o")
    }
  }
  if (is.character(legend_location)) {
    legend(legend_location, legend = evals@method_label, col = method_col,
           pch = method_pch, lty = method_lty, lwd = method_lwd)
  }
  palette("default")
  return()
}

ggplot_evals <- function(evals_df, metric_name_x, metric_name_y, method_labels, main, xlab,
                        ylab, xlim, ylim) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("To use this function, ggplot2 must be installed.", call. = FALSE)
  }
  ggplot2::ggplot(evals_df, ggplot2::aes_string(metric_name_x, metric_name_y)) +
    ggplot2::geom_line(ggplot2::aes(color = Method, group = Method:Draw)) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    ggplot2::scale_colour_discrete(labels = method_labels) +
    ggplot2::ylim(ylim) +
    ggplot2::xlim(xlim)
}
