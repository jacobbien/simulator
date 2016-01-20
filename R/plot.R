
#' Make a boxplot of a metric for each method using ggplot2
#'
ggplot_eval <- function(evals_df, metric_name, method_name, method_label,
                        main, facet_mains, ylab, ylim, nrow, ncol) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("To use this function, ggplot2 must be installed.", call. = FALSE)
  }
  if (missing(main)) main <- NULL
  if (length(unique(evals_df$Model)) == 1) {
    return(ggplot2::ggplot(evals_df, ggplot2::aes_string("Method", metric_name)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(x = "Method", y = ylab, title = main) +
      ggplot2::scale_x_discrete(breaks = method_name, labels = method_label) +
      ggplot2::scale_y_continuous(limits = ylim))
  }
  # display multiple facets...
  levels(evals_df$Model) <- facet_mains
  ggplot2::ggplot(evals_df, ggplot2::aes_string("Method", metric_name)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Method", y = ylab, title = main) +
    ggplot2::scale_x_discrete(breaks = method_name, labels = method_label) +
    ggplot2::facet_wrap("Model", nrow = nrow, ncol = ncol) +
    ggplot2::scale_y_continuous(limits = ylim)
}

#' Make a boxplot of a metric for each method
#'
#' If evals is a \code{listofEvals}, then each model will be its own plot.
#'
#' @param evals an object of class \code{\link{Evals}} or of class
#'        \code{listofEvals}
#' @param metric_name the name of a metric to plot
#' @param use_ggplot2 whether to use \code{ggplot2} (requires installation
#'        of \code{ggplot2})
#' @param main title of plot. Default is \code{model_label} when evals is
#'        a single Evals.
#' @param facet_mains only to be used when evals is a \code{listofEvals}
#'        and should be of the same length. Default will be the model_label
#'        for each model.
#' @param ylab the y-axis label (default is \code{metric_label})
#' @param ylim the y-axis limits to use (across all plots)
#' @param include_zero whether ylim should include 0.  Ignored if ylim
#'        is passed explicitly
#' @param angle angle of labels (only when \code{use_ggplot2 = FALSE})
#' @param ... additional arguments to pass to \code{boxplot} (only when
#'        \code{use_ggplot2 = FALSE}).
#' @export
plot_eval <- function(evals, metric_name, use_ggplot2 = TRUE, main,
                      facet_mains, ylab, include_zero = FALSE, ylim, angle = 0,
                      ...) {
  if ("Evals" %in% class(evals)) {
    evals <- list(evals)
    class(evals) <- c("listofEvals", "list")
  }
  else if (!("listofEvals" %in% class(evals)))
    stop("Invalid class for evals object.")
  stopifnot(unlist(lapply(evals, function(e) metric_name %in% e@metric_name)))
  if (length(evals) == 1) {
    if (missing(main))
      main <- evals[[1]]@model_label
    facet_mains <- main
  } else {
    # we have multiple facets
    if (missing(facet_mains))
      facet_mains <- unlist(lapply(evals, function(e) e@model_label))
  }
  if (missing(ylab))
    ylab <- evals[[1]]@metric_label[evals[[1]]@metric_name == metric_name]
  evals_df <- as.data.frame(evals)
  if (missing(ylim)) {
    ylim <- range(evals_df[[metric_name]])
    if (include_zero) ylim <- range(ylim, 0)
  }
  method_names <- unique(unlist(lapply(evals, function(e) e@method_name)))
  method_labels <- unique(unlist(lapply(evals, function(e) e@method_label)))
  nrow <- floor(sqrt(length(evals)))
  ncol <- ceiling(length(evals) / nrow)
  if (use_ggplot2) return(ggplot_eval(evals_df, metric_name, method_names,
                                      method_labels, main, facet_mains, ylab,
                                      ylim, nrow, ncol))
  par(mfrow = c(nrow, ncol))
  for (i in seq_along(evals)) {
    evals_df <- as.data.frame(evals[[i]])
    if (angle == 0) {
      boxplot(as.formula(paste0(metric_name, "~ Method")), data = evals_df,
              names = evals[[i]]@method_label, ylab = ylab, xlab = "Method",
              main = facet_mains[i], ylim = ylim, ...)
    } else {
      boxplot(as.formula(paste0(metric_name, "~ Method")), data = evals_df,
              names = evals[[i]]@method_label, xaxt = "n", ylab = ylab,
              xlab = "Method",
              main = facet_mains[i], ylim = ylim, ...)
      axis(1, labels=FALSE)
      text(x =  seq_along(evals[[i]]@method_label),
           y = par("usr")[3] - 0.03 * diff(par("usr"))[3],
           srt = angle,
           adj = 1, labels = evals[[i]]@method_label, xpd = TRUE)
    }
  }
  if (!missing(main) & length(evals) > 1)
    title(main, outer = TRUE, line = -1)
}

#' Plot one metric versus another for each method
#'
#' If evals is a \code{listofEvals}, then each model will be its own plot.
#'
#' @param evals an object of class \code{\link{Evals}} or of class
#'        \code{listofEvals}
#' @param metric_name_x the name of metric to plot on x axis
#' @param metric_name_y the name of metric to plot on y axis
#' @param use_ggplot2 whether to use \code{ggplot2} (requires installation
#'        of \code{ggplot2})
#' @param main title of plot. Default is \code{model_label} when evals is
#'        a single Evals.
#' @param facet_mains only to be used when evals is a \code{listofEvals}
#'        and should be of the same length. Default will be the model_label
#'        for each model.
#' @param xlab the x-axis label (default is \code{metric_label_x})
#' @param ylab the y-axis label (default is \code{metric_label_y})
#' @param xlim the limits of the x-axis
#' @param ylim the limits of the y-axis
#' @param include_zero whether ylim should include 0.  Ignored if ylim
#'        is passed explicitly
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
                       main, facet_mains, xlab, ylab, xlim, ylim,
                       include_zero = FALSE,
                       legend_location = "topright",
                       method_col = seq(num_methods),
                       method_lty = rep(1, num_methods),
                       method_lwd = rep(1, num_methods),
                       method_pch = rep(NA, num_methods),...) {
  if ("Evals" %in% class(evals)) {
    evals <- list(evals)
    class(evals) <- c("listofEvals", "list")
  }
  else if (!("listofEvals" %in% class(evals)))
    stop("Invalid class for evals object.")
  stopifnot(unlist(lapply(evals,
                          function(e) metric_name_x %in% e@metric_name)))
  stopifnot(unlist(lapply(evals,
                          function(e) metric_name_y %in% e@metric_name)))
  evals_df <- as.data.frame(evals)
  method_names <- lapply(evals, function(e) e@method_name)
  if (length(unique(method_names)) != 1)
    stop("All models must have same methods.")
  num_methods <- length(method_names[[1]])
  if (length(evals) == 1) {
    if (missing(main))
      main <- evals[[1]]@model_label
    facet_mains <- main
  } else {
    # we have multiple facets
    if (missing(facet_mains))
      facet_mains <- unlist(lapply(evals, function(e) e@model_label))
  }
  if (missing(xlab))
    xlab <- evals[[1]]@metric_label[evals[[1]]@metric_name == metric_name_x]
  if (missing(ylab))
    ylab <- evals[[1]]@metric_label[evals[[1]]@metric_name == metric_name_y]
  evals_df <- as.data.frame(evals)
  if (missing(xlim)) xlim <- range(evals_df[[metric_name_x]])
  if (missing(ylim)) {
    ylim <- range(evals_df[[metric_name_y]])
    if (include_zero) ylim <- range(0, ylim)
  }
  nrow <- floor(sqrt(length(evals)))
  ncol <- ceiling(length(evals) / nrow)
  if (use_ggplot2) return(ggplot_evals(evals_df, metric_name_x, metric_name_y,
                                       method_labels = evals[[1]]@method_label,
                                       main = main, facet_mains = facet_mains,
                                       xlab = xlab, ylab = ylab,
                                       xlim = xlim, ylim = ylim,
                                       nrow = nrow, ncol = ncol))
  stopifnot(length(method_col) == num_methods)
  stopifnot(length(method_lty) %in% c(1, num_methods))
  if (length(method_lty) == 1) method_lty <- rep(method_lty, num_methods)
  stopifnot(length(method_lwd) %in% c(1, num_methods))
  if (length(method_lwd) == 1) method_lwd <- rep(method_lwd, num_methods)
  stopifnot(length(method_pch) == num_methods)

  par(mfrow = c(nrow, ncol))
  palette(options("simulator.color_palette")[[1]])
  for (i in seq_along(evals)) {
    evals_df <- as.data.frame(evals[[i]])
    plot(0, 0, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
         main = facet_mains[i], type = "n", ...)
    for (r in unique(evals_df[["Draw"]])) {
      for (m in seq_along(method_names[[1]])) {
        ii <- evals_df$Method == method_names[[1]][m] & evals_df$Draw == r
        points(evals_df[ii, metric_name_x], evals_df[ii, metric_name_y],
               col = method_col[m], lty = method_lty[m],
               lwd = method_lwd[m], pch = method_pch[m], type = "o")
      }
    }
    if (i == 1 & is.character(legend_location)) {
      legend(legend_location, legend = evals[[1]]@method_label, col = method_col,
             pch = method_pch, lty = method_lty, lwd = method_lwd)
    }
  }
  if (!missing(main) & length(evals) > 1)
    title(main, outer = TRUE, line = -1)
  palette("default")
  return()
}

ggplot_evals <- function(evals_df, metric_name_x, metric_name_y, method_labels,
                         main, facet_mains, xlab, ylab, xlim, ylim,
                         nrow, ncol) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("To use this function, ggplot2 must be installed.", call. = FALSE)
  }
  if (missing(main)) main <- NULL
  if (length(unique(evals_df$Model)) == 1) {
    return(ggplot2::ggplot(evals_df, ggplot2::aes_string(metric_name_x, metric_name_y)) +
             ggplot2::geom_line(ggplot2::aes(color = Method, group = Method:Draw)) +
             ggplot2::labs(x = xlab, y = ylab, title = main) +
             ggplot2::scale_colour_discrete(labels = method_labels) +
             ggplot2::ylim(ylim) +
             ggplot2::xlim(xlim))
  }
  # display multiple facets...
  levels(evals_df$Model) <- facet_mains
  ggplot2::ggplot(evals_df, ggplot2::aes_string(metric_name_x, metric_name_y)) +
    ggplot2::geom_line(ggplot2::aes(color = Method, group = Method:Draw)) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    ggplot2::scale_colour_discrete(labels = method_labels) +
    ggplot2::ylim(ylim) +
    ggplot2::xlim(xlim) +
    ggplot2::facet_wrap("Model", nrow = nrow, ncol = ncol)
}
