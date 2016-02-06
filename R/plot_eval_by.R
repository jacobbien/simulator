
#' Plot a metric across multiple values of a model parameter
#'
#' This function is to be used on simulations in which
#' \code{\link{generate_model}} was called using the \code{vary_along}
#' parameter.  When this is a single (scalar) numeric parameter, a single plot
#' is created in which the x-axis is this parameter.  Eventually, this function
#' should handle one or two categorical variables (in which facets are used)
#' and one categorical combined with one continuous variable.
#'
#' The arguments errbars, method_col, method_lty, method_lwd, method_pch only
#' apply when use_ggplot2 is FALSE.
#'
#' @param sim an object of class \code{\link{simulation}}
#' @param metric_name the name of a metric to plot
#' @param varying character vector giving the name of a parameter that is
#'        varied across the models in evals. For now, this parameter must be
#'        numeric and there cannot be multiple models having the same value
#'        of this parameter.
#' @param use_ggplot2 whether to use \code{ggplot2} (requires installation
#'        of \code{ggplot2})
#' @param main title of plot.
#' @param xlab the x-axis label (default is \code{varying})
#' @param ylab the y-axis label (default is \code{metric_label})
#' @param xlim the x-axis limits to use
#' @param ylim the y-axis limits to use
#' @param include_zero whether ylim should include 0.  Ignored if ylim
#'        is passed explicitly
#' @param legend_location location of legend.  Set to NULL to remove legend.
#' @param errbars whether to include error bars
#' @param method_col color to use for each method
#' @param method_lty line style to use for each method
#' @param method_lwd line thickness to use for each method
#' @param method_pch point style to use for each method (default is that no
#'        points, only lines are drawn)
#' @param ... additional arguments to pass to \code{plot} (only when
#'        \code{use_ggplot2 = FALSE}).
#' @export
plot_eval_by <- function(sim, metric_name, varying, use_ggplot2 = TRUE, main,
                      xlab, ylab, xlim, ylim, include_zero = FALSE,
                      legend_location = "topright", errbars = TRUE,
                      method_col = seq(num_methods),
                      method_lty = rep(1, num_methods),
                      method_lwd = rep(1, num_methods),
                      method_pch = rep(NA, num_methods), ...) {
  if (use_ggplot2 & !requireNamespace("ggplot2", quietly = TRUE))
    stop("To use this function, ggplot2 must be installed.", call. = FALSE)
  # load models and get the values of varying
  m <- model(sim)
  vals <- lapply(m, function(mm) mm@params[[varying]])
  if (any(lapply(vals, is.null) == TRUE))
    stop("One or more models does not have ", varying,
         " as a parameter.")
  if (!all(lapply(vals, class) %in% c("numeric", "integer")))
    stop("For now, 'varying' must be a numeric/integer parameter.")
  if (any(lapply(vals, length) > 1)) stop("'varying' must be a scalar.")
  if (length(unique(vals)) != length(vals))
    stop("For now, cannot have multiple models having same value of 'varying'")
  model_names <- lapply(sim@model_refs, function(m) m@name)
  # load evals
  e <- subset_evals(evals(sim), metric_names = metric_name)
  stopifnot(unlist(lapply(e, function(ee) metric_name %in% ee@metric_name)))
  method_names <- lapply(e, function(ee) ee@method_name)
  if (length(unique(method_names)) != 1)
    stop("All models must have same methods.")
  method_names <- method_names[[1]]
  num_methods <- length(method_names)
  method_labels <- e[[1]]@method_label
  metric_label <- e[[1]]@metric_label
  # prepare data.frame that has varying, metric_name, and "Method" as columns
  df <- as.data.frame(e)
  ii <- match(df$Model, model_names)
  df[[varying]] <- unlist(vals)[ii] # add column 'varying' to data.frame

  # plotting parameters:
  if (missing(main)) main <- sprintf("Varying %s", varying)
  if (missing(xlab)) xlab <- varying
  if (missing(ylab)) ylab <- metric_label
  if (missing(xlim)) xlim <- range(df[[varying]])
  if (missing(ylim)) {
    ylim <- range(df[[metric_name]])
    if (include_zero) ylim <- range(0, ylim)
  }
  method_col <- recycle(method_col, num_methods)
  method_lwd <- recycle(method_lwd, num_methods)
  method_lty <- recycle(method_lty, num_methods)
  method_pch <- recycle(method_pch, num_methods)
  if (use_ggplot2) {
    return(ggplot2::ggplot(df, ggplot2::aes_string(varying, metric_name)) +
             ggplot2::geom_point(ggplot2::aes(color = Method, group = Method)) +
             ggplot2::stat_smooth(ggplot2::aes(color = Method, group = Method)) +
             ggplot2::labs(x = xlab, y = ylab, title = main) +
             ggplot2::scale_colour_discrete(labels = method_labels) +
             ggplot2::ylim(ylim) +
             ggplot2::xlim(xlim))
  } else {
    palette(options("simulator.color_palette")[[1]])
    plot(0, 0, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
         main = main, type = "n", ...)
    for (m in seq_along(method_names)) {
      df2 <- df[df$Method == method_names[m], ] # subset by method
      points(df2[[varying]], df2[[metric_name]],
             col = method_col[m], pch = method_pch[m])
      se <- function(a) sd(a) / sqrt(length(a))
      df_mean <- aggregate(df2[[metric_name]], by = list(df2[[varying]]), mean)
      df_se <- aggregate(df2[[metric_name]], by = list(df2[[varying]]), se)
      points(df_mean, col = method_col[m], lty = method_lty[m],
             lwd = method_lwd[m], pch = 20, type = "o")
      if (errbars) {
        segments(x0 = df_mean[, 1], y0 = df_mean[, 2] - df_se[, 2],
                 y1 = df_mean[, 2] + df_se[, 2], col = method_col[m])
      }
    }
    if (is.character(legend_location)) {
      legend(legend_location, legend = method_labels, col = method_col,
             pch = method_pch, lty = method_lty, lwd = method_lwd)
    }
  }
}

#' Recycles elements to create vector of desired length
#'
#' @param x vector to be expanded to proper length
#' @param length desired length
recycle <- function(x, length) {
  len_x <- length(x)
  if (len_x == length) return(x)
  if (len_x > length) {
    warning("Too many elements provided. Ignoring some.", call. = FALSE)
    return(x[1:length])
  }
  if (len_x < length) {
    num <- floor(length / len_x)
    if (length > len_x * num)
      return(c(rep(x, num), x[1:(length - len_x * num)]))
    else
      return(rep(x, num))
  }
}
