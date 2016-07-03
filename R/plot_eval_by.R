
#' Plot a metric across multiple values of a model parameter
#'
#' This function is to be used on simulations in which
#' \code{\link{generate_model}} was called using the \code{vary_along}
#' parameter.  When this is a single (scalar) numeric parameter, a single plot
#' is created in which the x-axis is this parameter.  Eventually, this function
#' should handle one or two categorical variables (in which facets are used)
#' and one categorical combined with one continuous variable.
#'
#' When \code{type} is "raw", the individual evals are shown (one point per
#' model-draw-method triplet) along with a loess smooth.  When \code{type} is
#' "aggregated", then \code{center_aggregator} and \code{spread_aggregator}
#' are used.  \code{center_aggregator} is used to draw a single line per method
#' in which the individual evals computed for each draw has been been
#' aggregated in some way.  By default, the \code{mean_aggregator} is used,
#' which simply averages the evals computed across all draws.  When
#' \code{spread_aggregator} is non-NULL, "error bars" are drawn with
#' (half)widths computed using \code{spread_aggregator}.  By default, the
#' \code{se_aggregator} is used, which gives an estimate of the standard error
#' of the sample mean.
#'
#' The arguments method_col, method_lty, method_lwd, method_pch only
#' apply when use_ggplot2 is FALSE.
#'
#' @param sim an object of class \code{\linkS4class{Simulation}}
#' @param metric_name the name of a metric to plot (ignored if custom
#'        aggregator is provided)
#' @param varying character vector giving the name of a parameter that is
#'        varied across the models in evals. For now, this parameter must be
#'        numeric and there cannot be multiple models having the same value
#'        of this parameter.
#' @param type if "aggregated" then shows line with error bars (line represents
#'        center_aggregator and error bars represent spread_aggregator; by
#'        default these are sample mean and estimated standard error); if
#'        \code{type} is "raw" then shows the raw data as points (with smoother
#'        overlayed)
#' @param center_aggregator ignored if \code{type} is "raw".  When NULL (which
#'        is default), the sample mean aggregator is used.  User can write
#'        specialized aggregators (see definition of class
#'        \code{\linkS4class{Aggregator}}) as necessary, for example, when the
#'        evaluated metric is not scalar-valued.
#' @param spread_aggregator ignored if \code{type} is "raw".  When NULL (which
#'        is default), the sample mean aggregator is used.  User can write
#'        specialized aggregators (see definition of class
#'        \code{\linkS4class{Aggregator}}) as necessary, for example, when the
#'        evaluated metric is not scalar-valued. Set \code{spread_aggregator}
#'        to \code{NA} to hide error bars.
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
#' @param method_col color to use for each method
#' @param method_lty line style to use for each method
#' @param method_lwd line thickness to use for each method
#' @param method_pch point style to use for each method (default is that no
#'        points, only lines are drawn)
#' @param ... additional arguments to pass to \code{plot} (only when
#'        \code{use_ggplot2 = FALSE}).
#' @export
#' @examples
#' \dontrun{
#'  # suppose previously we had run the following:
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model,
#'                   n = list(10, 20, 30),
#'                   vary_along = "n") %>%
#'    simulate_from_model(nsim = 50, index = 1:3) %>%
#'    run_method(my_example_method) %>%
#'    evaluate(my_example_loss)
#'    # then we could plot this
#'    plot_eval_by(sim, "myloss", varying = "n", include_zero = TRUE)
#'  }
plot_eval_by <- function(sim, metric_name, varying,
                         type = c("aggregated", "raw"),
                         center_aggregator = NULL,
                         spread_aggregator = NULL,
                         use_ggplot2 = TRUE,
                         main,
                         xlab, ylab, xlim, ylim, include_zero = FALSE,
                         legend_location = "topright",
                         method_col = seq(num_methods),
                         method_lty = rep(1, num_methods),
                         method_lwd = rep(1, num_methods),
                         method_pch = rep(1, num_methods), ...) {
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
    stop("For now, cannot have multiple models having same value of 'varying'. Use subset_simulation.")
  model_names <- lapply(sim@model_refs, function(m) m@name)
  # load evals
  e <- evals(sim)
  type <- type[1]
  if (type == "raw" | is.null(center_aggregator) | is.null(spread_aggregator))
    if(!all(unlist(lapply(e, function(ee) metric_name %in% ee@metric_name))))
      stop("'", metric_name, "' is not found in one or more Evals.")
  method_names <- lapply(e, function(ee) ee@method_name)
  if (length(unique(method_names)) != 1)
    stop("All models must have same methods.")
  method_names <- method_names[[1]]
  num_methods <- length(method_names)
  method_labels <- e[[1]]@method_label
  if (type == "raw" | is.null(center_aggregator) | is.null(spread_aggregator))
    metric_label <- e[[1]]@metric_label[e[[1]]@metric_name == metric_name]
  # prepare data.frame that has varying, metric_name, and "Method" as columns
  df <- as.data.frame(e)
  ii <- match(df[["Model"]], model_names)
  df[[varying]] <- unlist(vals)[ii] # add column 'varying' to data.frame
  # check type
  type <- type[1]
  if (!(type %in% c("aggregated", "raw"))) stop("Unrecognized type.")
  if (type == "aggregated") {
    val_varied <- rep(NA, length(e))
    for (i in seq_along(e)) {
      imodel <- which(model_names == e[[i]]@model_name)
      val_varied[i] <- vals[[imodel]]
    }
    if (is.null(center_aggregator)) {
      # create an aggregator that computes the sample mean of
      # the "metric_name" evals
      center_aggregator <- make_scalar_aggregator("Mean",
                                                  metric_name,
                                                  metric_label,
                                                  mean)
    }
    if (is.null(spread_aggregator)) {
      # create an aggregator that computes an estimate of the standard error of
      # the sample mean of the "metric_name" evals
      se <- function(a) sd(a) / sqrt(length(a))
      spread_aggregator <- make_scalar_aggregator("Standard error",
                                                  metric_name,
                                                  metric_label,
                                                  se)
    }
    center <- aggregate_evals(e, center_aggregator)
    if (isS4(spread_aggregator)) {
      spread <- aggregate_evals(e, spread_aggregator)
      lower <- center - spread
      upper <- center + spread
    }
  }
  # plotting parameters:
  if (missing(main)) main <- sprintf("Varying %s", varying)
  if (missing(xlab)) xlab <- varying
  if (missing(ylab)) {
    if (type == "raw")
      ylab <- metric_label
    else
      ylab <- center_aggregator@label
  }
  if (missing(xlim)) xlim <- range(df[[varying]])
  if (missing(ylim)) {
    if (type == "aggregated") {
      if (isS4(spread_aggregator))
        ylim <- range(upper, lower)
      else
        ylim <- range(center)
    }
    else if (type == "raw")
      ylim <- range(df[[metric_name]])
    if (include_zero) ylim <- range(0, ylim)
  }
  method_col <- recycle(method_col, num_methods)
  method_lwd <- recycle(method_lwd, num_methods)
  method_lty <- recycle(method_lty, num_methods)
  method_pch <- recycle(method_pch, num_methods)
  if (use_ggplot2) {
    if (type == "aggregated") {
      df2 <- stats::reshape(as.data.frame(center),
                            direction = "long",
                            varying = colnames(center),
                            v.names = ".center",
                            idvar = "Model",
                            ids = rownames(center),
                            timevar = "Method",
                            times = colnames(center))
      df2[[".varying"]] <- rep(val_varied, ncol(center))
      if (isS4(spread_aggregator)) {
        df2[[".upper"]] <- as.numeric(upper)
        df2[[".lower"]] <- as.numeric(lower)
      }
      g <- ggplot2::ggplot(df2, ggplot2::aes_string(".varying",
                                                    ".center",
                                                    color = "Method")) +
           ggplot2::labs(x = xlab, y = ylab, title = main) +
           ggplot2::scale_colour_discrete(labels = method_labels) +
           ggplot2::ylim(ylim) +
           ggplot2::xlim(xlim) +
           ggplot2::geom_line(ggplot2::aes_string(position = ".center")) +
           ggplot2::geom_point(ggplot2::aes_string(position = ".center"))
      if (isS4(spread_aggregator)) {
        g <- g + ggplot2::geom_errorbar(ggplot2::aes_string(ymin = ".lower",
                                                            ymax = ".upper",
                                                            width = 0.1,
                                                            position = ".center"))
      }

    } else if (type == "raw") {
      g <- ggplot2::ggplot(df, ggplot2::aes_string(varying, metric_name)) +
           ggplot2::labs(x = xlab, y = ylab, title = main) +
           ggplot2::scale_colour_discrete(labels = method_labels) +
           ggplot2::ylim(ylim) +
           ggplot2::xlim(xlim) +
           ggplot2::geom_point(ggplot2::aes_string(color = "Method",
                                                   group = "Method")) +
           ggplot2::stat_smooth(ggplot2::aes_string(color = "Method",
                                                    group = "Method"))
    }
    return(g)
  }
  # rest of function is for use_ggplot = FALSE case
  palette(options("simulator.color_palette")[[1]])
  plot(0, 0, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, main = main,
       type = "n", ...)
  if (type == "aggregated") {
    for (m in seq_along(method_names)) {
      points(val_varied, center[, m], col = method_col[m], pch = 20,
             lty = method_lty[m], lwd = method_lwd[m], type = "o")
      if (isS4(spread_aggregator)) {
        segments(x0 = val_varied, y0 = lower[, m],
                 y1 = upper[, m], col = method_col[m])
      }
    }
  } else if (type == "raw") {
    for (m in seq_along(method_names)) {
      dfm <- df[df[["Method"]] == method_names[m], ] # subset by method
      points(dfm[[varying]], dfm[[metric_name]], col = method_col[m],
             pch = method_pch[m])
      smooth <- loess(dfm[[metric_name]] ~ dfm[[varying]])
      xx <- seq(min(dfm[[varying]]), max(dfm[[varying]]), length = 20)
      lines(xx, predict(smooth, newdata = xx), col = method_col[m],
            lty = method_lty[m], lwd = method_lwd[m])
    }
  }
  if (is.character(legend_location)) {
    legend(legend_location, legend = method_labels, col = method_col,
           pch = method_pch, lty = method_lty, lwd = method_lwd)
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
