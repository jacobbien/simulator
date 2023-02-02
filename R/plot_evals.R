
#' Plot one metric versus another for each method
#'
#' This function is used when both evaluated metrics are vector-valued, so a
#' curve is plotted, parametrized by the two metrics.  To plot a single metric
#' that is vector-valued, pass NULL for metric_name_x. This behaves similarly
#' to \code{plot(runif(5))}, in which the x-axis variable is simply \code{1:5}.
#' If evals is a \code{listofEvals}, then each model will be its own plot.
#'
#' @param object an object of class \code{\linkS4class{Simulation}},
#'        \code{\linkS4class{Evals}}, or \code{listofEvals}
#' @param metric_name_x the name of metric to plot on x axis (or NULL)
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
plot_evals <- function(object, metric_name_x, metric_name_y, use_ggplot2 = TRUE,
                       main, facet_mains, xlab, ylab, xlim, ylim,
                       include_zero = FALSE,
                       legend_location = "topright",
                       method_col = seq(num_methods),
                       method_lty = rep(1, num_methods),
                       method_lwd = rep(1, num_methods),
                       method_pch = rep(NA, num_methods), ...) {
  ev_list <- get_evals_list(object)
  if (!any(unlist(lapply(ev_list,
                         function(e) metric_name_y %in% e@metric_name)))) {
    stop("Passed object does not have Evals named ", metric_name_y)
  }
  if (is.null(metric_name_x)) {
    # add a "pseudo" eval for the x-axis like how graphics::plot puts "Index"
    metric_name_x <- options("simulator.plot_evals.index.name")[[1]]
    lab <- options("simulator.plot_evals.index.label")[[1]]
    for (m in seq_along(ev_list)) {
      ev_list[[m]]@metric_name <- c(ev_list[[m]]@metric_name, metric_name_x)
      ev_list[[m]]@metric_label <- c(ev_list[[m]]@metric_label, lab)
      for(meth in names(ev_list[[m]]@evals)) {
        for (rid in names(ev_list[[m]]@evals[[meth]])) {
          # the length of this pseudo eval should match that to be on y-axis
          len <- length(ev_list[[m]]@evals[[meth]][[rid]][[metric_name_y]])
          ev_list[[m]]@evals[[meth]][[rid]][[metric_name_x]] <- seq(len)
        }
      }
    }
  }
  if (length(ev_list) == 0) stop("Passed object does not have Evals to plot.")
  if (!any(unlist(lapply(ev_list,
                         function(e) metric_name_x %in% e@metric_name)))) {
    stop("Passed object does not have Evals named ", metric_name_x)
  }
  method_names <- lapply(ev_list, function(e) e@method_name)
  if (length(unique(method_names)) != 1)
    stop("All models must have same methods.")
  num_methods <- length(method_names[[1]])
  if (length(ev_list) == 1) {
    if (missing(main))
      main <- ev_list[[1]]@model_label
    facet_mains <- main
  } else {
    # we have multiple facets
    if (missing(facet_mains))
      facet_mains <- unlist(lapply(ev_list, function(e) e@model_label))
  }
  if (missing(xlab))
    xlab <- ev_list[[1]]@metric_label[ev_list[[1]]@metric_name == metric_name_x]
  if (missing(ylab))
    ylab <- ev_list[[1]]@metric_label[ev_list[[1]]@metric_name == metric_name_y]
  ev_df <- as.data.frame(ev_list)
  if (missing(xlim)) xlim <- range(ev_df[[metric_name_x]])
  if (missing(ylim)) {
    ylim <- range(ev_df[[metric_name_y]])
    if (include_zero) ylim <- range(0, ylim)
  }
  nrow <- floor(sqrt(length(ev_list)))
  ncol <- ceiling(length(ev_list) / nrow)
  if (use_ggplot2) return(ggplot_evals(ev_df, metric_name_x, metric_name_y,
                                       method_labels = ev_list[[1]]@method_label,
                                       main = main, facet_mains = facet_mains,
                                       xlab = xlab, ylab = ylab,
                                       xlim = xlim, ylim = ylim,
                                       nrow = nrow, ncol = ncol,
                                       legend_location = legend_location))
  stopifnot(length(method_col) == num_methods)
  stopifnot(length(method_lty) %in% c(1, num_methods))
  if (length(method_lty) == 1) method_lty <- rep(method_lty, num_methods)
  stopifnot(length(method_lwd) %in% c(1, num_methods))
  if (length(method_lwd) == 1) method_lwd <- rep(method_lwd, num_methods)
  stopifnot(length(method_pch) == num_methods)

  if (nrow != 1 | ncol != 1)
    par(mfrow = c(nrow, ncol))
  palette(options("simulator.color_palette")[[1]])
  for (i in seq_along(ev_list)) {
    ev_df <- as.data.frame(ev_list[[i]])
    plot(0, 0, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim,
         main = facet_mains[i], type = "n", ...)
    for (r in unique(ev_df[["Draw"]])) {
      for (m in seq_along(method_names[[1]])) {
        ii <- ev_df[["Method"]] == method_names[[1]][m] & ev_df[["Draw"]] == r
        if (sum(ii) == 1 & is.na(method_pch[m])) {
          # there's only a single point, so a line won't show... so override
          # the choice of NA so this method's evals will be visible
          method_pch[m] <- 1
          method_lwd[m] <- NA
        }
        points(ev_df[ii, metric_name_x], ev_df[ii, metric_name_y],
               col = method_col[m], lty = method_lty[m],
               lwd = method_lwd[m], pch = method_pch[m], type = "o")
      }
    }
    if (i == 1 & is.character(legend_location)) {
      legend(legend_location, legend = ev_list[[1]]@method_label, col = method_col,
             pch = method_pch, lty = method_lty, lwd = method_lwd)
    }
  }
  if (!missing(main) & length(ev_list) > 1)
    title(main, outer = TRUE, line = -1)
  palette("default")
  return()
}


ggplot_evals <- function(ev_df, metric_name_x, metric_name_y, method_labels,
                         main, facet_mains, xlab, ylab, xlim, ylim,
                         nrow, ncol, legend_location) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("To use this function, ggplot2 must be installed.", call. = FALSE)
  }
  if (missing(main)) main <- NULL
  if (length(unique(ev_df$Model)) == 1) {
    g <- ggplot2::ggplot(ev_df,
                         ggplot2::aes_string(metric_name_x, metric_name_y)) +
      ggplot2::geom_line(ggplot2::aes_string(
        color = "Method",
        group = "interaction(Method,Draw)")) +
      ggplot2::geom_point(ggplot2::aes_string(
        color = "Method",
        group = "interaction(Method,Draw)")) +
      ggplot2::labs(x = xlab, y = ylab, title = main) +
      ggplot2::scale_colour_discrete(labels = method_labels) +
      ggplot2::ylim(ylim) +
      ggplot2::xlim(xlim)
    if (is.null(legend_location))
      g <- g + ggplot2::theme(legend.position = "none")
    return(g)
  }
  # display multiple facets...
  levels(ev_df[["Model"]]) <- facet_mains
  g <- ggplot2::ggplot(ev_df,
                       ggplot2::aes_string(metric_name_x, metric_name_y)) +
    ggplot2::geom_line(ggplot2::aes_string(
      color = "Method",
      group = "interaction(Method,Draw)")) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    ggplot2::scale_colour_discrete(labels = method_labels) +
    ggplot2::ylim(ylim) +
    ggplot2::xlim(xlim) +
    ggplot2::facet_wrap("Model", nrow = nrow, ncol = ncol)
  if (is.null(legend_location))
    g <- g + ggplot2::theme(legend.position = "none")
  return(g)
}

get_evals_list <- function(object) {
  # object can be an Evals, list of Evals, listofEvals, or Simulation
  if (isS4(object)) {
    if (is(object, "Simulation")) {
      ev <- evals(object)
    } else if (is(object, "Evals"))
      ev <- object
    else stop("Invalid class for 'object'.")
    if ("Evals" %in% class(ev)) {
      ev <- list(ev)
      class(ev) <- c("listofEvals", "list")
    }
    return(ev)
  }
  if (length(class(object)) == 2)
    if (all(class(object) == c("listofEvals", "list")))
      return(object)
  if (length(class(object)) == 1) {
    if (is(object, "list"))
      class(object) <- c("listofEvals", "list")
      return(object)
  }
  stop("Invalid class for 'object'.")
}
