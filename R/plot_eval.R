
#' Make a boxplot of a metric for each method
#'
#' If evals is a \code{listofEvals}, then each model will be its own plot.
#'
#' @param object an object of class \code{\linkS4class{Simulation}},
#'        \code{\linkS4class{Evals}}, or \code{listofEvals}
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
plot_eval <- function(object, metric_name, use_ggplot2 = TRUE, main,
                      facet_mains, ylab, ylim, include_zero = FALSE, angle = 0,
                      ...) {
  ev_list <- get_evals_list(object)
  stopifnot(unlist(lapply(ev_list, function(e) metric_name %in% e@metric_name)))
  if (length(ev_list) == 1) {
    if (missing(main))
      main <- ev_list[[1]]@model_label
    facet_mains <- main
  } else {
    # we have multiple facets
    if (missing(facet_mains))
      facet_mains <- unlist(lapply(ev_list, function(e) e@model_label))
  }
  if (missing(ylab))
    ylab <- ev_list[[1]]@metric_label[ev_list[[1]]@metric_name == metric_name]
  ev_list <- subset_evals(ev_list, metric_names = metric_name)
  evals_df <- as.data.frame(ev_list)
  if(any(table(evals_df[, c("Model", "Method", "Draw")]) != 1)) {
    stop("plot_eval should only be called on metrics that are scalar-valued.",
         " Plot a vector-valued metric versus another using plot_evals.")
  }
  if (missing(ylim)) {
    ylim <- range(evals_df[[metric_name]])
    if (include_zero) ylim <- range(ylim, 0)
  }
  method_names <- unique(unlist(lapply(ev_list, function(e) e@method_name)))
  method_labels <- unique(unlist(lapply(ev_list, function(e) e@method_label)))
  nrow <- floor(sqrt(length(ev_list)))
  ncol <- ceiling(length(ev_list) / nrow)
  if (use_ggplot2) return(ggplot_eval(evals_df, metric_name, method_names,
                                      method_labels, main, facet_mains, ylab,
                                      ylim, nrow, ncol))
  par(mfrow = c(nrow, ncol))
  for (i in seq_along(evals)) {
    evals_df <- as.data.frame(ev_list[[i]])
    if (angle == 0) {
      boxplot(as.formula(paste0(metric_name, "~ Method")), data = evals_df,
              names = ev_list[[i]]@method_label, ylab = ylab, xlab = "Method",
              main = facet_mains[i], ylim = ylim, ...)
    } else {
      boxplot(as.formula(paste0(metric_name, "~ Method")), data = evals_df,
              names = ev_list[[i]]@method_label, xaxt = "n", ylab = ylab,
              xlab = "Method",
              main = facet_mains[i], ylim = ylim, ...)
      axis(1, labels=FALSE)
      text(x =  seq_along(ev_list[[i]]@method_label),
           y = par("usr")[3] - 0.03 * diff(par("usr"))[3],
           srt = angle,
           adj = 1, labels = ev_list[[i]]@method_label, xpd = TRUE)
    }
  }
  if (!missing(main) & length(ev_list) > 1)
    title(main, outer = TRUE, line = -1)
}

#' Make a boxplot of a metric for each method using ggplot2
#' @keywords internal
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
  if (length(unique(facet_mains)) < length(facet_mains))
    stop("Labels of individual plots must be unique. Pass facet_mains explicitly.")
  levels(evals_df$Model) <- facet_mains
  ggplot2::ggplot(evals_df, ggplot2::aes_string("Method", metric_name)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Method", y = ylab, title = main) +
    ggplot2::scale_x_discrete(breaks = method_name, labels = method_label) +
    ggplot2::facet_wrap("Model", nrow = nrow, ncol = ncol) +
    ggplot2::scale_y_continuous(limits = ylim)
}
