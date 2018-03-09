
#' Plot a metric's value for each method
#'
#' When the evaluted metric is scalar-valued, this functions makes a boxplot of
#' this metric for each method.  When the metric is vector-valued, this function
#' makes a curve with this metric on the y-axis, with one curve for each method
#' (the x-axis is the corresponding entry of that metric's vector). If evals is
#' a \code{listofEvals}, then each model will be its own plot.
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
#' @seealso \code{\link{plot_evals}} \code{\link{plot_eval_by}}
#' \code{\link{tabulate_eval}}
#' @examples
#' \dontrun{
#'  # suppose previously we had run the following:
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model, n = 20) %>%
#'    simulate_from_model(nsim = 50, index = 1:3) %>%
#'    run_method(my_example_method) %>%
#'    evaluate(my_example_loss)
#'    # then we could plot this
#'    plot_eval(sim, "myloss") # "myloss" is my_example_loss@name
#'  }
plot_eval <- function(object, metric_name, use_ggplot2 = TRUE, main,
                      facet_mains, ylab, ylim, include_zero = FALSE, angle = 0,
                      ...) {
  ev_list <- get_evals_list(object)
  if (length(ev_list) == 0) stop("Passed object does not have Evals to plot.")
  if (!any(unlist(lapply(ev_list,
                         function(e) metric_name %in% e@metric_name)))) {
    stop("Passed object does not have Evals with this metric name.")
  }
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
    return(plot_evals(object, metric_name_x = NULL, metric_name_y = metric_name,
               use_ggplot2 = use_ggplot2, main = main, facet_mains = facet_mains,
               ylab = ylab, ylim = ylim, include_zero = include_zero))
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
  if (nrow != 1 | ncol != 1)
    par(mfrow = c(nrow, ncol))

  for (i in seq_along(ev_list)) {
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
  if (length(unique(evals_df[["Model"]])) == 1) {
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
