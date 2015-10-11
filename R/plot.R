library(ggplot2)

#' Make a boxplot of a metric for each method using ggplot2
#'
#' @param evals_df output of \code{\link{as.data.frame.Evals}}
ggplot_eval <- function(evals_df, metric_name, main, ylab) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("To use this function, ggplot2 must be installed.", call. = FALSE)
  }
  ggplot2::ggplot(evals_df, ggplot2::aes_string("Method", metric_name)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Method", y = ylab, title = main) +
    ggplot2::scale_x_discrete(breaks = evals@method_name,
                              labels = evals@method_label)
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
  if (use_ggplot2) return(ggplot_eval(evals_df, metric_name, main = main,
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
