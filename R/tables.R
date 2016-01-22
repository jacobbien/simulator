#' Make a table of a metric for each pair of models and methods
#'
#' Each row of the table corresponds to a different model and each column
#' to a different method.  The metric must be a scalar.  The way in which
#' standard error is shown (or not shown) is controlled by \code{se_format}.
#'
#' Uses \code{knitr}'s function \code{kable} to put table in various formats,
#' including latex, html, markdown, etc.
#'
#' @param evals_list a list of one or more objects of class \code{\link{Evals}}.
#'        Each evals object should just differ by model_name.
#' @param metric_name the name of a metric to tabulate.  Must be scalar valued.
#' @param method_names character vector indicating methods to include in table.
#'        If NULL, then will include all methods found in evals_list.
#' @param caption caption of plot
#' @param se_format format of the standard error
#' @param output_type see \code{knitr::kable}'s argument format for options.
#'        Default is "latex" but other options include "html" and "markdown"
#' @param format_args arguments to pass to the function \code{\link{format}}
#' @param na_string what to write in table in place of NA
#' @export
tabulate_eval <- function(evals_list, metric_name, method_names = NULL,
                          caption = NULL,
                          se_format = c("Paren", "PlusMinus", "None"),
                          output_type = "latex",
                          format_args = list(nsmall = 0,
                                             digits = NULL,
                                             scientific = FALSE),
                          na_string = "--") {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("To use this function, knitr must be installed.", call. = FALSE)
  }
  if (class(evals_list) == "Evals") evals_list <- list(evals_list)
  stopifnot(class(evals_list) == "list", lapply(evals_list, class) == "Evals")
  model_labels <- unlist(lapply(evals_list, function(evals) evals@model_label))
  method_labels <- unique(unlist(lapply(evals_list,
                                        function(evals) evals@method_label)))
  meth_names <- unique(unlist(lapply(evals_list,
                                 function(evals) evals@method_name)))
  metric_label <- NA
  if (is.null(method_names)) {
    method_names <- meth_names
  } else {
    # only keep those methods in method_names
    namdiff <- setdiff(method_names, meth_names)
    if (length(namdiff) > 0)
      stop("Method with name '", namdiff[1], "' not found in any evals.")
    method_labels <- method_labels[meth_names %in% method_names]
  }
  tabm <- tabse <- tabn <- matrix(NA,
                                  length(model_labels),
                                  length(method_labels))
  for (i in seq_along(evals_list)) {
    if (!(metric_name %in% evals_list[[i]]@metric_name)) {
      # metric has not been computed for any methods in this model.
      next
    }
    ev <- subset_evals(evals_list[[i]], metric_names = metric_name)
    for (j in seq_along(method_names)) {
      if (!(method_names[j] %in% ev@method_name)) {
        # method j has not been computed for model i
        next
      }
      if (is.na(metric_label)) metric_label <- ev@metric_label
      values <- unlist(ev@evals[[method_names[j]]])
      if (length(values) != length(ev@evals[[method_names[j]]]))
        stop("Metric tabulated must be scalar valued.")
      tabm[i, j] <- mean(values)
      tabn[i, j] <- length(values)
      if (se_format[1] != "None")
        tabse[i, j] <- sd(values) / sqrt(length(values))
    }
  }
  tabm_str <- do.call("format", c(list(x = tabm), format_args))
  if (se_format[1] == "None")
    tab <- tabm_str
  else {
    tabse_str <- do.call("format", c(list(x = tabse), format_args))
    if (se_format[1] == "Paren")
      tab <- sprintf("%s (%s)", tabm_str, tabse_str)
    else if (se_format[1] == "PlusMinus") {
      if (output_type == "latex")
        pm <- "$\\pm$"
      else
        pm <- "&plusmn"
      tab <- sprintf("%s %s %s", tabm_str, pm, tabse_str)
    }
    else stop("Unrecognized value for se_format.")
  }
  tab <- matrix(tab, nrow = nrow(tabm))
  rownames(tab) <- model_labels
  colnames(tab) <- method_labels
  tab[is.na(tabm)] <- na_string
  if (is.null(caption)) {
    if (is.na(metric_label)) metric_label <- metric_name
    sdn <- sd(tabn, na.rm = TRUE)
    if (!is.na(sdn)) {
      # some cells of table are not NA
      if (sdn == 0)
        ndraws <- min(tabn, na.rm = TRUE)
      else ndraws <- "differing numbers of"
    }
    caption <- sprintf("A comparison of %s (averaged over %s replicates).",
                       metric_label, ndraws)
  }
  str <- sprintf("generated by simulator on %s.", date())
  if (output_type == "latex") str <- paste("%", str)
  else if (output_type %in% c("markdown", "html"))
    str <- paste("<!--", str, "-->")
  else str <- ""
  cat(str, fill = TRUE)
  knitr::kable(tab, format = output_type, caption = caption, escape = FALSE)
}