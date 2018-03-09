#' Make a table of a metric for each pair of models and methods
#'
#' Each row of the table corresponds to a different model and each column
#' to a different method.  The metric must be a scalar.  The way in which
#' standard error is shown (or not shown) is controlled by \code{se_format}.
#'
#' Uses \code{knitr}'s function \code{kable} to put table in various formats,
#' including latex, html, markdown, etc.
#'
#' @param object an object of class \code{\linkS4class{Simulation}},
#'        \code{\linkS4class{Evals}}, or \code{listofEvals}.
#'        Each evals object should just differ by model_name.
#' @param metric_name the name of a metric to tabulate.  Must be scalar valued.
#' @param method_names character vector indicating methods to include in table.
#'        If NULL, then will include all methods found in object's evals.
#' @param caption caption of plot. If NULL, then default caption used; if FALSE
#'        then no caption (and returns tabular without table).
#' @param center_aggregator When NULL (which is default), the sample mean
#'        aggregator is used.  User can write specialized aggregators (see
#'        definition of class \code{\linkS4class{Aggregator}}) as necessary,
#'        for example, when the evaluated metric is not scalar-valued.
#' @param spread_aggregator When NULL (which is default), the standard error 
#'        of the sample mean is used.  User can write specialized aggregators (see
#'        definition of class \code{\linkS4class{Aggregator}}) as necessary,
#'        for example, when the evaluated metric is not scalar-valued. Set
#'        \code{spread_aggregator} to \code{NA} to hide error bars.
#' @param se_format format of the standard error
#' @param output_type see \code{\link[knitr]{kable}}'s argument format for options.
#'        Default is "latex" but other options include "html" and "markdown"
#' @param format_args arguments to pass to the function \code{\link{format}}
#' @param na_string what to write in table in place of NA
#' @param bold puts in bold the value that is smallest/largest for each model
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
#'    tabulate_eval(sim, "myloss")
#'  }
tabulate_eval <- function(object, metric_name, method_names = NULL,
                          caption = NULL,
                          center_aggregator = NULL,
                          spread_aggregator = NULL,
                          se_format = c("Paren", "PlusMinus", "None"),
                          output_type = "latex",
                          format_args = list(nsmall = 0,
                                             digits = NULL,
                                             scientific = FALSE),
                          na_string = "--",
                          bold = c("None", "Smallest", "Largest")) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("To use this function, knitr must be installed.", call. = FALSE)
  }
  ev_list <- get_evals_list(object)
  if (length(ev_list) == 0)
    stop("Passed object does not have Evals to tabulate.")
  stopifnot("list" %in% class(ev_list), lapply(ev_list, class) == "Evals")
  model_labels <- unlist(lapply(ev_list, function(evals) evals@model_label))
  method_labels <- unique(unlist(lapply(ev_list,
                                        function(evals) evals@method_label)))
  meth_names <- unique(unlist(lapply(ev_list,
                                     function(evals) evals@method_name)))
  if (is.null(method_names)) {
    method_names <- meth_names
  } else {
    # only keep those methods in method_names
    namdiff <- setdiff(method_names, meth_names)
    if (length(namdiff) > 0)
      stop("Method with name '", namdiff[1], "' not found in any evals.")
    method_labels <- method_labels[meth_names %in% method_names]
  }
  e <- subset_evals(ev_list, method_names = method_names)
  metric_label <- e[[1]]@metric_label[e[[1]]@metric_name == metric_name]
  if (is.null(center_aggregator)) {
    # create an aggregator that computes the sample mean of
    # the "metric_name" evals
    center_aggregator <- make_scalar_aggregator("Mean",
                                                metric_name,
                                                metric_label,
                                                mean)
  } else {
    # user is supplying a custom center_aggregator
    if (is.null(spread_aggregator)) 
      warning("A custom spread_aggregator should ",
              "be used when a custom center_aggregator is being used. ",
              "Another option is simply to take spread_aggregator = NA.")
  }
  center_label <- center_aggregator@label
  if (is.null(spread_aggregator)) {
    # create an aggregator that computes an estimate of the standard error of
    # the sample mean of the "metric_name" evals
    se <- function(a) sd(a) / sqrt(length(a))
    spread_aggregator <- make_scalar_aggregator("Standard error",
                                                metric_name,
                                                metric_label,
                                                se)
  }
  num_sim_aggregator <- new_aggregator("Number", function(ev) length(ev))
  center <- aggregate_evals(e, center_aggregator)
  num_sim <- aggregate_evals(e, num_sim_aggregator)
  if (isS4(spread_aggregator))
    spread <- aggregate_evals(e, spread_aggregator)
  tabm_str <- do.call("format", c(list(x = center), format_args))
  bold <- bold[1]
  if (bold != "None") {
    if (bold == "Smallest")
      ii <- apply(center, 1, which.min)
    else if (bold == "Largest")
      ii <- apply(center, 1, which.max)
    else stop("Not a recognized argument for bold.")
    ii <- cbind(seq_along(ii), ii)
    tabm_str[ii] <- add_bold(tabm_str[ii], output_type)
  }
  if (se_format[1] == "None")
    tab <- tabm_str
  else {
    if (!isS4(spread_aggregator))
      stop("se_format must be None if spread_aggregator is NA.")
    tabse_str <- do.call("format", c(list(x = spread), format_args))
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
  tab <- matrix(tab, nrow = nrow(center))
  rownames(tab) <- model_labels
  colnames(tab) <- method_labels
  tab[is.na(center)] <- na_string
  if (is.null(caption)) {
    ndraws <- unique(as.vector(num_sim))
    ndraws <- ndraws[!is.na(ndraws)]
    if (length(ndraws) > 1) ndraws <- "differing numbers of"
    caption <- sprintf("A comparison of %s (averaged over %s replicates).",
                       center_label, ndraws)
  } else if (is.logical(caption) & !caption)
    caption <- NULL # this specifies knitr::kable
  else if (!is.character(caption))
  stop("Caption must be of class character or NULL or FALSE.")
  str <- sprintf("generated by simulator on %s.", date())
  if (output_type == "latex") str <- paste("%", str)
  else if (output_type %in% c("markdown", "html"))
    str <- paste("<!--", str, "-->")
  else str <- ""
  catsim(str, sep = "\n")
  knitr::kable(tab, format = output_type, caption = caption, escape = FALSE)
}

#' Make a string bold in a certain format
#'
#' For example, in latex it would take "2" and output "{\\bf 2}"; in html
#' it would output "<b>2</b>".
#'
#' @param str string or strings (character) to make bold
#' @param output_type output type (see knitr::kable's format)
add_bold <- function(str, output_type) {
  if (output_type == "latex")
    return(paste0("{\\bf ", str, "}"))
  if (output_type == "html")
    return(paste0("<b>", str, "</b>"))
  # do the markdown double asterisk as default
  return(paste0("**", str, "**"))
}
