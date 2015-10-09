#' Evaluate outputs of methods according to provided metrics.
#'
#' Given a \code{\link{Metric}} object or list of \code{\link{Metric}} objects,
#' this function evaluates an \code{\link{Output}} object according to these
#' metrics.  The computed values of the metrics are saved to file.
#'
#' This function creates objects of class \code{\link{Evals}} and saves each to
#' file (at dir/model_name/<out_loc>/r<index>_<method_name>_evals.Rdata. Since
#' evaluating metrics is usually (in statistical methodological papers) fast,
#' parallel functionality has not been developed for the evaluation component.
#'
#' @export
#' @param metrics a list of \code{\link{Metric}} objects or a single
#'        \code{\link{Metric}} object
#' @param dir the directory where \code{\link{Model}} object was saved (by
#'        \code{\link{generate_model}})
#' @param model_name the \code{\link{Model}} object's \code{name} attribute
#' @param index the index of a computed \code{\link{Draws}} object.  Can
#'        alternately be a vector of such indices.
#' @param method_names the \code{\link{Method}} objects' \code{name} attributes
#'        as a character vector.
#' @param out_loc (optional) a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.
#' @seealso \code{\link{generate_model}} \code{\link{simulate_from_model}}
#' \code{\link{run_method}}
#' @examples
#' \dontrun{
#'  }
evaluate <- function(metrics, dir = ".", model_name, index, method_names,
                       out_loc = "out") {
  # make sure metrics is a list of Metric objects
  if (class(metrics) == "list") {
    stopifnot(all(unlist(lapply(metrics, function(m) class(m) == "Metric"))))
  } else {
    stopifnot(class(metrics) == "Metric")
    metrics <- list(metrics)
  }
  md <- get_model_dir_and_file(dir, model_name)
  model <- load_model(dir, model_name, more_info = FALSE)
  out_dir <- file.path(md$dir, remove_slash(out_loc))
  index <- sort(index)
  num_metrics <- length(metrics)
  num_methods <- length(method_names)
  out_files <- list()
  ii <- 1
  for (i in seq(index)) {
    for (m in seq(num_methods)) {
      output <- load_outputs(dir, model_name, index[i], method_names[m],
                             out_loc = out_loc)
      evals <- evaluate_single(metrics, model, output)
      out_files[[ii]] <- save_evals_to_file(out_dir, evals)
      ii <- ii + 1
    }
  }
  invisible(out_files)
}

#' Run one or more metrics on outputs.
#'
#' This is an internal function.  Users should call the wrapper function.
#' \code{\link{run_metrics}}. Here "single" refers to a single output (and
#' thus a single method, though not necessarily a single index).
#' The metrics provided are run and saved together in a file.
#'
#' @param metrics a list of \code{\link{Metric}} objects
#' @param model a \code{\link{Model}} object
#' @param output a \code{\link{Output}} object
evaluate_single <- function(metrics, model, output) {
  if (class(metrics) == "Metric") metrics <- list(metrics)
  else if (is.list(metrics)) {
    stopifnot(all(unlist(lapply(metrics, class)) == "Metric"))
  }
  stopifnot(class(model) == "Model")
  stopifnot(class(output) == "Output")
  evals <- list()
  for (rid in names(output@out)) {
    evals[[rid]] <- list()
    for (m in seq_along(metrics)) {
      evals[[rid]][[metrics[[m]]@name]] <- metrics[[m]]@metric(model,
                                                             output@out[[rid]])
    }
  }
  metric_names <- unlist(lapply(metrics, function(m) m@name))
  metric_labels <- unlist(lapply(metrics, function(m) m@label))
  new("Evals", model_name = model@name,
               model_label = model@label,
               index = output@index,
               method_name = output@method_name,
               method_label = output@method_label,
               metric_name = metric_names,
               metric_label = metric_labels,
               evals = evals)
}

#' Load one or more Evals objects from file.
#'
#' After \code{\link{evaluate}} has been called, this function can
#' be used to load one or more of the saved \code{\link{Evals}} object(s).
#' If multiple indices are provided, these will be combined by index into a
#' new single \code{\link{Evals}} object.  If multiple methods are provided,
#' a list of \code{\link{Evals}} objects will be returned.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the \code{\link{Model}} object's \code{name}
#' @param index a vector of positive integers.
#' @param method_name the \code{name} of one or more \code{\link{Method}}
#'        objects.
#' @param metric_names (optional) a character vector of which elements of
#'        evals should be loaded. If NULL, then all elements are loaded.
#' @param out_loc only needed if it was used in call to
#'        \code{\link{run_method}}.
#' @seealso \code{\link{run_method}} \code{\link{load_model}} \code{\link{load_draws}}
#' @examples
#' \dontrun{
#' }
load_evals <- function(dir, model_name, index, method_name,
                         metric_names = NULL, out_loc = "out") {
  md <- get_model_dir_and_file(dir, model_name)
  index <- sort(unique(index))
  num_methods <- length(method_name)
  if (!is.null(metric_names)) metric_names <- sort(metric_names)
  out_dir <- file.path(md$dir, remove_slash(out_loc))
  evals_list <- list() # will have length num_methods
  if (length(index) == 1) {
    evals_files <- sprintf("%s/r%s_%s_evals.Rdata", out_dir, index,
                           method_name)
    for (m in seq(num_methods)) {
      tryCatch(load(evals_files[m]),
               warning=function(w)
                 stop(sprintf("Could not find output file at %s.",
                              evals_files[m])))
      if (!is.null(metric_names))
        evals <- subset_evals(evals, metric_names)
      evals_list[[m]] <- evals
    }
    ifelse(num_methods == 1, return(evals_list[[1]]), return(evals_list))
  }
  for (m in seq(num_methods)) {
    newevals <- list()
    evals_files <- sprintf("%s/r%s_%s_evals.Rdata", out_dir, index,
                           method_name[m])
    for (i in seq_along(index)) {
      tryCatch(load(evals_files[i]),
               warning=function(w)
                 stop(sprintf("Could not find evals file at %s.",
                              evals_files[i])))
      if (!is.null(metric_names)) {
        evals <- subset_evals(evals, metric_names)
      }
      newevals <- c(newevals, evals@evals)
    }
    evals@index <- index
    evals@evals <- newevals
    evals_list[[m]] <- evals
  }
  ifelse(num_methods == 1, return(evals_list[[1]]), return(evals_list))
}


save_evals_to_file <- function(out_dir, evals) {
  file <- sprintf("%s/r%s_%s_evals.Rdata", out_dir, evals@index,
                  evals@method_name)
  save(evals, file = file)
  catsim("..Evaluated", evals@method_label, "in terms of",
         paste(evals@metric_label, collapse = ", "), fill = TRUE)
  file
}



subset_evals <- function(evals, metric_names) {
  for (j in seq(length(evals@evals))) {
    if (!(all(metric_names %in% names(evals@evals[[j]]))))
      stop("Element ", names(evals@evals)[j], " does not match metric_names.")
    evals@evals[[j]] <- evals@evals[[j]][metric_names]
  }
  ii <- match(metric_names, evals@metric_name)
  evals@metric_name <- evals@metric_name[ii]
  evals@metric_label <- evals@metric_label[ii]
  evals
}
