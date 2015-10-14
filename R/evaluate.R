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
  evals[[output@method_name]] <- list()
  metric_names <- unlist(lapply(metrics, function(m) m@name))
  metric_labels <- unlist(lapply(metrics, function(m) m@label))
  for (rid in names(output@out)) {
    evals[[1]][[rid]] <- list()
    for (m in seq_along(metrics)) {
      evals[[1]][[rid]][[metric_names[m]]] <- metrics[[m]]@metric(model,
                                                             output@out[[rid]])
    }
  }
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
#' @param method_names the \code{name} of one or more \code{\link{Method}}
#'        objects.
#' @param metric_names (optional) a character vector of which elements of
#'        evals should be loaded. If NULL, then all elements are loaded.
#' @param out_loc only needed if it was used in call to
#'        \code{\link{run_method}}.
#' @seealso \code{\link{load_model}} \code{\link{load_draws}} \code{\link{as.data.frame.Evals}}
#' @examples
#' \dontrun{
#' }
load_evals <- function(dir, model_name, index, method_names,
                         metric_names = NULL, out_loc = "out") {
  md <- get_model_dir_and_file(dir, model_name)
  index <- sort(unique(index))
  num_methods <- length(method_names)
  method_names <- sort(method_names)
  method_labels <- rep(NA, num_methods)
  out_dir <- file.path(md$dir, remove_slash(out_loc))
  newevals <- list()
  for (m in seq(num_methods)) {
    newevals[[method_names[m]]] <- list()
    evals_files <- sprintf("%s/r%s_%s_evals.Rdata", out_dir, index,
                           method_names[m])
    for (i in seq_along(index)) {
      tryCatch(load(evals_files[i]),
               warning=function(w)
                 stop(sprintf("Could not find evals file at %s.",
                              evals_files[i])))
      if (!is.null(metric_names)) {
        evals <- subset_evals(evals, metric_names = metric_names)
      }
      if (m > 1) {
        this_metric_names <- names(evals@evals[[1]][[1]])
        prev_metric_names <- names(newevals[[m - 1]][[1]])
        if (!all(this_metric_names == prev_metric_names))
          stop("Cannot create Evals object with methods having different sets",
               " of metrics computed.  Not all methods have ",
               paste(setdiff(union(this_metric_names, prev_metric_names),
                             intersect(this_metric_names, prev_metric_names)),
                     collapse = ", "),
               " computed.")
      }
      newevals[[m]] <- c(newevals[[m]], evals@evals[[1]])
    }
    method_labels[m] <- evals@method_label
  }
  evals@index <- index
  evals@method_name <- method_names
  evals@method_label <- method_labels
  evals@evals <- newevals
  return(evals)
}


save_evals_to_file <- function(out_dir, evals) {
  file <- sprintf("%s/r%s_%s_evals.Rdata", out_dir, evals@index,
                  evals@method_name)
  save(evals, file = file)
  catsim("..Evaluated", evals@method_label, "in terms of",
         paste(evals@metric_label, collapse = ", "), fill = TRUE)
  file
}

#' Reduce an Evals object to a subset of methods and/or metrics
#'
#' If \code{method_names} is \code{NULL}, then subsetting is not done over
#' methods.  Likewise for \code{metric_names}.
#'
#' @param evals an object of class \code{\link{Evals}}.
#' @param method_names a character vector of method names
#' @param metric_names a character vector of metric names
#' @export
subset_evals <- function(evals, method_names = NULL, metric_names = NULL) {
  if (!is.null(method_names)) {
    stopifnot(method_names %in% evals@method_name)
    ii <- match(method_names, evals@method_name)
    evals@method_name <- evals@method_name[ii]
    evals@method_label <- evals@method_label[ii]
    evals@evals <- evals@evals[evals@method_name]
  }
  if (!is.null(metric_names)) {
    stopifnot(metric_names %in% evals@metric_name)
    ii <- match(metric_names, evals@metric_name)
    evals@metric_name <- evals@metric_name[ii]
    evals@metric_label <- evals@metric_label[ii]
    for (m in names(evals@evals))
      for (r in names(evals@evals[[m]])) {
        evals@evals[[m]][[r]] <- evals@evals[[m]][[r]][evals@metric_name]
      }
  }
  evals
}
