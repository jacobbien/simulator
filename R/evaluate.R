#' Evaluate outputs of methods according to provided metrics.
#'
#' Given a \code{\linkS4class{Metric}} object or list of \code{\linkS4class{Metric}} objects,
#' this function evaluates an \code{\linkS4class{Output}} object according to these
#' metrics.  The computed values of the metrics are saved to file.
#'
#' This function creates objects of class \code{\linkS4class{Evals}} and saves each to
#' file (at dir/model_name/<out_loc>/r<index>_<method_name>_evals.Rdata. Since
#' evaluating metrics is usually (in statistical methodological papers) fast,
#' parallel functionality has not been developed for the evaluation component.
#'
#' @param metrics a list of \code{\linkS4class{Metric}} objects or a single
#'        \code{\linkS4class{Metric}} object
#' @param dir the directory where \code{\linkS4class{Model}} object was saved (by
#'        \code{\link{generate_model}})
#' @param model_name the \code{\linkS4class{Model}} object's \code{name} attribute
#' @param index the index of a computed \code{\linkS4class{Draws}} object.  Can
#'        alternately be a vector of such indices.
#' @param method_names the \code{\linkS4class{Method}} objects' \code{name} attributes
#'        as a character vector.
#' @param out_loc (optional) a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.
evaluate_internal <- function(metrics, dir = ".", model_name, index, method_names,
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
  erefs <- list()
  ii <- 1
  for (i in seq_along(index)) {
    for (m in seq(num_methods)) {
      output <- load_outputs(dir, model_name, index[i], method_names[m],
                             out_loc = out_loc)
      if (any(unlist(lapply(metrics,
                     function(m) "draw" %in% names(formals(m@metric))))))
        draws <- load_draws(dir, model_name, index[i])
      else
        draws <- NULL
      evals <- evaluate_single(metrics, model, output, draws)
      erefs[[ii]] <- save_evals_to_file(out_dir, dir, out_loc, evals)
      ii <- ii + 1
    }
  }
  invisible(erefs)
}

#' Evaluate outputs of methods according to provided metrics.
#'
#' Given a \code{\linkS4class{Metric}} object or list of \code{\linkS4class{Metric}} objects,
#' this function evaluates an \code{\linkS4class{Output}} object according to these
#' metrics.  The computed values of the metrics are saved to file.  The "user"
#' time to run the method (as measured by \code{\link{system.time}}) is added
#' to \code{metrics} by default unless one of the passed metrics has name
#' "time".
#'
#' This function creates objects of class \code{\linkS4class{Evals}} and saves each to
#' file (at dir/model_name/<out_loc>/r<index>_<method_name>_evals.Rdata. Since
#' evaluating metrics is usually (in statistical methodological papers) fast,
#' parallel functionality has not been developed for the evaluation component.
#'
#' @export
#' @param object object of class \code{\linkS4class{OutputRef}} as produced by
#'        \code{\link{run_method}} (or list of such objects). If
#'        \code{object} is a \code{\linkS4class{Simulation}}, then function is applied
#'         to the referenced outputs in that simulation and returns the same
#'        \code{Simulation} object but with references added to the new evals
#'        created.
#' @param metrics a list of \code{\linkS4class{Metric}} objects or a single
#'        \code{\linkS4class{Metric}} object.
#' @seealso \code{\link{generate_model}} \code{\link{simulate_from_model}}
#' \code{\link{run_method}}
#' @examples
#' \dontrun{
#'  # suppose previously we had run the following:
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model, n = 20) %>%
#'    simulate_from_model(nsim = 50, index = 1:3) %>%
#'    run_method(my_example_method)
#'  # then we could add
#'  sim <- evaluate(sim, my_example_loss)
#'  }
evaluate <- function(object, metrics) {
  # make sure metrics is a list of Metric objects
  if (class(metrics) == "list") {
    stopifnot(all(lapply(metrics, class) == "Metric"))
  } else {
    stopifnot(class(metrics) == "Metric")
    metrics <- list(metrics)
  }
  computing_time <- new_metric(name = "time",
                               label = "Computing time (sec)",
                               metric = function(model, out) {
                                 return(as.numeric(out$time[1]))
                               })
  if (!(computing_time@name %in% lapply(metrics, function(m) m@name)))
    metrics <- c(metrics, computing_time)
  if (class(object) == "Simulation")
    output_ref <- output(object, reference = TRUE)
  else
    output_ref <- object
  if (class(output_ref) == "list") {
    if (all(lapply(output_ref, class) == "list")) {
      # if output_ref is a list of lists, recursively apply to each sub-list
      eref <- lapply(output_ref, evaluate, metrics = metrics)
      if (class(object) == "Simulation")
        return(invisible(add(object, eref)))
      return(invisible(eref))
    }
  } else if (class(output_ref) == "OutputRef") {
    output_ref <- list(output_ref)
  } else stop("Invalid class for output_ref.")
  sf <- lapply(output_ref, function(oref) oref@simulator.files)
  if (any(sf != getOption("simulator.files")))
    stop(sprintf("OutputRef's %s must match getOption(\"%s\")",
                 "simulator.files", "simulator.files"))
  eref <- list()
  for (o in seq_along(output_ref)) {
    eref <- c(eref,
              evaluate_internal(metrics,
                                dir = output_ref[[o]]@dir,
                                model_name = output_ref[[o]]@model_name,
                                index = output_ref[[o]]@index,
                                method_names = output_ref[[o]]@method_name,
                                out_loc = output_ref[[o]]@out_loc))
  }
  if (class(object) == "Simulation")
    return(invisible(add(object, eref)))
  invisible(eref)
}

#' Run one or more metrics on outputs.
#'
#' This is an internal function.  Users should call the wrapper function
#' \code{\link{evaluate}}. Here "single" refers to a single output (and
#' thus a single method, though not necessarily a single index).
#' The metrics provided are run and saved together in a file.
#'
#' @param metrics a list of \code{\linkS4class{Metric}} objects
#' @param model a \code{\linkS4class{Model}} object
#' @param output a \code{\linkS4class{Output}} object
#' @param draws (optional) a \code{\linkS4class{Draws}} object or NULL
evaluate_single <- function(metrics, model, output, draws = NULL) {
  if (class(metrics) == "Metric") metrics <- list(metrics)
  else if (is.list(metrics)) {
    stopifnot(all(unlist(lapply(metrics, class)) == "Metric"))
  }
  stopifnot(class(model) == "Model")
  stopifnot(class(output) == "Output")
  ev <- list()
  metric_names <- unlist(lapply(metrics, function(m) m@name))
  metric_labels <- unlist(lapply(metrics, function(m) m@label))
  for (rid in names(output@out)) {
    ev[[rid]] <- list()
    for (m in seq_along(metrics)) {
      mname <- metric_names[m]
      if ("draw" %in% names(formals(metrics[[m]]@metric)))
        ev[[rid]][[mname]] <- metrics[[m]]@metric(model = model,
                                                  out = output@out[[rid]],
                                                  draw = draws@draws[[rid]])
      else
        ev[[rid]][[mname]] <- metrics[[m]]@metric(model = model,
                                                  out = output@out[[rid]])
    }
  }
  evals <- list()
  evals[[output@method_name]] <- ev
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
#' be used to load one or more of the saved \code{\linkS4class{Evals}} object(s).
#' If multiple indices are provided, these will be combined by index into a
#' new single \code{\linkS4class{Evals}} object.  If multiple methods are provided,
#' a list of \code{\linkS4class{Evals}} objects will be returned.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the \code{\linkS4class{Model}} object's \code{name}
#' @param index a vector of positive integers.
#' @param method_names the \code{name} of one or more \code{\linkS4class{Method}}
#'        objects.
#' @param metric_names (optional) a character vector of which elements of
#'        evals should be loaded. If NULL, then all elements are loaded.
#' @param out_loc only needed if it was used in call to
#' @param simulator.files if NULL, then \code{getOption("simulator.files")}
#'        will be used.
#'        \code{\link{run_method}}.
#' @seealso \code{\link{load_model}} \code{\link{load_draws}}
#'          \code{\link{as.data.frame.Evals}}
load_evals <- function(dir, model_name, index, method_names,
                         metric_names = NULL, out_loc = "out",
                       simulator.files = NULL) {
  md <- get_model_dir_and_file(dir, model_name,
                               simulator.files = simulator.files)
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

save_evals_to_file <- function(out_dir, dir, out_loc, evals) {
  file <- sprintf("%s/r%s_%s_evals.Rdata", out_dir, evals@index,
                  evals@method_name)
  save(evals, file = file)
  catsim("..Evaluated", evals@method_label, "in terms of",
         paste(evals@metric_label, collapse = ", "), fill = TRUE)
  new("EvalsRef", dir = dir, model_name = evals@model_name,
      index = evals@index, method_name = evals@method_name,
      out_loc = out_loc,
      simulator.files = getOption("simulator.files"))
}

#' Reduce an Evals object to a subset of methods and/or metrics
#'
#' If \code{method_names} is \code{NULL}, then subsetting is not done over
#' methods.  Likewise for \code{metric_names}.
#'
#' @param evals an object of class \code{\linkS4class{Evals}} or
#'        \code{listofEvals}.
#' @param method_names a character vector of method names
#' @param metric_names a character vector of metric names
#' @export
subset_evals <- function(evals, method_names = NULL, metric_names = NULL) {
  if ("listofEvals" %in% class(evals)) {
    ll <- lapply(evals, subset_evals, method_names = method_names,
                 metric_names = metric_names)
    class(ll) <- class(evals)
    return(ll)
  }
  if (!is.null(method_names)) {
    ii <- match(method_names, evals@method_name)
    ii <- ii[!is.na(ii)]
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


#' @export
#' @rdname load_evals
#' @param ref an object of class \code{\linkS4class{EvalsRef}}
load_evals_from_ref <- function(ref, metric_names = NULL) {
  return(load_evals(dir = ref@dir, model_name = ref@model_name,
                      index = ref@index,
                      method_names = ref@method_name,
                    metric_names = metric_names,
                      out_loc = ref@out_loc,
                      simulator.files = ref@simulator.files))
}
