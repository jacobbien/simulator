
#' Run one or more methods on simulated data.
#'
#' Given a \code{\link{Method}} object or list of \code{\link{Method}} objects,
#' this function runs the method(s) on the draws passed through \code{object}.
#' The output of each method is saved to file.
#'
#' This function creates objects of class \code{\link{Output}} and saves each to
#' file (at dir/model_name/<out_loc>/r<index>_<method_name>.Rdata. If parallel
#' is not NULL, then it must be a list containing \code{socket_names}, which can
#' either be a positive integer specifying the number of copies to run on
#' localhost or else a character vector of machine names (e.g.,
#' "mycluster-0-0").  The list \code{parallel} can also contain
#' \code{libraries}, a character vector of R packages that will be needed on the
#' slaves and \code{save_locally}, a logical that indicates whether the files
#' generated should be saved on the slaves (i.e., locally) or on the master.
#'
#' Before running each method on index i, the RNG state is restored to what it
#' was at the end of calling \code{\link{simulate_from_model}} on this index.
#' This is only relevant for randomized methods.  The choice to do this ensures
#' that one will get identical results regardless of the order in which methods
#' and indices are run in.
#'
#' @export
#' @param object an object of class \code{\link{DrawsRef}} (or a list of
#'        such objects) as returned by \code{link{simulate_from_model}}. If
#'        \code{object} is a \code{\link{Simulation}}, then function is applied
#'         to the referenced draws in that simulation and returns the same
#'        \code{Simulation} object but with references added to the new outputs
#'        created.
#' @param methods a list of \code{\link{Method}} objects or a single
#'        \code{\link{Method}} object
#' @param out_loc (optional) a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.
#' @param parallel either \code{NULL} or a list containing \code{socket_names}
#'        and (optionally) \code{libraries} and \code{save_locally}
#'        (see Details for more information)
#' @seealso \code{\link{generate_model}} \code{\link{simulate_from_model}}
#' @examples
#' \dontrun{
#'  }
run_method <- function(object, methods, out_loc = "out", parallel = NULL) {
  if (class(object) == "Simulation")
    draws_ref <- draws(object, reference = TRUE)
  else
    draws_ref <- object
  if (class(draws_ref) == "DrawsRef") draws_ref <- list(draws_ref)
  if (class(draws_ref) == "list") {
    if (all(lapply(draws_ref, class) == "list")) {
      # if this is a list of lists, simply apply this function to each list
      oref <- lapply(draws_ref, run_method, methods = methods,
                    out_loc = out_loc, parallel = parallel)
      if (class(object) == "Simulation")
        return(invisible(add(object, oref)))
      else
        return(invisible(oref))
    }
  }
  if (class(draws_ref) == "list" & length(draws_ref) > 1) {
    str <- "Use a list of nested lists for draws_ref from multiple %s."
    if (length(unique(lapply(draws_ref, function(dref) dref@model_name))) > 1)
      stop(sprintf(str, "models"))
    if (length(unique(lapply(draws_ref, function(dref) dref@dir))) > 1)
      stop(sprintf(str, "dir"))
    sf <- lapply(draws_ref, function(dref) dref@simulator.files)
    if (length(unique(sf)) > 1)
      stop(sprintf(str, "simulator.files"))
  }
  if (draws_ref[[1]]@simulator.files != getOption("simulator.files"))
    stop(sprintf("draws_ref@%s must match getOption(\"%s\")",
                 "simulator.files", "simulator.files"))
  if (class(methods) == "list") {
    stopifnot(all(unlist(lapply(methods, function(m) class(m) == "Method"))))
  } else {
    stopifnot(class(methods) == "Method")
    methods <- list(methods)
  }
  # load model
  dir <- draws_ref[[1]]@dir
  model_name <- draws_ref[[1]]@model_name
  index <- unlist(lapply(draws_ref, function(dref) dref@index))
  md <- get_model_dir_and_file(dir, model_name)
  model <- load_model(dir, model_name, more_info = FALSE)
  # prepare output directory
  out_dir <- file.path(md$dir, remove_slash(out_loc))
  if (!file.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # now run methods on each index
  index <- sort(index)
  nmethods <- length(methods)
  orefs <- list()
  if (is.null(parallel) || nmethods * length(index) == 1) {
    # run sequentially
    ii <- 1
    for (i in seq(length(index))) {
      draws_list <- load_draws(dir, model_name, index[i], more_info = TRUE)
      # get state of RNG after i-th simulation
      seed <- draws_list$rng$rng_end_seed
      for (m in seq(nmethods)) {
        .Random.seed <<- seed # run each method as if it had been run directly
        # after calling simulate_from_model for this index alone
        # this makes it so the order in which methods and indices are run will
        # not change things (note: only relevant for methods that require RNG)
        out_list <- run_method_single(methods[[m]], model, draws_list$draws)
        orefs[[ii]] <- save_output_to_file(out_dir, dir, out_loc,
                                               out_list$output, out_list$info)
        ii <- ii + 1
      }
    }
  } else {
    # run in parallel
    check_parallel_list(parallel)
    if (is.null(parallel$save_locally)) parallel$save_locally <- FALSE
    orefs <- run_method_parallel(methods,  model, dir, model_name,
                                     index, out_dir, out_loc,
                                     socket_names = parallel$socket_names,
                                     libraries = parallel$libraries,
                                     save_locally = parallel$save_locally)
  }
  if (class(object) == "Simulation")
    return(invisible(add(object, orefs)))
  invisible(orefs)
}

#' Run one or more methods on simulated data.
#'
#' This is an internal function.  Users should call the wrapper function.
#' \code{\link{run_method}}. Here "single" refers to a single index-method
#' pair.
#'
#' @param method a \code{\link{Method}} object
#' @param model a \code{\link{Model}} object
#' @param draws a \code{\link{Draws}} object generated by \code{model}
run_method_single <- function(method, model, draws) {
  stopifnot(length(draws@index) == 1)
  out <- list()
  for (rid in names(draws@draws)) {
    out[[rid]] <- list()
    time <- system.time({temp <- method@method(model, draws@draws[[rid]])})
    if (class(temp) != "list") temp <- list(out = temp)
    out[[rid]] <- temp
    out[[rid]]$time <- time
  }
  output <- new("Output",
                model_name = model@name,
                index = draws@index,
                method_name = method@name,
                method_label = method@label,
                out = out)
  info <- list(method = method, date_generated = date())
  list(output = output, info = info)
}

save_output_to_file <- function(out_dir, dir, out_loc, output, info) {
  stopifnot(length(output@index) == 1)
  file <- sprintf("%s/r%s_%s.Rdata", out_dir, output@index, output@method_name)
  save(output, info, file = file)
  avg_time <- mean(unlist(lapply(output@out, function(o) o$time[1])))
  catsim(sprintf("..Performed %s in %s seconds (on average over %s sims)",
                 output@method_label, round(avg_time, 2), length(output@out)),
                 fill = TRUE)
  new("OutputRef", dir = dir, model_name = output@model_name,
      index = output@index, method_name = output@method_name,
      out_loc = out_loc,
      simulator.files = getOption("simulator.files"))
}

#' Load one or more output objects from file.
#'
#' After \code{\link{run_method}} has been called, this function can
#' be used to load one or more of the saved \code{\link{Output}} object(s).
#' If multiple indices are provided, these will be combined
#' into a new single \code{\link{Output}} object.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the \code{\link{Model}} object's \code{name}
#' @param index a vector of positive integers.
#' @param method_name the \code{\link{Method}} object's \code{name}
#' @param out_names a character vector of which elements of output should be
#'        loaded. If NULL, then all elements are loaded.
#' @param out_loc only needed if it was used in call to
#'        \code{\link{run_method}}.
#' @param simulator.files if NULL, then \code{getOption("simulator.files")}
#'        will be used.
#' @seealso \code{\link{run_method}} \code{\link{load_model}} \code{\link{load_draws}}
#' @examples
#' \dontrun{
#' }
load_outputs <- function(dir, model_name, index, method_name, out_names = NULL,
                         out_loc = "out", simulator.files = NULL) {
  if (is.null(simulator.files)) simulator.files <- getOption("simulator.files")
  md <- get_model_dir_and_file(dir, model_name, simulator.files = simulator.files)
  index <- sort(unique(index))
  out_dir <- file.path(md$dir, remove_slash(out_loc))
  output_files <- sprintf("%s/r%s_%s.Rdata", out_dir, index, method_name)
  if (length(index) == 1) {
    tryCatch(load(output_files),
             warning=function(w)
               stop(sprintf("Could not find output file at %s.",
                            output_files)))
    if (!is.null(out_names))
      output <- subset_output(output, out_names)
    return(output)
  }
  newout <- list()
  for (i in seq_along(index)) {
    tryCatch(load(output_files[i]),
             warning=function(w)
               stop(sprintf("Could not find output file at %s.",
                            output_files[i])))
    if (!is.null(out_names)) {
      output <- subset_output(output, out_names)
    }
    newout <- c(newout, output@out)
  }
  draws <- new("Output", model_name = model_name, index = index,
               method_name = method_name, method_label = output@method_label,
               out = newout)
  return(draws)
}


#' @export
#' @rdname load_outputs
#' @param ref an object of class \code{\link{OutputRef}}
load_outputs_from_ref <- function(ref, out_names = NULL) {
  return(load_outputs(dir = ref@dir, model_name = ref@model_name,
                      index = ref@index,
                      method_name = ref@method_name,
                      out_names = out_names,
                      out_loc = ref@out_loc,
                      simulator.files = ref@simulator.files))
}


subset_output <- function(output, out_names) {
  for (j in seq(length(output@out))) {
    if (!(all(out_names %in% names(output@out[[j]]))))
      stop("Element ", names(output@out)[j], " does not match out_names.")
    output@out[[j]] <- output@out[[j]][out_names]
  }
  output
}
