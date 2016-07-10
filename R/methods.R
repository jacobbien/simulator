#' @include extended-method-class.R
NULL

#' Run one or more methods on simulated data.
#'
#' Given a \code{\linkS4class{Method}} object or list of \code{\linkS4class{Method}} objects,
#' this function runs the method(s) on the draws passed through \code{object}.
#' The output of each method is saved to file.
#'
#' This function creates objects of class \code{\linkS4class{Output}} and saves each to
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
#' and indices are run in.  When \code{\linkS4class{ExtendedMethod}} objects are
#' passed, these are run after all \code{Method} objects have been run.  This is
#' because each \code{ExtendedMethod} object depends on the output of its base
#' method.  Furthermore, before an \code{ExtendedMethod} is called, the RNG
#' state is restored to what it was after the base method had been called.
#'
#' @export
#' @param object an object of class \code{\linkS4class{DrawsRef}} (or a list of
#'        such objects) as returned by \code{link{simulate_from_model}}. If
#'        \code{object} is a \code{\linkS4class{Simulation}}, then function is applied
#'         to the referenced draws in that simulation and returns the same
#'        \code{Simulation} object but with references added to the new outputs
#'        created.
#' @param methods a list of \code{\linkS4class{Method}} and/or
#'        \code{\linkS4class{ExtendedMethod}} objects or a single \code{\linkS4class{Method}}
#'        or object \code{\linkS4class{ExtendedMethod}}
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
#'  # suppose previously we had run the following:
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model, n = 20) %>%
#'    simulate_from_model(nsim = 50, index = 1:3)
#'  # then we could add
#'  sim <- run_method(sim, my_example_method)
#'  }
run_method <- function(object, methods, out_loc = "out", parallel = NULL) {
  if (class(methods) == "list") {
    classes <- unlist(lapply(methods, class))
    stopifnot(classes %in% c("Method", "ExtendedMethod"))
    if (length(unique(classes)) == 2) {
      if (class(object) != "Simulation")
        stop("When both Method and ExtendedMethod objects passed to \"methods\"",
             " object must be of class \"Simulation\" for now.")
        # run all the methods first followed by all the extended methods
        object <- run_method(object, methods = methods[classes == "Method"],
                             out_loc = out_loc, parallel = parallel)
        object <- run_method(object,
                             methods = methods[classes == "ExtendedMethod"],
                             out_loc = out_loc, parallel = parallel)
        return(invisible(object))
    }
  } else {
    stopifnot(class(methods) %in% c("Method", "ExtendedMethod"))
    methods <- list(methods)
  }
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
      for (m in seq(nmethods)) {
        if (class(methods[[m]]) == "Method") {
          out_list <- run_method_single(methods[[m]], model, draws_list)
        } else {
          # ExtendedMethod
          tryCatch({
            base_out_list <- load_outputs(dir, model_name, index[i],
                                          methods[[m]]@base_method@name,
                                          more_info = TRUE)},
            error = function(e) stop("Could not find output of method \"",
                                     methods[[m]]@base_method@label,
                                     "\" for index ", index[i], ".",
                                      call. = FALSE))

          out_list <- run_extendedmethod_single(methods[[m]], model,
                                                draws_list$draws,
                                                base_out_list)
        }
        orefs[[ii]] <- save_output_to_file(out_dir, dir, out_loc,
                                               out_list$output, out_list$info)
        ii <- ii + 1
      }
    }
  } else {
    # run in parallel
    check_parallel_list(parallel)
    if (is.null(parallel$save_locally)) parallel$save_locally <- FALSE
    if (all(lapply(methods, class) == "Method"))
      orefs <- run_method_parallel(methods, dir, model_name,
                                   index, out_dir, out_loc,
                                   socket_names = parallel$socket_names,
                                   libraries = parallel$libraries,
                                   save_locally = parallel$save_locally)
    else if (all(lapply(methods, class) == "ExtendedMethod")) {
      # extended methods
      orefs <- run_extmethod_parallel(methods, dir, model_name,
                                   index, out_dir, out_loc,
                                   socket_names = parallel$socket_names,
                                   libraries = parallel$libraries,
                                   save_locally = parallel$save_locally)
      } else stop("This should never happen!")
    }
  if (class(object) == "Simulation")
    return(invisible(add(object, orefs)))
  invisible(orefs)
}

#' Run a single method on a single index of simulated data.
#'
#' This is an internal function.  Users should call the wrapper function.
#' \code{\link{run_method}}. Here "single" refers to a single index-method
#' pair.
#'
#' @param method a \code{\linkS4class{Method}} object
#' @param model a \code{\linkS4class{Model}} object
#' @param draws_list the result of loading a \code{\linkS4class{Draws}} object with
#'        \code{more_info = TRUE} so that it includes RNG endstate.
run_method_single <- function(method, model, draws_list) {
  .Random.seed <<- draws_list$rng$rng_end_seed
  # run this method as if it had been run directly
  # after calling simulate_from_model for this index alone
  # this makes it so the order in which methods and indices are run will
  # not change things (note: only relevant for methods that require RNG)
  stopifnot(length(draws_list$draws@index) == 1)
  out <- list()
  settings_args <- intersect(names(formals(method@method)),
                             names(method@settings))
  for (rid in names(draws_list$draws@draws)) {
    out[[rid]] <- list()
    arguments <- c(list(model = model, draw = draws_list$draws@draws[[rid]]),
                   method@settings[settings_args])
    time <- system.time({temp <- do.call(method@method, arguments)})
    if (!is.list(temp)) temp <- list(out = temp)
    out[[rid]] <- temp
    out[[rid]]$time <- time
  }
  output <- new("Output",
                model_name = model@name,
                index = draws_list$draws@index,
                method_name = method@name,
                method_label = method@label,
                out = out)
  # record seed state at start and end of calling this method on this index
  rng <- list(rng_seed = draws_list$rng$rng_end_seed,
              rng_end_seed = .Random.seed)
  info <- list(method = method, date_generated = date(), rng = rng)
  list(output = output, info = info)
}

#' Run a single extended method on a single index of simulated data.
#'
#' This is an internal function.  Users should call the wrapper function.
#' \code{\link{run_method}}. Here "single" refers to a single
#' index-ExtendedMethod
#' pair.
#'
#' @param extmethod a \code{\linkS4class{ExtendedMethod}} object
#' @param model a \code{\linkS4class{Model}} object
#' @param draws a \code{\linkS4class{Draws}} object generated by \code{model}
#' @param base_output_list the result of loading a \code{\linkS4class{Output}} object with
#'        \code{more_info = TRUE} so that it includes RNG endstate.
run_extendedmethod_single <- function(extmethod, model, draws,
                                      base_output_list) {
  .Random.seed <<- base_output_list$rng$rng_end_seed
  stopifnot(length(draws@index) == 1)
  out <- list()
  for (rid in names(draws@draws)) {
    out[[rid]] <- list()
    time <- system.time({
      temp <- extmethod@extended_method(model = model,
                                       draw = draws@draws[[rid]],
                                       out = base_output_list$output@out[[rid]],
                                       base_method = extmethod@base_method)
      })
    if (!is.list(temp)) temp <- list(out = temp)
    out[[rid]] <- temp
    # add together the times from running base_method and the extension
    out[[rid]]$time <- time + base_output_list$output@out[[rid]]$time
  }
  output <- new("Output",
                model_name = model@name,
                index = draws@index,
                method_name = extmethod@name,
                method_label = extmethod@label,
                out = out)
  # record seed state at start and end of calling this method on this index
  rng <- list(rng_seed = base_output_list$rng$rng_end_seed,
              rng_end_seed = .Random.seed)
  info <- list(method = extmethod, date_generated = date(), rng = rng)
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
#' be used to load one or more of the saved \code{\linkS4class{Output}} object(s).
#' If multiple indices are provided, these will be combined
#' into a new single \code{\linkS4class{Output}} object.
#' If simulation object is available, it is easier to use the function
#' \code{\link{output}} to load it.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the \code{\linkS4class{Model}} object's \code{name}
#' @param index a vector of positive integers.
#' @param method_name the \code{\linkS4class{Method}} object's \code{name}
#' @param out_names a character vector of which elements of output should be
#'        loaded. If NULL, then all elements are loaded.
#' @param out_loc only needed if it was used in call to
#'        \code{\link{run_method}}.
#' @param more_info if TRUE, then returns additional information such as
#'        state of RNG after calling \code{\link{simulate_from_model}}
#' @param simulator.files if NULL, then \code{getOption("simulator.files")}
#'        will be used.
#' @seealso \code{\link{run_method}} \code{\link{output}}
load_outputs <- function(dir, model_name, index, method_name, out_names = NULL,
                         out_loc = "out", more_info = FALSE,
                         simulator.files = NULL) {
  if (is.null(simulator.files)) simulator.files <- getOption("simulator.files")
  md <- get_model_dir_and_file(dir, model_name,
                               simulator.files = simulator.files)
  index <- sort(unique(index))
  out_dir <- file.path(md$dir, remove_slash(out_loc))
  output_files <- sprintf("%s/r%s_%s.Rdata", out_dir, index, method_name)
  if (length(index) == 1) {
    env <- new.env()
    tryCatch(load(output_files, envir = env),
             warning=function(w)
               stop(sprintf("Could not find output file at %s.",
                            output_files)))
    output <- env$output
    if (!is.null(out_names))
      output <- subset_output(output, out_names)
    if (more_info)
      return(list(output = output, rng = env$info$rng))
    else
      return(output)
  }
  newout <- list()
  for (i in seq_along(index)) {
    env <- new.env()
    tryCatch(load(output_files[i], envir = env),
             warning=function(w)
               stop(sprintf("Could not find output file at %s.",
                            output_files[i])))
    output <- env$output
    if (!is.null(out_names)) {
      output <- subset_output(output, out_names)
    }
    newout <- c(newout, output@out)
  }
  output <- new("Output", model_name = model_name, index = index,
               method_name = method_name, method_label = output@method_label,
               out = newout)
  if (more_info)
    return(list(output = output, rng = env$info$rng))
  else
    return(output)
}


#' @rdname load_outputs
#' @param ref an object of class \code{\linkS4class{OutputRef}}
#' @keywords internal
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
