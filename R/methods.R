
#' Run one or more methods on simulated data.
#'
#' Given a \code{\link{Method}} object or list of \code{\link{Method}} objects,
#' this function runs the method(s) on the draws specified by \code{model_name}
#' and \code{index}.  The output of each method is saved to file.
#'
#' This function creates objects of class \code{\link{Object}} and saves each to
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
#' @param my_methods a list of \code{\link{Method}} objects or a single
#'        \code{\link{Method}} object
#' @param dir the directory where \code{\link{Model}} object was saved (by
#'        \code{\link{generate_model}})
#' @param model_name the \code{\link{Model}} object's \code{name} attribute
#' @param index the index of a computed \code{\link{Draws}} object.  Can
#'        alternately be a vector of such indices.
#' @param out_loc (optional) a character vector that gives location (relative
#'        to model's path) that method outputs are stored.  This can be useful
#'        for staying organized when multiple simulations are based on the same
#'        Model and Draws objects.
#' @param parallel either \code{NULL} or a list containing \code{socket_names}
#'        and (optionally) \code{libraries} and \code{save_locally}
#'        (see Details for more information)
#' @seealso \code{\link{generate_model}} \code{\link{simulate_from_model}}
#' @examples
#' \dontrun{
#'  run_method(list(my_method, their_method), model_name = "fm", index = 1:2)
#'  run_method(list(my_method, their_method), model_name = "fm", index = 1:2,
#'             parallel = list(socket_names = 3))
#'  }
run_method <- function(my_methods, dir = ".", model_name, index,
                       out_loc = "out", parallel = NULL) {
  # make sure my_methods is a list of Method objects
  if (class(my_methods) == "list") {
    stopifnot(all(unlist(lapply(my_methods, function(m) class(m) == "Method"))))
  } else {
    stopifnot(class(my_methods) == "Method")
    my_methods <- list(my_methods)
  }
  # load model
  md <- get_model_dir_and_file(dir, model_name)
  model <- load_model(dir, model_name, more_info = FALSE)
  # prepare output directory
  out_dir <- file.path(md$dir, remove_slash(out_loc))
  if (!file.exists(out_dir)) dir.create(out_dir)
  # now run methods on each index
  index <- sort(index)
  nmethods <- length(my_methods)
  out_files <- list()
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
        out_list <- run_method_single(my_methods[[m]], model, draws_list$draws)
        out_files[[ii]] <- save_output_to_file(out_dir, out_list$output,
                                               out_list$info)
        ii <- ii + 1
      }
    }
  } else {
    # run in parallel
    check_parallel_list(parallel)
    if (is.null(parallel$save_locally)) parallel$save_locally <- FALSE
    out_files <- run_method_parallel(my_methods,  model, dir, model_name,
                                     index, out_dir,
                                     socket_names = parallel$socket_names,
                                     libraries = parallel$libraries,
                                     save_locally = parallel$save_locally)
  }
  invisible(out_files)
}

#' Run one or more methods on simulated data.
#'
#' This is an internal function.  Users should call the wrapper function.
#' \code{\link{run_method}}. Here "single" refers to a single index
#' and a single method.
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

save_output_to_file <- function(out_dir, output, info, avg_time) {
  stopifnot(length(output@index) == 1)
  file <- sprintf("%s/r%s_%s.Rdata", out_dir, output@index, output@method_name)
  save(output, info, file = file)
  avg_time <- mean(unlist(lapply(output@out, function(o) o$time[1])))
  catsim(sprintf("..Performed %s in %s seconds (on average over %s sims)",
                 output@method_label, round(avg_time, 2), length(output@out)),
                 fill = TRUE)
  file
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
#' @param out_loc Only needed if it was used in call to
#'        \code{\link{run_method}}.
#' @seealso \code{\link{run_method}} \code{\link{load_model}} \code{\link{load_draws}}
#' @examples
#' \dontrun{
#' }
load_outputs <- function(dir, model_name, index, method_name,
                         out_names = NULL, out_loc = "out") {
  md <- get_model_dir_and_file(dir, model_name)
  if (any(table(index) > 1)) stop("index cannot have repeats.")
  index <- sort(index)
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



subset_output <- function(output, out_names) {
  for (j in seq(length(output@out))) {
    if (!(all(out_names %in% names(output@out[[j]]))))
      stop("Element ", names(output@out)[j], " does not match out_names.")
    output@out[[j]] <- output@out[[j]][out_names]
  }
  output
}
