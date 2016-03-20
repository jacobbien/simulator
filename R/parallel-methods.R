#' Run one or more methods on simulated data.
#'
#' This is an internal function. Jobs are run in parallel both by method
#' and by index.  Users should call the wrapper function
#' \code{\link{run_method}}.
#' @param my_methods list of methods to be run in parallel
#' @param dir the directory where Model object was saved (by
#'        \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param index a vector of positive integer indices.  Allows simulations to be
#'        carried out in chunks.  Each chunk gets a separate RNG stream,
#'        meaning that the results will be identical whether we run these in
#'        parallel or sequentially.
#' @param out_dir full directory to where method outputs are stored.
#' @param out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.
#' @param socket_names (quoting from \code{\link[parallel]{makePSOCKcluster}}
#'        "either a character vector of host names on which to run the worker
#'        copies of R, or a positive integer (in which case that number of
#'        copies is run on localhost)."
#' @param libraries character vector of R packages that will be needed on the
#'        slaves.
#' @param save_locally if TRUE, then files will be saved on slaves.  If FALSE,
#'        they will be saved on master.
#' @keywords internal
run_method_parallel <- function(my_methods, dir, model_name, index,
                                out_dir, out_loc, socket_names, libraries,
                                save_locally = TRUE) {
  function_to_do <- function(dir, model_name, index, method) {
    model <- load_model(dir, model_name)
    draws_list <- load_draws(dir, model_name, index, more_info = TRUE)
    out_list <- run_method_single(method, model, draws_list)
    return(out_list)
  }
  num_methods <- length(my_methods)
  num_index <- length(index)
  # index over all method-index pairs:
  ii <- cbind(rep(seq(num_methods), each = num_index),
              rep(seq(num_index), times = num_methods))
  njobs <- nrow(ii)
  # make list where params1[[i]] are the arguments to pass to
  # function_to_do for i-th job.  Here i indexes method-index pairs
  params1 <- lapply(seq(njobs),
                    function(i) list(dir = dir, model_name = model_name,
                                     index = index[ii[i, 2]],
                                     method = my_methods[[ii[i, 1]]]))
  # this is function to use when saving info in out_list to file
  # (whether it be on slave or master):
  save_to_file <- function(output, info, out_dir, dir, out_loc) {
    save_output_to_file(out_dir, dir, out_loc, output, info)
  }
  # parameters to be passed to save_to_file other than out_list
  params2 <- lapply(seq(njobs), function(i) list(out_dir = out_dir,
                                                 dir = dir,
                                                 out_loc = out_loc))
  do_in_parallel(function_to_do, params1,
                 save_to_file, params2,
                 socket_names = socket_names,
                 libraries = libraries,
                 save_locally = save_locally)
}

#' Run one or more extended methods on simulated data.
#'
#' This is an internal function. Jobs are run in parallel both by method
#' and by index.  Users should call the wrapper function
#' \code{\link{run_method}}.
#' @param my_extmethods list of extended methods to be run in parallel
#' @param dir the directory where Model object was saved (by
#'        \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param index a vector of positive integer indices.  Allows simulations to be
#'        carried out in chunks.  Each chunk gets a separate RNG stream,
#'        meaning that the results will be identical whether we run these in
#'        parallel or sequentially.
#' @param out_dir full directory to where method outputs are stored.
#' @param out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.
#' @param socket_names (quoting from \code{\link[parallel]{makePSOCKcluster}})
#'        "either a character vector of host names on which to run the worker
#'        copies of R, or a positive integer (in which case that number of
#'        copies is run on localhost)."
#' @param libraries character vector of R packages that will be needed on the
#'        slaves.
#' @param save_locally if TRUE, then files will be saved on slaves.  If FALSE,
#'        they will be saved on master.
#' @keywords internal
run_extmethod_parallel <- function(my_extmethods, dir, model_name, index,
                                out_dir, out_loc, socket_names, libraries,
                                save_locally = TRUE) {
  function_to_do <- function(dir, model_name, index, extmethod) {
    model <- load_model(dir, model_name)
    draws_list <- load_draws(dir, model_name, index, more_info = TRUE)
    tryCatch({
      base_out_list <- load_outputs(dir, model_name, index,
                                    extmethod@base_method@name,
                                    more_info = TRUE)},
      error = function(e) stop("Could not find output of method \"",
                               extmethod@base_method@label,
                               "\" for index ", index, ".", call. = FALSE))
    out_list <- run_extendedmethod_single(extmethod, model, draws_list$draws,
                                          base_out_list)
    return(out_list)
  }
  num_methods <- length(my_extmethods)
  num_index <- length(index)
  # index over all method-index pairs:
  ii <- cbind(rep(seq(num_methods), each = num_index),
              rep(seq(num_index), times = num_methods))
  njobs <- nrow(ii)
  # make list where params1[[i]] are the arguments to pass to
  # function_to_do for i-th job.  Here i indexes method-index pairs
  params1 <- lapply(seq(njobs),
                    function(i) list(dir = dir, model_name = model_name,
                                     index = index[ii[i, 2]],
                                     extmethod = my_extmethods[[ii[i, 1]]]))
  # this is function to use when saving info in out_list to file
  # (whether it be on slave or master):
  save_to_file <- function(output, info, out_dir, dir, out_loc) {
    save_output_to_file(out_dir, dir, out_loc, output, info)
  }
  # parameters to be passed to save_to_file other than out_list
  params2 <- lapply(seq(njobs), function(i) list(out_dir = out_dir,
                                                 dir = dir,
                                                 out_loc = out_loc))
  do_in_parallel(function_to_do, params1,
                 save_to_file, params2,
                 socket_names = socket_names,
                 libraries = libraries,
                 save_locally = save_locally)
}
