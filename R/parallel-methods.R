#' Run one or more methods on simulated data.
#'
#' This is an internal function. Jobs are run in parallel both by method
#' and by index.  Users should call the wrapper function
#' \code{\link{run_method}}.
#' @param dir the directory where Model object was saved (by
#'        \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param nsim number of simulations to be conducted on each chunk.  Vector of
#'        same length as \code{index}
#' @param index a vector of positive integer indices.  Allows simulations to be
#'        carried out in chunks.  Each chunk gets a separate RNG stream,
#'        meaning that the results will be identical whether we run these in
#'        parallel or sequentially.
#' @param out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.
#' @param out_dir full directory to where method outputs are stored.
#' @param socket_names (quoting from \code{\link{parallel::makePSOCKcluster}}
#'        "either a character vector of host names on which to run the worker
#'        copies of R, or a positive integer (in which case that number of
#'        copies is run on localhost)."
#' @param libraries character vector of R packages that will be needed on the
#'        slaves.
#' @param save_locally if TRUE, then files will be saved on slaves.  If FALSE,
#'        they will be saved on master.
run_method_parallel <- function(my_methods, model, dir, model_name, index,
                                out_dir, out_loc, socket_names, libraries,
                                save_locally = TRUE) {
  function_to_do <- function(dir, model_name, index, method) {
    model <- load_model(dir, model_name)
    draws_list <- load_draws(dir, model_name, index, more_info = TRUE)
    seed <- draws_list$rng$rng_end_seed
    .Random.seed <<- seed
    # run each method as if it had been run directly
    # after calling simulate_from_model for this index alone
    # this makes it so the order in which methods and indices are run will
    # not change things (note: only relevant for methods that require RNG)
    out_list <- run_method_single(method, model, draws_list$draws)
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
