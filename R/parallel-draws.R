#' Simulate from a model in parallel.
#'
#' This is an internal function. Draws are done in chunks labeled
#' by indices and of size determined by nsim. Users should call the wrapper
#' function \code{\link{simulate_from_model}}.
#'
#' @param dir the directory where Model object was saved (by
#'        \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param nsim number of simulations to be conducted on each chunk.  Vector of
#'        same length as \code{index}
#' @param index a vector of positive integer indices.  Allows simulations to be
#'        carried out in chunks.  Each chunk gets a separate RNG stream,
#'        meaning that the results will be identical whether we run these in
#'        parallel or sequentially.
#' @param seeds a list of \code{length(index)} L'Ecuyer-CMRG seed vectors.
#'        Each should be from a separate stream.  In particular, starting from
#'        the seed used to generate the model object, seeds[i] should be the
#'        result of calling \code{\link{parallel::nextRNGStream}} index[i]
#'        times.
#' @param socket_names (quoting from \code{\link{parallel::makePSOCKcluster}}
#'        "either a character vector of host names on which to run the worker
#'        copies of R, or a positive integer (in which case that number of
#'        copies is run on localhost)."
#' @param libraries character vector of R packages that will be needed on the
#'        slaves.
#' @param save_locally if TRUE, then files will be saved on slaves.  If FALSE,
#'        they will be saved on master.
simulate_parallel <- function(dir, model_name, nsim, index, seeds,
                              socket_names, libraries, save_locally = TRUE) {
  stopifnot(length(index) == length(nsim))
  # this is function to be run on each slave:
  function_to_do <- function(dir, model_name, nsim, index, seed) {
    model <- load_model(dir, model_name)
    simulate_from_model_single(model, nsim, index, seed)
  }
  # make list where params1[[i]] are the arguments to pass to
  # function_to_do for i-th index
  params1 <- lapply(seq(length(index)),
                    function(i) list(dir = dir, model_name = model_name,
                                     nsim = nsim[i], index = index[i],
                                     seed = seeds[[i]]))
  # this is function to use when saving info in d to file (whether it be on
  # slave or master):
  save_to_file <- function(draws, rng, time, out_dir, index, nsim) {
    save_draws_to_file(out_dir, index, nsim, draws, rng, time[1])
  }
  # parameters to be passed to save_to_file other than d
  md <- get_model_dir_and_file(dir, model_name)
  params2 <- lapply(seq(length(index)),
                    function(i) list(out_dir = md$dir,
                                     index = index[i],
                                     nsim = nsim[i]))
  do_in_parallel(function_to_do, params1,
                 save_to_file, params2,
                 socket_names = socket_names,
                 libraries = libraries,
                 save_locally = save_locally)
}
