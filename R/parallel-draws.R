#' Simulate from a model in parallel.
#'
#' This is an internal function. Draws are done in chunks labeled
#' by indices and of size determined by nsim. Users should call the wrapper
#' function \code{\link{simulate_from_model}}.
#'
#' @param model_ref object of class \code{\linkS4class{ModelRef}}
#' @param nsim number of simulations to be conducted on each chunk.  Vector of
#'        same length as \code{index}
#' @param index a vector of positive integer indices.  Allows simulations to be
#'        carried out in chunks.  Each chunk gets a separate RNG stream,
#'        meaning that the results will be identical whether we run these in
#'        parallel or sequentially.
#' @param seeds a list of \code{length(index)} L'Ecuyer-CMRG seed vectors.
#'        Each should be from a separate stream.  In particular, starting from
#'        the seed used to generate the model object, seeds[i] should be the
#'        result of calling \code{\link[parallel]{nextRNGStream}} index[i]
#'        times.
#' @param socket_names (quoting from \code{\link[parallel]{makePSOCKcluster}}
#'        "either a character vector of host names on which to run the worker
#'        copies of R, or a positive integer (in which case that number of
#'        copies is run on localhost)."
#' @param libraries character vector of R packages that will be needed on the
#'        slaves.
#' @param save_locally if TRUE, then files will be saved on slaves.  If FALSE,
#'        they will be saved on master.
simulate_parallel <- function(model_ref, nsim, index, seeds,
                              socket_names, libraries, save_locally = TRUE) {
  stopifnot(length(index) == length(nsim))
  # this is function to be run on each slave:
  function_to_do <- function(model_ref, nsim, index, seed) {
    model <- load(model_ref)
    simulate_from_model_single(model, nsim, index, seed)
  }
  # make list where params1[[i]] are the arguments to pass to
  # function_to_do for i-th index
  params1 <- lapply(seq(length(index)),
                    function(i) list(model_ref = model_ref,
                                     nsim = nsim[i], index = index[i],
                                     seed = seeds[[i]]))
  # this is function to use when saving info in d to file (whether it be on
  # slave or master):
  save_to_file <- function(draws, rng, time, out_dir, model_ref, index, nsim) {
    save_draws_to_file(out_dir = out_dir, model_ref = model_ref, index = index,
                       nsim = nsim, draws = draws, rng = rng, time = time[1])
  }
  # parameters to be passed to save_to_file other than d
  md <- get_model_dir_and_file(model_ref@dir, model_ref@name)
  params2 <- lapply(seq(length(index)),
                    function(i) list(out_dir = md$dir,
                                     model_ref = model_ref,
                                     index = index[i],
                                     nsim = nsim[i]))
  do_in_parallel(function_to_do = function_to_do,
                 function_params = params1,
                 save_to_file = save_to_file,
                 save_params = params2,
                 socket_names = socket_names,
                 libraries = libraries,
                 save_locally = save_locally)
}
