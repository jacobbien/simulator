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
#' @export
simulate_parallel <- function(dir, model_name, nsim, index, seeds, socket_names,
                              libraries) {
  stopifnot(length(index) == length(nsim))
  # This function will be called on each cpu:
  simulate_wrapper <- function(i) {
    simulate_from_model_single(dir, model_name, nsim[i], index[i], seeds[[i]])
  }
  cl <- parallel::makePSOCKcluster(names = socket_names)
  if (!("simulator" %in% libraries)) libraries <- c("simulator", libraries)

  tryCatch({
    load_libraries_on_cluster(cl, libraries)
    varlist <- c("seeds", "dir", "model_name", "nsim", "index")
    parallel::clusterExport(cl, varlist = varlist, envir = environment())
    files <- parallel::parLapplyLB(cl, seq(length(index)), simulate_wrapper)
  }, finally = {
    # regardless of whether an error occurs, do close cluster.
    message("Shutting down cluster.")
    parallel::stopCluster(cl)})
  files <- unlist(files)
  catsim("..Created", as.character(files), "in parallel.", fill = TRUE)
  invisible(files)
}

