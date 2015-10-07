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
#' @export
simulate_parallel <- function(dir, model_name, nsim, index, seeds,
                              socket_names, libraries, save_locally = TRUE) {
  stopifnot(length(index) == length(nsim))
  # This function will be called on each cpu:
  inner_simulate_wrapper <- function(i) {
    model <- load_model(dir, model_name)
    simulate_from_model_single(model, nsim[i], index[i], seeds[[i]])
  }
  if (save_locally) {
    simulate_wrapper <- function(i) {
      # create model's directory on slave if it doesn't yet exist:
      model_dir <- file.path(remove_slash(dir), getOption("simulator.files"),
                             model_name)
      if (!dir.exists(model_dir))
        dir.create(model_dir, recursive = TRUE)
      d <- inner_simulate_wrapper(i)
      # save this draws on slave
      file <- save_draws_to_file(model_dir, index[i], nsim[i],
                                 d$draws, d$rng, d$time[1])
      return(file)
    }
  } else {
    simulate_wrapper <- inner_simulate_wrapper
  }
  cl <- parallel::makePSOCKcluster(names = socket_names, outfile = NULL)
  if (!("simulator" %in% libraries)) libraries <- c("simulator", libraries)

  tryCatch({
    load_libraries_on_cluster(cl, libraries)
    varlist <- c("seeds", "dir", "model_name", "nsim", "index")
    parallel::clusterExport(cl, varlist = varlist, envir = environment())
    out <- parallel::parLapplyLB(cl, seq(length(index)), simulate_wrapper)
  }, finally = {
    # regardless of whether an error occurs, do close cluster.
    message("Shutting down cluster.")
    parallel::stopCluster(cl)})

  if (save_locally) {
    # out is a list of file names on slave where output was saved
    files <- unlist(out)
  } else {
    # out is a list of outputs of simulate_from_model_single
    # we now save these to file (on master)
    md <- get_model_dir_and_file(dir, model_name)
    files <- rep(NA, length(out))
    for (i in seq_along(out)) {
      files[i] <- save_draws_to_file(md$dir, index[i], nsim[i],out[[i]]$draws,
                                     out[[i]]$rng, out[[i]]$time[1])
    }
    invisible(files)
  }
  catsim("..Created", as.character(files), "in parallel.", fill = TRUE)
  if (save_locally) catsim("(saved on slaves)", fill = TRUE)
  invisible(files)
}

