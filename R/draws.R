#' @include draws-class.R
NULL

#' Simulate from a model.
#'
#' Given a \code{\link{Model}} object, this function calls the model's
#' \code{simulate} function on its \code{params}.  It repeats this \code{nsim}
#' times.  For example, when simulating regression with a fixed design, this
#' function would generate \code{nsim} response vectors \code{y}.
#'
#' This function creates objects of class \code{\link{Draws}} and saves each to
#' file (at dir/model_name/r*.Rdata, where * is an index).  If parallel is not
#' NULL, then it must be a list containing \code{socket_names}, which can either
#' be a positive integer specifying the number of copies to run on localhost or
#' else a character vector of machine names (e.g., "mycluster-0-0").  The list
#' \code{parallel} can can also contain \code{libraries}, a character vector of
#' R packages that will be needed on the slaves.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the \code{\link{Model}} object's \code{name}
#' @param nsim number of simulations to be conducted.  If a scalar, then
#'        value repeated for each index.  Otherwise can be a vector of length
#'        \code{length(index)}
#' @param index a vector of positive integer indices.  Allows simulations to be
#'        carried out in chunks.  Each chunk gets a separate RNG stream,
#'        meaning that the results will be identical whether we run these in
#'        parallel or sequentially.
#' @param parallel either \code{NULL} or a list containing \code{socket_names}
#'        and (optionally) \code{libraries}  (see Details for more information)
#' @seealso \code{\link{load_draws}} \code{\link{generate_model}} \code{\link{run_method}}
#' @examples
#' \dontrun{
#'  generate_model(make_my_model, dir = ".")
#'  simulate_from_model(dir = ".", "fm", nsim = 50, index = 1)
#'  simulate_from_model(dir = ".", "fm", nsim = 50, index = 2:3,
#'                      parallel = list(cpus = 4))
#'  }
simulate_from_model <- function(dir = ".", model_name, nsim,
                                index = 1, parallel = NULL) {
  stopifnot(index == round(index), index > 0)
  stopifnot(nsim == round(nsim), nsim > 0)
  if (length(nsim) == 1) {
    nsim <- rep(nsim, length(index))
  } else {
    stopifnot(length(nsim) == length(index))
    o <- order(index)
    index <- index[o]; nsim <- nsim[o]
  }
  md <- get_model_dir_and_file(dir, model_name)
  # generate L'Ecuyer seeds based on model's seed
  m <- load_model(dir, model_name, more_info = TRUE)
  model_seed <- m$rng$rng_seed # seed used to generate m$model
  seeds <- get_seeds_for_draws(model_seed, index)
  files <- rep(NA, length(index))
  if (is.null(parallel) || length(index) == 1) {
    # simulate sequentially
    for (i in seq(length(index))) {
       d <- simulate_from_model_single(m$model, nsim = nsim[i],
                                       index = index[i], seed = seeds[[i]])
       files[i] <- save_draws_to_file(md$dir, index[i], nsim[i], d$draws,
                                      d$rng, d$time[1])
    }
  } else {
    check_parallel_list(parallel)
    if (is.null(parallel$save_locally)) parallel$save_locally <- FALSE
    files <- simulate_parallel(dir, model_name, nsim, index, seeds = seeds,
                               socket_names = parallel$socket_names,
                               libraries = parallel$libraries,
                               save_locally = parallel$save_locally)
  }
  invisible(files)
}

save_draws_to_file <- function(model_dir, index, nsim, draws, rng, time) {
  file <- sprintf("%s/r%s.Rdata", model_dir, index)
  save(draws, rng, file = file)
  catsim(sprintf("..Simulated %s draws in %s sec and saved in %s", nsim,
                 round(time, 2), file), fill = TRUE)
  file
}

get_seeds_for_draws <- function(model_seed, index) {
  RNGkind("L'Ecuyer-CMRG")
  # index gives which stream relative to stream used to generate model:
  seeds <- list(model_seed)
  for (i in seq(2, 1 + max(index)))
    seeds[[i]] <- parallel::nextRNGStream(seeds[[i - 1]])
  seeds <- seeds[-1]
  seeds <- seeds[index] # now use these seeds[[i]] for index[i]'s chunk:
  seeds
}

#' Simulate from a model.
#'
#' This is an internal function.  Users should call the wrapper function
#' \code{\link{simulate_from_model}}.
#'
#' @param model a Model object
#' @param nsim number of simulations to be conducted.
#' @param index a positive integer index.
#' @param seed this is the 7 digit seed used by L'Ecuyer RNG
simulate_from_model_single <- function(model, nsim, index, seed) {
  stopifnot(length(nsim) == 1, length(index) == 1)
  RNGkind("L'Ecuyer-CMRG")
  .Random.seed <<- seed
  time <- system.time({sims1 <- model@simulate(model@params, nsim)})
  rng <- list(rng_seed = seed, rng_end_seed = .Random.seed)
  sims <- list()
  for (i in seq(nsim))
    sims[[sprintf("r%s.%s", index, i)]] <- sims1[[i]]
  rm(sims1)
  # create object of class Draws
  draws <- new("Draws", name = model@name,
               label = sprintf("(Block %s:) %s draws from %s", index, nsim,
                               model@label),
               draws = sims,
               index = as.integer(index))
  validObject(draws)
  return(list(draws = draws, rng = rng, time = time))
}


#' Load one or more draws objects from file.
#'
#' After \code{\link{simulate_from_model}} has been called, this function can
#' be used to load one or more of the saved \code{\link{Draws}} object(s)
#' (along with RNG information).  If multiple indices are provided, these will be combined
#' into a new single \code{\link{Draws}} object.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param index a vector of positive integers.
#' @param more_info if TRUE, then returns additional information such as
#'        state of RNG after calling \code{\link{generate_model}}
#' @seealso \code{\link{simulate_from_model}} \code{\link{load_model}}
#' @examples
#' \dontrun{
#' # see example ?generate_model for make_my_model definition
#'  generate_model(make_my_model, dir = ".")
#'  simulate_from_model(model_name = "fm", nsim = 50, index = 1)
#'  simulate_from_model(model_name = "fm", nsim = 50, index = 2)
#'  load_draws(model_name = "fm", 1:2) # makes Draws object with 100 entries
#' }
load_draws <- function(dir, model_name, index, more_info = FALSE) {
  md <- get_model_dir_and_file(dir, model_name)
  index <- sort(unique(index))
  draws_files <- sprintf("%s/r%s.Rdata", md$dir, index)
  if (length(index) == 1) {
    tryCatch(load(draws_files),
         warning=function(w) stop(sprintf("Could not find draws file at %s.",
                                          draws_files)))
    if (more_info) return(list(draws = draws, rng = rng))
    else return(draws)
  }
  newdraws <- rnglist <- list()
  for (i in seq_along(index)) {
    tryCatch(load(draws_files[i]),
          warning=function(w) stop(sprintf("Could not find draws file at %s.",
                                           draws_files[i])))
    newdraws <- c(newdraws, draws@draws)
    rnglist[[i]] <- rng[[1]]
  }
  indices <- paste(index, collapse = ", ")
  nsim <- length(newdraws)
  model <- load_model(dir, model_name, more_info = FALSE)
  draws <- new("Draws", name = model_name,
               label = sprintf("(Blocks %s:) %s draws from %s", indices, nsim,
                               model@label), index = index, draws = newdraws)
  if (more_info)
    return(list(draws = draws, rng = rnglist))
  else
    return(draws)
}
