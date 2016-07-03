#' @include draws-class.R
NULL

#' Simulate from a model.
#'
#' Given a reference to a \code{\linkS4class{Model}} object, this function calls the
#' model's \code{simulate} function on its \code{params}.  It repeats this
#' \code{nsim} times.  For example, when simulating regression with a fixed
#' design, this function would generate \code{nsim} response vectors \code{y}.
#'
#' This function creates objects of class \code{\linkS4class{Draws}} and saves each to
#' file (at dir/files/model_name/r<index>.Rdata). Note: while "files" is the
#' default, the name of this directory is from getOption("simulator.files"),
#' which is the value of getOption("simulator.files") when the model was
#' created.
#'
#' If parallel is not NULL, then it must be a list containing
#' \code{socket_names}, which can either be a positive integer specifying the
#' number of copies to run on localhost or else a character vector of machine
#' names (e.g., "mycluster-0-0").  The list \code{parallel} can also contain
#' \code{libraries}, a character vector of R packages that will be needed on the
#' slaves and \code{save_locally}, a logical that indicates whether the files
#' generated should be saved on the slaves (i.e., locally) or on the master.

#'
#' @export
#' @param object an object of class \code{\linkS4class{ModelRef}} as returned by
#'        \code{link{generate_model}}. Or a list of such objects. If
#'        \code{object} is a \code{Simulation}, then function is applied to the
#'        referenced models in that simulation and returns the same
#'        \code{Simulation} object but with references added to the new draws
#'        created.
#' @param nsim number of simulations to be conducted.  If a scalar, then
#'        value repeated for each index.  Otherwise can be a vector of length
#'        \code{length(index)}
#' @param index a vector of positive integer indices.  Allows simulations to be
#'        carried out in chunks.  Each chunk gets a separate RNG stream,
#'        meaning that the results will be identical whether we run these in
#'        parallel or sequentially.
#' @param parallel either \code{NULL} or a list containing \code{socket_names}
#'        and (optionally) \code{libraries} and \code{save_locally}
#'        (see Details for more information)
#' @seealso \code{\link{load_draws}} \code{\link{generate_model}} \code{\link{run_method}}
#' @examples
#' \dontrun{
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model, n = 20) %>%
#'    simulate_from_model(nsim = 50, index = 1:3,
#'      parallel = list(socket_names = 3))
#'  }
simulate_from_model <- function(object, nsim,
                                index = 1, parallel = NULL) {
  if (class(object) == "Simulation")
    model_ref <- model(object, reference = TRUE)
  else
    model_ref <- object
  if (class(model_ref) == "list") {
    dref <- lapply(model_ref, simulate_from_model, nsim = nsim, index = index,
           parallel = parallel)
    if (class(object) == "Simulation")
      return(invisible(add(object, dref)))
    return(invisible(dref))
  }
  stopifnot(index == round(index), index > 0)
  stopifnot(nsim == round(nsim), nsim > 0)
  if (length(nsim) == 1) {
    nsim <- rep(nsim, length(index))
  } else {
    stopifnot(length(nsim) == length(index))
    o <- order(index)
    index <- index[o]; nsim <- nsim[o]
  }
  dir <- model_ref@dir
  model_name <- model_ref@name
  if (model_ref@simulator.files != getOption("simulator.files"))
    stop("model_ref@simulator.files must match getOption(\"simulator.files\")")
  md <- get_model_dir_and_file(dir, model_name,
                               simulator.files = model_ref@simulator.files)
  # generate L'Ecuyer seeds based on model's seed
  m <- load_model(dir, model_name, more_info = TRUE,
                  simulator.files = model_ref@simulator.files)
  model_seed <- m$rng$rng_seed # seed used to generate m$model
  seeds <- get_seeds_for_draws(model_seed, index)
  dref <- list() # a list of DrawsRef objects
  if (is.null(parallel) || length(index) == 1) {
    # simulate sequentially
    for (i in seq(length(index))) {
       d <- simulate_from_model_single(m$model, nsim = nsim[i],
                                       index = index[i], seed = seeds[[i]])
       dref[[i]] <- save_draws_to_file(md$dir, model_ref, index[i], nsim[i],
                                       d$draws, d$rng, d$time[1])

    }
  } else {
    check_parallel_list(parallel)
    if (is.null(parallel$save_locally)) parallel$save_locally <- FALSE
    dref <- simulate_parallel(model_ref, nsim, index, seeds = seeds,
                               socket_names = parallel$socket_names,
                               libraries = parallel$libraries,
                               save_locally = parallel$save_locally)
  }
  if (class(object) == "Simulation")
    return(invisible(add(object, dref)))
  invisible(dref)
}

save_draws_to_file <- function(out_dir, model_ref, index, nsim, draws, rng,
                               time) {
  file <- sprintf("%s/r%s.Rdata", out_dir, index)
  save(draws, rng, file = file)
  catsim(sprintf("..Simulated %s draws in %s sec and saved in %s", nsim,
                 round(time, 2), sprintf("%s/r%s.Rdata", model_ref@name,
                                         index)), fill = TRUE)
  new("DrawsRef", dir = model_ref@dir, model_name = model_ref@name,
      index = index, simulator.files = getOption("simulator.files"))
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
  args <- setdiff(names(formals(model@simulate)), "nsim")
  time <- system.time({
    sims1 <- do.call(model@simulate, c(model@params[args], nsim = nsim))
  })
  if (length(sims1) != nsim)
    stop("model's simulate function must return list of length nsim.")
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
#' be used to load one or more of the saved \code{\linkS4class{Draws}} object(s)
#' (along with RNG information).  If multiple indices are provided, these will
#' be combined into a new single \code{\linkS4class{Draws}} object.
#' If simulation object is available, it is easier to use the function
#' \code{\link{draws}} to load it.
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param index a vector of positive integers.
#' @param more_info if TRUE, then returns additional information such as
#'        state of RNG after calling \code{\link{generate_model}}
#' @param simulator.files if NULL, then \code{getOption("simulator.files")}
#'        will be used.
#' @seealso \code{\link{simulate_from_model}} \code{\link{draws}}
load_draws <- function(dir, model_name, index, more_info = FALSE,
                       simulator.files = NULL) {
  md <- get_model_dir_and_file(dir, model_name,
                               simulator.files = simulator.files)
  index <- sort(unique(index))
  draws_files <- sprintf("%s/r%s.Rdata", md$dir, index)
  if (length(index) == 1) {
    env <- new.env()
    tryCatch(load(draws_files, envir = env),
         warning=function(w) stop(sprintf("Could not find draws file at %s.",
                                          draws_files)))
    draws <- env$draws
    if (more_info) return(list(draws = draws, rng = env$rng))
    else return(draws)
  }
  newdraws <- rnglist <- list()
  env <- new.env()
  for (i in seq_along(index)) {
    tryCatch(load(draws_files[i], envir = env),
          warning=function(w) stop(sprintf("Could not find draws file at %s.",
                                           draws_files[i])))
    newdraws <- c(newdraws, env$draws@draws)
    rnglist[[i]] <- env$rng
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
