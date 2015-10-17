#' @include model-class.R
NULL

#' Generate a model.
#'
#' This function executes the \code{make_model} function provided by the user
#' and writes to file the resulting \code{\link{Model}} object.  For example,
#' when simulating regression with a fixed design, \code{X} would be generated
#' in this function and \code{n}, \code{p}, \code{beta}, and \code{sigma} would
#' also be specified.
#'
#' \code{make_model} is called generating an object of class
#' \code{\link{Model}}, called \code{model}, which is saved to
#' \code{dir/name/model.Rdata} (where \code{name} is the name attribute of
#' \code{model}). This file also contains the random number generator state and
#' other information such as the function \code{make_model} itself and the date
#' when \code{model} was created.
#'
#' @export
#' @param make_model a function that outputs an object of class
#'        \code{\link{Model}}
#' @param dir directory where directory named "files" exists (or is created) to
#'        save \code{\link{Model}} object in. Default is current working
#'        directory
#' @param seed an integer seed for the random number generator.
#' @param ... optional parameters that may be passed to make_model
#' @seealso \code{\link{simulate_from_model}} \code{\link{run_method}}
#' @examples
#' \dontrun{
#'  make_my_model <- function() {
#'     # this function returns an object of class Model
#'     n <- 20
#'     params <- list(n = n, mu = rnorm(n))
#'     simulate <- function(mu, n, nsim) {
#'       # define function here that returns a list of length nsim
#'       y <- list()
#'       for (i in seq(nsim))  y[[i]] <- mu + rnorm(n)
#'       return(y)
#'     }
#'     return(new("Model", name = "fm", label = "My First Model",
#'                params = params, simulate = simulate))
#'  }
#'  generate_model(make_my_model, dir = ".")
#'  }
generate_model <- function(make_model, dir = ".", seed=123, ...) {
  # Thinking of this as an internal function.  See MakeFixed.
  stopifnot(class(make_model) == "function")
  dir <- remove_slash(dir)
  stopifnot(file.info(dir)$isdir)
  # initialize lecuyer RNG:
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  rng_seed <- .Random.seed # this is the seed used when Model is generated
  # generate model object:
  model <- make_model(...)
  if (class(model) != "Model")
    stop("make_model must return an object of class Model.")
  # create directories files and files/model_name if don't exist)
  files_dir <- file.path(dir, getOption("simulator.files"))
  model_dir <- file.path(files_dir, model@name)
  if (!file.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
  # save model to file
  file <- sprintf("%s/model.Rdata", model_dir)
  rng <- list(rng_end_seed = .Random.seed,
              rng_seed = rng_seed)
  info <- list(make_model = make_model, date_generated = date())
  save(model, rng, info, file = file)
  catsim("..Created model and saved in", file, fill = TRUE)
  invisible(file)
}


#' Load a model from file.
#'
#' After \code{\link{generate_model}} has been called, this function can be used
#' to load the saved \code{\link{Model}} object (along with the RNG state and
#' other information if desired).
#'
#' Depending on \code{more_info}, either returns \code{\link{Model}} object
#' or a list containing \code{\link{Model}} object and other information.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param more_info if TRUE, then returns additional information such as
#'        state of RNG after calling \code{\link{generate_model}}
#' @seealso \code{\link{generate_model}} \code{\link{load_draws}}
#' @examples
#' \dontrun{
#' # see example ?generate_model for make_my_model definition
#' generate_model(make_my_model, dir = ".")
#' load_model(dir = ".", model_name = "fm")
#' }
load_model <- function(dir, model_name, more_info = FALSE) {
  md <- get_model_dir_and_file(dir, model_name)
  tryCatch(load(md$file),
           warning=function(w)
             stop(sprintf("Could not find model file at %s.", md$file)))
  if (more_info)
    return(list(model = model, rng = rng, info = info))
  else
    return(model)
}
