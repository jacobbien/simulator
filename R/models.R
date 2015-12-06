#' @include model-class.R
NULL

#' Generate a model.
#'
#' This function executes the \code{make_model} function provided by the user
#' and writes to file the resulting \code{\link{Model}} object(s).  For example,
#' when simulating regression with a fixed design, \code{X} would be generated
#' in this function and \code{n}, \code{p}, \code{beta}, and \code{sigma} would
#' also be specified.
#'
#' When \code{make_model} has arguments, these can be passed using \code{...}.
#' These will be passed directly to \code{make_model} except for any arguments
#' named in \code{vary_along}.  These arguments should be lists and a separate
#' model will be created for each combination of elements in these lists.  For
#' example, if \code{vary_along = c("n", "p")}, then we can pass
#' \code{n=as.list(c(50, 100, 150))} and \code{p=as.list(c(10, 100))} and 6
#' models will be created, one for each pair of \code{n} and \code{p}.  Function
#' returns a reference or list of references to the model(s) generated.
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
#' @param vary_along character vector with all elements contained in names(...)
#'        See description for more details.
#' @param ... optional parameters that may be passed to make_model
#' @seealso \code{\link{simulate_from_model}} \code{\link{run_method}}
#' @examples
#' \dontrun{
#'  make_my_model <- function(n = 10) {
#'     # this function returns an object of class Model
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
#'  generate_model(make_my_model, dir = ".", n = 20)
#'  generate_model(make_my_model, dir = ".", n = as.list(c(10, 20, 30)),
#'                 vary_along = "n")
#'  }
generate_model <- function(make_model, dir = ".", seed = 123, vary_along = NULL, ...) {
  stopifnot(class(make_model) == "function")
  dir <- remove_slash(dir)
  stopifnot(file.info(dir)$isdir)
  passed_params <- as.list(match.call(expand.dots = FALSE)$`...`)
  passed_params <- lapply(passed_params, eval)
  if (is.null(vary_along))
    return(generate_model_single(make_model, dir, seed, passed_params))
  stopifnot(is.character(vary_along))
  if (!all(vary_along %in% names(passed_params)))
    stop("vary_along must only include names of parameters passed via \"...\".")
  if (!all(unlist(lapply(passed_params[vary_along], is.list))))
      stop("each parameter named in vary_along must be passed (through ...)",
           " as a list.")
  indices <- lapply(passed_params[vary_along], function(a) seq(length(a)))
  ii <- expand.grid(indices)
  # loop over all combinations of parameters named in vary_along
  params_to_pass <- passed_params
  mref <- list()
  for (i in seq(nrow(ii))) {
    for (j in seq(ncol(ii))) { # jth vary_along parameter
      var <- vary_along[j]
      params_to_pass[[var]] <- passed_params[[var]][[ii[i, j]]]
    }
    extension <- paste(vary_along, ii[i, ], sep = "", collapse = "/")
    mref[[i]] <- generate_model_single(make_model, dir, seed, params_to_pass,
                                       extension)
  }
  invisible(mref)
}

generate_model_single <- function(make_model, dir, seed, params_to_pass,
                                  extension = NULL) {
  # initialize lecuyer RNG:
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  rng_seed <- .Random.seed # this is the seed used when Model is generated
  # generate model object:
  if (is.null(params_to_pass))
    model <- make_model()
  else
    model <- do.call(make_model, params_to_pass)
  if (class(model) != "Model")
    stop("make_model must return an object of class Model.")
  # if any passed parameters are added to model@params by make_model, make sure
  # they match the values passed (to avoid a potentially hard to find bug)
  added_already <- intersect(names(params_to_pass), names(model@params))
  for (param in added_already) {
    if (!identical(model@params[[param]], params_to_pass[[param]]))
      warning("make_model sets ", param, " to a value different from value",
           " passed to it by generate_model.")
  }
  to_add <- setdiff(names(params_to_pass), names(model@params))
  model@params <- c(model@params, params_to_pass[to_add])
  if (!is.null(extension))
    model@name <- sprintf("%s/%s", model@name, extension)
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
  catsim(paste0("..Created model and saved in ", model@name, "/model.Rdata"),
         fill = TRUE)
  model_ref <- new("ModelRef", name = model@name, dir = dir,
                   simulator.files = getOption("simulator.files"))
  invisible(model_ref)
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
#' @param simulator.files if NULL, then \code{getOption("simulator.files")}
#'        will be used.
#' @seealso \code{\link{generate_model}} \code{\link{load_draws}}
#' @examples
#' \dontrun{
#' # see example ?generate_model for make_my_model definition
#' generate_model(make_my_model, dir = ".")
#' load_model(dir = ".", model_name = "fm")
#' }
load_model <- function(dir, model_name, more_info = FALSE,
                       simulator.files = NULL) {
  md <- get_model_dir_and_file(dir, model_name,
                               simulator.files = simulator.files)
  tryCatch(load(md$file),
           warning=function(w)
             stop(sprintf("Could not find model file at %s.", md$file)))
  if (more_info)
    return(list(model = model, rng = rng, info = info))
  else
    return(model)
}
