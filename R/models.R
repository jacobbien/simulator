#' @include model-class.R
NULL

#' Generate a model.
#'
#' This function executes the \code{make_model} function provided by the user
#' and writes to file the resulting \code{\linkS4class{Model}} object(s).  For example,
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
#' models will be created, one for each pair of \code{n} and \code{p}.  For each
#' pair (n,p), a distinct extension is added to the end of the model name. This
#' extension is generated using a hash function so that different values of the
#' vary_along parameters will lead to different model name extensions. This
#' ensures that if one later decides to add more values of the vary_along
#' parameters, this will not lead to pre-existing files being overwritten
#' (unless the same values of the vary_along combination are used again.
#'
#' If \code{object} is a directory name, the function returns a reference or
#' list of references to the model(s) generated. If \code{object} is a
#' \code{Simulation}, then function returns the same \code{Simulation} object
#' but with references added to the new models created.  These changes to the
#' \code{Simulation} object are saved to file.
#'
#' \code{make_model} is called generating an object of class
#' \code{\linkS4class{Model}}, called \code{model}, which is saved to
#' \code{dir/name/model.Rdata} (where \code{name} is the name attribute of
#' \code{model}). This file also contains the random number generator state and
#' other information such as the function \code{make_model} itself and the date
#' when \code{model} was created.
#'
#' @export
#' @param object the name of the directory where directory named "files" exists
#'        (or should be created) to save \code{\linkS4class{Model}} object in.
#'        Default is current working directory. Or can be an object of class
#'        \code{\linkS4class{Simulation}}, in which case the \code{object@@dir} is used
#'        and a simulation object is returned instead of an object of class
#'        \code{\linkS4class{ModelRef}}.
#' @param make_model a function that outputs an object of class
#'        \code{\linkS4class{Model}}.  Or a list of such functions.
#' @param ... optional parameters that may be passed to make_model
#' @param seed an integer seed for the random number generator.
#' @param vary_along character vector with all elements contained in names(...)
#'        See description for more details.
#' @seealso \code{\link{new_model}} \code{\link{simulate_from_model}}
#' \code{\link{run_method}}
#' @examples
#'  # initialize a new simulation
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir())
#'  # generate a model (and add it to the simulation)
#'  sim <- generate_model(sim, make_my_example_model, n = 20)
#'  # generate a sequence of models (and add them to the simulation)
#'  sim <- generate_model(sim, make_my_example_model,
#'                        n = list(10, 20, 30),
#'                        vary_along = "n")
generate_model <- function(object = ".", make_model, ..., seed = 123,
                           vary_along = NULL) {
  stopifnot(length(object) == 1)
  if (is(object, "Simulation"))
    dir <- object@dir
  else if (is(object, "character"))
    dir <- object
  else stop("object must be of class 'character' or 'Simulation'.")
  if (is(make_model, "list")) {
    mrefs <- lapply(make_model,
                    function(mm) {
                      generate_model(object = dir, make_model = mm,
                                     seed = seed, vary_along = vary_along,
                                     ...)
                    })
    if (is(object, "Simulation"))
      return(invisible(add(object, mrefs)))
    else
      return(invisible(mrefs))
  } else stopifnot(is(make_model, "function"))
  make_model_args <- names(formals(make_model))
  illegal_arguments <- c("seed", "object", "vary_along")
  if (any(illegal_arguments %in% make_model_args))
    stop(sprintf("Function 'make_model' cannot have an argument named '%s'.",
                 illegal_arguments[illegal_arguments %in% make_model_args][1]))
  dir <- remove_slash(dir)
  stopifnot(file.info(dir)$isdir)
  passed_params <- as.list(match.call(expand.dots = FALSE)$`...`)
  passed_params <- lapply(passed_params, eval)
  if (length(passed_params) > 1) {
    # add any passed parameters that aren't part of "vary_along" to it as singletons
    # so that the model name stem will be added to
    passed_but_not_varied <- setdiff(names(passed_params), vary_along)
    for (var_name in passed_but_not_varied) {
      passed_params[[var_name]] <- list(passed_params[[var_name]])
    }
    vary_along <- sort(c(vary_along, passed_but_not_varied))
  }
  if (is.null(vary_along)) {
    mref <- generate_model_single(make_model, dir, seed, passed_params)
    if (is(object, "Simulation"))
      return(invisible(add(object, mref)))
    else
      return(invisible(mref))
  }
  # everything beyond this point focuses on using "vary_along" functionality
  stopifnot(is.character(vary_along))
  if (!all(vary_along %in% names(passed_params)))
    stop("vary_along must only include names of parameters passed via \"...\".")
  if (!all(unlist(lapply(passed_params[vary_along], is.list))))
      stop("each parameter named in vary_along must be passed (through ...)",
           " as a list.")
  # vary_along parameters that can be written as a short decimal, an integer,
  # or a character string will not be digest
  is_fine_as_is <- rep(TRUE, length(vary_along))
  for (j in seq_along(vary_along)) {
    for (k in seq_along(passed_params[[vary_along[j]]])) {
      val <- passed_params[[vary_along[j]]][[k]]
      if (length(val) != 1) {
        is_fine_as_is[j] <- FALSE
        break
      }
      if (is(val, "character")) next
      if (is(val, "integer")) next
      if (is(val, "numeric"))
        if(abs(round(val, getOption("simulator.ndecimal")) - val) < 1e-12)
          next
      is_fine_as_is[j] <- FALSE
    }
  }
  if (!all(is_fine_as_is)) {
    if (!requireNamespace("digest", quietly = TRUE))
      stop("The package digest must be installed for vary_along to be used",
           " with parameters that are not easily represented as strings.",
           call. = FALSE)
  }
  indices <- lapply(passed_params[vary_along], function(a) seq(length(a)))
  ii <- expand.grid(indices)
  # loop over all combinations of parameters named in vary_along
  params_to_pass <- passed_params
  mref <- list()
  ext <- rep(NA, ncol(ii))
  for (i in seq(nrow(ii))) {
    for (j in seq(ncol(ii))) { # jth vary_along parameter
      var <- vary_along[j]
      params_to_pass[[var]] <- passed_params[[var]][[ii[i, j]]]
      if (is_fine_as_is[j]) {
        ext[j] <- sprintf("%s_%s", var,
                          format(params_to_pass[[var]], scientific = FALSE))
      } else {
        # apply hash to this object to get a unique file name
        # this means that if we later decide to add some more combinations,
        # we will not write on top of the pre-existing ones (unless the same
        # value combinations of the vary_along parameters are used).
        ext[j] <- paste0(var, "_", digest::sha1(params_to_pass[[var]]))
      }
    }
    extension <- paste(ext, collapse = "/")
    mref[[i]] <- generate_model_single(make_model, dir, seed, params_to_pass,
                                       extension)
  }
  model_labels <- unlist(lapply(mref, function(m) m@label))
  if (length(unique(model_labels)) < length(model_labels))
    warning("Labels are not unique across models.  This can lead to confusion.")
  if (is(object, "Simulation"))
    return(invisible(add(object, mref)))
  if (length(mref) == 1) mref <- mref[[1]]
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
  if (!is(model, "Model"))
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
  model_ref <- new("ModelRef", name = model@name, label = model@label,
                   dir = dir, simulator.files = getOption("simulator.files"))
  invisible(model_ref)
}

#' Load a model from file.
#'
#' After \code{\link{generate_model}} has been called, this function can be used
#' to load the saved \code{\linkS4class{Model}} object (along with the RNG state and
#' other information if desired).
#'
#' Depending on \code{more_info}, either returns \code{\linkS4class{Model}} object
#' or a list containing \code{\linkS4class{Model}} object and other information.
#' If simulation object is available, it is easier to use the function
#' \code{\link{model}} to load the model.
#'
#' @export
#' @param dir the directory passed to \code{\link{generate_model}})
#' @param model_name the Model object's \code{name} attribute
#' @param more_info if TRUE, then returns additional information such as
#'        state of RNG after calling \code{\link{generate_model}}
#' @param simulator.files if NULL, then \code{getOption("simulator.files")}
#'        will be used.
#' @seealso \code{\link{generate_model}} \code{\link{model}}
load_model <- function(dir, model_name, more_info = FALSE,
                       simulator.files = NULL) {
  md <- get_model_dir_and_file(dir, model_name,
                               simulator.files = simulator.files)
  env <- new.env()
  tryCatch(load(md$file, envir = env),
           warning=function(w)
             stop(sprintf("Could not find model file at %s.", md$file)))
  model <- env$model
  if (more_info)
    return(list(model = model, rng = env$rng, info = env$info))
  else
    return(model)
}
