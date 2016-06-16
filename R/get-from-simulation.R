#' @include simulation-class.R
NULL

#' Get one or more models from a simulation
#'
#' Returns either the models themselves or references to them.
#'
#' The parameter subset can be one of three types.  (1) The fastest retrieval
#' of a model uses numerical values indicating the order in which the models
#' are stored in the simulation object.  This order can be learned by printing
#' the simulation object.  (2) The next fastest way to retrieve a subset of the
#' models is to set subset to be a character vector of the model names desired.
#' (3) Perhaps the most convenient approach is to take subset to be a list
#' specifying the values of various params in the models.  For example, if
#' \code{subset = list(n = 100, p = 10)}, then this will return all models
#' \code{m} for which \code{m$n == 100} and \code{m$p == 10}.  This approach,
#' unlike the first two approaches, requires loading all models from file.
#' This may be slow in situations in which there are a lot of models and/or
#' the models are large and thus slow to load.
#'
#' @param sim a simulation object
#' @param subset a vector indicating which models should be returned.  See
#'        below for details
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
model <- function(sim, subset = NULL, reference = FALSE) {
  ii <- get_model_indices(sim, subset)
  mref <- sim@model_refs[ii]
  obj <- lapply(mref, function(m) {
    m@dir <- normalizePath(file.path(sim@dir, m@dir))
    return(m)
  })
  if (length(obj) == 1) obj <- obj[[1]]
  if (!reference) obj <- load(obj)
  return(obj)
}

#' Get one or more draws from a simulation
#'
#' Returns either the draws objects themselves or references to them.
#'
#' @param sim a simulation object
#' @param subset specifies which models' draws should be selected. See
#'        \code{\link{model}} for details
#' @param index a vector of positive integers specifying which draws objects
#'        are desired. If missing, then all draws' outputs are returned.
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
draws <- function(sim, subset = NULL, index, reference = FALSE) {
  if (!missing(index))
    stopifnot(is.numeric(index), index > 0, index == round(index))
  if (length(sim@draws_refs) == 0) return(list())
  dref <- sim@draws_refs
  if (!missing(subset)) {
    ii <- get_model_indices(sim, subset)
    subset_model_names <- unlist(lapply(sim@model_refs,
                                        function(m) m@name))[ii]
  }
  obj <- list()
  for (i in seq_along(dref)) {
    if (!missing(subset)) {
      if (length(dref[[i]]) == 0) next # no draws are in this model
      if (!(dref[[i]][[1]]@model_name %in% subset_model_names))
        next # subset excluded this model
    }
    # this model's draws should be included (if they match the index)
    obj[[i]] <- dref[[i]]
    keep <- rep(FALSE, length(dref[[i]]))
    for (j in seq_along(dref[[i]])) {
      if (!missing(index)) {
        if (any(dref[[i]][[j]]@index %in% index)) {
          # only include the desired indices in the DrawsRef
          obj[[i]][[j]]@index <- intersect(obj[[i]][[j]]@index, index)
          keep[j] <- TRUE
        }
      }
      # each object's dir should no longer be relative to sim's dir
      obj[[i]][[j]]@dir <- normalizePath(file.path(sim@dir,
                                                   obj[[i]][[j]]@dir))
    }
    if (!missing(index)) obj[[i]] <- obj[[i]][keep]
  }
  if (length(obj) == 0) return(list())
  obj <- obj[!unlist(lapply(obj, is.null))]
  if (length(obj) == 1) obj <- obj[[1]]
  if (!reference) obj <- load(obj)
  return(obj)
}

#' Get one or more outputs from a simulation
#'
#' Returns either the output object itself or a reference to it.
#'
#' @param sim a simulation object
#' @param subset specifies which models' outputs should be selected. See
#'        \code{\link{model}} for details
#' @param index a vector of positive integers specifying which draws' objects
#'        are desired. If missing, then all draws' outputs are returned.
#' @param methods character vector of method names of interest.  If missing,
#'        then all methods' outputs are returned
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
output <- function(sim, subset = NULL, index, methods,
                   reference = FALSE) {
  outputs_or_evals(sim, sim@output_refs, TRUE, subset, index, methods, reference)
}

#' Get one or more evals from a simulation
#'
#' Returns either the Evals object itself or a reference to it.
#'
#' @param sim a simulation object
#' @param subset specifies which models' evals should be selected. See
#'        \code{\link{model}} for details
#' @param index a vector of positive integers specifying which draws' objects
#'        are desired. If missing, then all draws' evals are returned.
#' @param methods character vector of method names of interest.  If missing,
#'        then all methods' evals are returned
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
evals <- function(sim, subset = NULL, index, methods,
                  reference = FALSE) {
  outputs_or_evals(sim, sim@evals_refs, FALSE, subset, index, methods,
                   reference)
}

#' Internal function used by both outputs and evals
#'
#' @param sim simulation object
#' @param refs either sim@@output_refs or sim@@evals_refs
#' @param sort_by_method whether returned object should have each method's objects
#'        in its own list or not
#' @keywords internal
outputs_or_evals <- function(sim, refs, sort_by_method,
                             subset, index, methods, reference) {
  if (!missing(index))
    stopifnot(is.numeric(index), index > 0, index == round(index))
  if (length(refs) == 0) return(list())
  if (!missing(subset)) {
    ii <- get_model_indices(sim, subset)
    subset_model_names <- unlist(lapply(sim@model_refs, function(m) m@name))[ii]
  }
  obj <- list()
  for (i in seq_along(refs)) {
    if (!missing(subset)) {
      if (length(refs[[i]]) == 0) next # no outputs are in this model
      if (!(refs[[i]][[1]]@model_name %in% subset_model_names)) next # subset excluded this model
    }
    # this model's outputs should be included (if they match the index and methods)
    obj[[i]] <- list()
    methods_in_list <- NULL
    for (o in seq_along(refs[[i]])) {
      if (!missing(index)) {
        if (!(refs[[i]][[o]]@index %in% index)) {
          # o-th ObjectRef of model i has wrong index; don't put in obj[[i]]
          next
        }
      }
      if (!missing(methods)) {
        if (!(refs[[i]][[o]]@method_name %in% methods)) {
          # o-th ObjectRef of model i is of wrong method; don't put in obj[[i]]
          next
        }
      }
      # made it through the the index and method screen; add to obj[[i]]
      # first, each object's dir should no longer be relative to sim's dir
      refs[[i]][[o]]@dir <- normalizePath(file.path(sim@dir, refs[[i]][[o]]@dir))
      if (sort_by_method) {
        # organize ObjectRefs by method_name
        o_method_name <- refs[[i]][[o]]@method_name
        obj[[i]][[o_method_name]] <- c(obj[[i]][[o_method_name]], refs[[i]][[o]])
      } else {
        obj[[i]] <- c(obj[[i]], refs[[i]][[o]])
      }
    }
    if (sort_by_method) {
      obj[[i]] <- unname(obj[[i]])
      if (length(obj[[i]]) == 1) obj[[i]] <- obj[[i]][[1]]
    }
  }
  if (length(obj) == 0) return(list())
  obj <- obj[!unlist(lapply(obj, is.null))]
  if (length(obj) == 1) obj <- obj[[1]]
  if (!reference) obj <- load(obj)
  return(obj)
}

#' Returns indices of a specified subset of sim@@model_refs
#'
#' See \code{\link{model}} for information about the various formats of subset.
#' @param sim a simulation object
#' @param subset a vector indicating which models should be returned.
get_model_indices <- function(sim, subset) {
  num_models <- length(sim@model_refs)
  if (is.null(subset)) subset = seq(num_models)
  if (num_models == 0) stop("This simulation has no models.")
  if (is.numeric(subset)) {
    if (!all(subset == round(subset) & subset >= 1 & subset <= num_models))
      stop("subset, if numeric, must be integers between 1 and num_models.")
    ii <- subset
  } else if (is.character(subset)) {
    # subset should consist of model names
    model_names <- unlist(lapply(sim@model_refs, function(m) m@name))
    if (!all(subset %in% model_names))
      stop("subset includes an unrecognized model name.")
    ii <- match(subset, model_names)
  } else if (is.list(subset)) {
    models <- model(sim)
    ii <- rep(TRUE, length(models))
    nams <- names(subset)
    for (i in seq_along(models)) {
      for (j in seq_along(subset)) {
        if (!(nams[j] %in% names(models[[i]]@params))) {
          ii[i] <- FALSE
          next
        }
        if (!isTRUE(all.equal(models[[i]]@params[[nams[j]]], subset[[j]]))) {
          ii[i] <- FALSE
          next
        }
      }
    }
    ii <- which(ii)
  }
  else stop("subset is not in a valid format.")
  return(ii)
}

#' Create a simulation that is a subset of a preexisting simulation object
#'
#' Given a simulation, creates a new simulation that is a subset of the
#' preexisting simulation.  Does not save this new one to file.  To do so,
#' first change the name (and, potentially, label) of the simulation
#' and then use \code{\link{save_simulation}}.  If you call
#' \code{\link{save_simulation}} before changing the name, you will overwrite
#' the preexisting simulation.  Use \code{\link{rename}} and
#' \code{\link{relabel}}.
#'
#' @param sim a simulation object
#' @param subset specifies which models should be selected. See
#'        \code{\link{model}} for details
#' @param index a vector of positive integers specifying which draws' objects
#'        are desired. If missing, then all draws' evals are returned.
#' @param methods character vector of method names of interest.  If missing,
#'        then all methods' evals are returned
#' @export
subset_simulation <- function(sim, subset = NULL, index, methods) {
  if (is.null(subset)) subset = seq_along(sim@model_refs)
  mref <- model(sim, subset = subset, reference = TRUE)
  dref <- draws(sim, subset = subset, index = index, reference = TRUE)
  oref <- output(sim, subset = subset, index = index, methods = methods,
                 reference = TRUE)
  eref <- evals(sim, subset = subset, index = index, methods = methods,
                 reference = TRUE)
  new_simulation(name = sim@name, label = sim@label,
                 refs = c(mref, dref, oref, eref), save_to_file = FALSE)
}
