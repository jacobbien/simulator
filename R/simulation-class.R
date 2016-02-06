#' @include reference-classes.R

check_simulation <- function(object) {
  errors <- check_component(object)
  str <- "%s must be a list (or nested lists) of %s objects."
  if (any(lapply(unlist(object@model_refs), class) != "ModelRef"))
    errors <- c(errors, sprintf(str, "model_refs", "ModelRef"))
  if (any(lapply(unlist(object@draws_refs), class) != "DrawsRef"))
    errors <- c(errors, sprintf(str, "draws_refs", "DrawsRef"))
  if (any(lapply(unlist(object@output_refs), class) != "OutputRef"))
    errors <- c(errors, sprintf(str, "output_refs", "OutputRef"))
  if (any(lapply(unlist(object@evals_refs), class) != "EvalsRef"))
    errors <- c(errors, sprintf(str, "evals_refs", "EvalsRef"))
  if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing a simulation.
#'
#' A simulation is a set of references to simulator objects that have been
#' saved to file.  The DrawsRef, OutputRef, and EvalsRef objects are organized
#' by model into separate lists.
#'
#' When a reference ref is added to a simulation sim, ref@@dir is changed so
#' that the referenced file is located at file.path(sim@@dir, ref@@dir).
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param dir common directory which serves as basis for individual reference
#'        paths
#' @slot model_refs a list of \code{\link{ModelRef}} objects
#' @slot draws_refs a list of lists of \code{\link{DrawsRef}} objects
#' @slot output_refs a list of lists of \code{\link{OutputRef}} objects
#' @slot evals_refs a list of lists of \code{\link{EvalsRef}} objects
#'
#' @rdname Simulation
#' @export
setClass("Simulation",
         representation(
           name = "character", # shortname identifier
           label = "character", # human readable label
           dir = "character",
           model_refs = "list",
           draws_refs = "list",
           output_refs = "list",
           evals_refs = "list"
         ),
         prototype(name = NA_character_, label = NA_character_, dir = "."),
         contains = "Component",
         validity = check_component
)

setMethod("show", "Simulation", function(object) {
  validObject(object)
  callNextMethod()
  cat(" dir:", object@dir, fill = TRUE)
  mrefs <- unlist(object@model_refs) # flatten list
  drefs <- unlist(object@draws_refs)
  orefs <- unlist(object@output_refs)
  cat(sprintf("Has %s model(s).", length(mrefs)), fill = TRUE)
  model_names <- unlist(lapply(mrefs, function(a) a@name))
  dref_model_names <- unlist(lapply(drefs, function(a) a@model_name))
  dref_model <- match(dref_model_names, model_names)
  indices <- unlist(lapply(drefs, function(a) a@index))
  oref_model_names <- unlist(lapply(orefs, function(a) a@model_name))
  oref_model <- match(oref_model_names, model_names)
  oref_indices <- unlist(lapply(orefs, function(a) a@index))
  oref_indices <- match(oref_indices, indices)
  for (i in seq_along(mrefs)) {
    cat(paste0(" ", i, ") "))
    cat("model_name:", mrefs[[i]]@name, fill = TRUE)
    dref <- drefs[dref_model == i]
    ind <- unlist(lapply(dref, function(ref) ref@index))
    if (length(ind) == 0) {
      cat("    no draws", fill = TRUE)
      next
    }
    o <- order(ind)
    ind <- ind[o]
    dref <- dref[o]
    ind_string <- paste(ind, collapse = ", ")
    cat("    draws for index", ind_string, fill = TRUE)
    for (d in seq_along(dref)) {
      cat(sprintf("    %s) draw index %s", d, ind[d]), fill = TRUE)
      oref <- orefs[oref_model == i & oref_indices == ind[d]]
      if (length(oref) == 0) next
      for (o in seq_along(oref)) {
        cat("     outputs:", oref[[o]]@method_name, fill = TRUE)
      }
    }
  }
})

#' Make a new simulation object
#'
#' Creates an object of class \code{\link{Simulation}}.  In addition to having
#' a name and label, this object consists of a set of references to objects of
#' class \code{\link{ModelRef}}, \code{\link{DrawsRef}},
#' \code{\link{OutputRef}}, and \code{\link{EvalsRef}}.
#'
#' A Simulation object is the basic unit of a simulation study.  Roughly, one
#' can think of it as all the files relevant to a single figure. This might be
#' a single plot or a series of related plots/panels.  It could also correspond
#' to a single table.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot dir a directory that reference's directories are relative to
#' @slot model_refs a list of \code{\link{ModelRef}} objects
#' @slot draws_refs a list of \code{\link{DrawsRef}} objects
#' @slot output_refs a list of \code{\link{OutputRef}} objects
#' @slot evals_refs a list of \code{\link{EvalsRef}} objects
#' @export
#' @seealso \code{\link{add}}
new_simulation <- function(name, label, dir = ".", refs = list()) {
  sim <- new("Simulation", name = name, label = label, dir = dir)
  return(add(sim, refs))
}



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
model <- function(sim, subset = 1:num_models, reference = FALSE) {
  num_models <- length(sim@model_refs)
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
draws <- function(sim, subset, index, reference = FALSE) {
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
    if (missing(index))
      obj[[i]] <- dref[[i]]
    else {
      index_of_dref <- lapply(dref[[i]], function(d) d@index)
      if (any(index_of_dref %in% index)) {
        obj[[i]] <- dref[[i]][index_of_dref %in% index]
      } else next # all draws indices for this model do not appear in index
    }
    # each object's dir should no longer be relative to sim's dir
    obj[[i]] <- lapply(obj[[i]], function(d) {
      d@dir <- normalizePath(file.path(sim@dir, d@dir)); return(d)})
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
output <- function(sim, subset, index, methods, reference = FALSE) {
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
evals <- function(sim, subset, index, methods, reference = FALSE) {
  outputs_or_evals(sim, sim@evals_refs, FALSE, subset, index, methods, reference)
}

#' Internal function used by both outputs and evals
#'
#' @param refs either sim@@output_refs or sim@@evals_refs
#' @param sort_by_method whether returned object should have each method's objects
#'        in its own list or not
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
