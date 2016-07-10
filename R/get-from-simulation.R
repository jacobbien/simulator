#' @include simulation-class.R
NULL

#' Get one or more models from a simulation
#'
#' Returns either the models themselves or references to them.
#'
#' There are two main ways to specify a subset of the models.  (1) The easiest
#' way is by writing a conditional expression involving the parameters and
#' passing it through \code{...}.  For example, \code{n > 100 & p <= 20}.
#' Only parameters that are length one and either numeric or character can be
#' used in these expressions.  (2) The faster way to retrieve a subset of
#' models is to use the \code{subset} argument.  This can be either a set of
#' numerical values (specifying which models to load based on the order in
#' which the models are stored in the simulation object.  This order can be
#' ascertained by printing the simulation object.) or as a set of a character
#' vector of the model names desired.
#'
#'  While approach (1) is very convenient, it requires loading all models from
#'  file. This may be slow in situations in which there are a lot of models
#'  and/or the models are large and thus slow to load.
#'
#' @param sim a simulation object
#' @param ... logical conditions to specify a subset of models.  Conditions can
#'        only involve params of model that have length 1 and are of class
#'        numeric or character.
#' @param subset a vector of integers indexing the models or a vector of model
#'        names. To select models based on parameter values, use \code{...}.
#'        However, using \code{...} is slower than using subset.
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
model <- function(sim, ..., subset = NULL, reference = FALSE) {
  ii <- get_model_indices(sim, subset)
  mref <- sim@model_refs[ii]
  obj <- lapply(mref, function(m) {
    m@dir <- normalizePath(file.path(sim@dir, m@dir), winslash = "/")
    return(m)
  })
  if (length(obj) == 1) obj <- obj[[1]]
  passed_a_condition <- length(match.call(expand.dots = FALSE)$`...`) != 0
  if (!passed_a_condition) {
    if (reference)
      return(obj)
    else
      return(load(obj))
  }
  # if a condition was passed, we have to load the models to apply condition
  obj1 <- load(obj)
  model_names <- subset_models(obj1, ...)
  if (length(obj1) == 1) {
    if (obj1@name %in% model_names) {
      if (reference) {
        return(obj)
      } else {
      return(obj1)
      }
    } else {
      return(list())
    }
  }
  to_return <- unlist(lapply(mref, function(mm) mm@name %in% model_names))
  if (reference)
    obj <- obj[to_return]
  else
    obj <- obj1[to_return]
  if (length(obj) == 1)
    return(obj[[1]])
  else
    return(obj)
}

#' Get one or more draws from a simulation
#'
#' Returns either the draws objects themselves or references to them.  See
#' \code{\link{model}} function for more information on the \code{...} and
#' \code{subset} arguments, which are used to specify a subset of the models.
#'
#' @param sim a simulation object
#' @param ... logical conditions to specify a subset of models.  Conditions can
#'        only involve params of model that have length 1 and are of class
#'        numeric or character.
#' @param subset a vector of integers indexing the models or a vector of model
#'        names. To select models based on parameter values, use \code{...}.
#'        However, using \code{...} is slower than using subset.
#' @param index a vector of positive integers specifying which draws objects
#'        are desired. If missing, then all draws' outputs are returned.
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
#' @examples
#' \dontrun{
#'  # suppose previously we had run the following:
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model, n = 20) %>%
#'    simulate_from_model(nsim = 50, index = 1:3)
#'  # then we could get the simulated draws as follows:
#'  d <- draws(sim)
#'  d@draws$r1.1 # first random draw
#'  }
draws <- function(sim, ..., subset = NULL, index, reference = FALSE) {
  if (!missing(index))
    stopifnot(is.numeric(index), index > 0, index == round(index))
  if (length(sim@draws_refs) == 0) return(list())
  dref <- sim@draws_refs
  mref <- model(sim, ..., subset = subset, reference = TRUE)
  if (length(mref) == 1) mref <- list(mref)
  subset_model_names <- unlist(lapply(mref, function(ref) ref@name))
  obj <- list()
  for (i in seq_along(dref)) {
    if (length(dref[[i]]) == 0) next # no draws are in this model
    if (!(dref[[i]][[1]]@model_name %in% subset_model_names))
      next # subset excluded this model
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
                                                   obj[[i]][[j]]@dir),
                                         winslash = "/")
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
#' @param ... logical conditions to specify a subset of models.  Conditions can
#'        only involve params of model that have length 1 and are of class
#'        numeric or character.
#' @param subset a vector of integers indexing the models or a vector of model
#'        names. To select models based on parameter values, use \code{...}.
#'        However, using \code{...} is slower than using subset.
#' @param index a vector of positive integers specifying which draws' objects
#'        are desired. If missing, then all draws' outputs are returned.
#' @param methods character vector of method names of interest.  If missing,
#'        then all methods' outputs are returned
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
#' @examples
#' \dontrun{
#'  # suppose previously we had run the following:
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model, n = 20) %>%
#'    simulate_from_model(nsim = 50, index = 1:3) %>%
#'    run_method(my_example_method)
#'  # then we could get the method's output as follows:
#'  o <- output(sim)
#'  o@out$r1.1 # first random draw's output
#'  }
output <- function(sim, ..., subset = NULL, index, methods,
                   reference = FALSE) {
  outputs_or_evals(sim, sim@output_refs, TRUE, subset, index, methods,
                   reference, ...)
}


#' Get one or more evals from a simulation
#'
#' Returns either the Evals object itself or a reference to it.
#'
#' @param sim a simulation object
#' @param ... logical conditions to specify a subset of models.  Conditions can
#'        only involve params of model that have length 1 and are of class
#'        numeric or character.
#' @param subset a vector of integers indexing the models or a vector of model
#'        names. To select models based on parameter values, use \code{...}.
#'        However, using \code{...} is slower than using subset.
#' @param index a vector of positive integers specifying which draws' objects
#'        are desired. If missing, then all draws' evals are returned.
#' @param methods character vector of method names of interest.  If missing,
#'        then all methods' evals are returned
#' @param reference whether to return the ModelRef or the Model object itself
#' @export
#' @seealso \code{\link{as.data.frame}}
#' @examples
#' \dontrun{
#'  # suppose previously we had run the following:
#'  sim <- new_simulation(name = "normal-example",
#'                        label = "Normal Mean Estimation",
#'                        dir = tempdir()) %>%
#'    generate_model(make_my_example_model, n = 20) %>%
#'    simulate_from_model(nsim = 50, index = 1:3) %>%
#'    run_method(my_example_method) %>%
#'    evaluate(my_example_loss)
#'  # then we could get the metric evaluated on the method's output:
#'  e <- evals(sim)
#'  # we can export it as a data.frame
#'  as.data.frame(e)
#'  # or we can get at a particular draw-method-metric triplet
#'  e@evals$`my-method`$r1.1$myloss
#'  }
evals <- function(sim, ..., subset = NULL, index, methods,
                  reference = FALSE) {
  outputs_or_evals(sim, sim@evals_refs, FALSE, subset, index, methods,
                   reference, ...)
}

#' Internal function used by both outputs and evals
#'
#' @param sim simulation object
#' @param refs either sim@@output_refs or sim@@evals_refs
#' @param sort_by_method whether returned object should have each method's objects
#'        in its own list or not
#' @keywords internal
outputs_or_evals <- function(sim, refs, sort_by_method,
                             subset, index, methods, reference, ...) {
  if (!missing(index))
    stopifnot(is.numeric(index), index > 0, index == round(index))
  if (length(refs) == 0) return(list())
  mref <- model(sim, ..., subset = subset, reference = TRUE)
  if (length(mref) == 1) mref <- list(mref)
  subset_model_names <- unlist(lapply(mref, function(ref) ref@name))
  obj <- list()
  for (i in seq_along(refs)) {
    if (length(refs[[i]]) == 0) next # no outputs are in this model
    if (!(refs[[i]][[1]]@model_name %in% subset_model_names)) next # subset excluded this model
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
      refs[[i]][[o]]@dir <- normalizePath(file.path(sim@dir, refs[[i]][[o]]@dir),
                                          winslash = "/")
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
  } else stop("subset is not in a valid format.")
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
#' @param ... logical conditions to specify a subset of models.  Conditions can
#'        only involve params of model that have length 1 and are of class
#'        numeric or character.
#' @param subset a vector of integers indexing the models or a vector of model
#'        names. To select models based on parameter values, use \code{...}.
#'        However, using \code{...} is slower than using subset.
#' @param index a vector of positive integers specifying which draws' objects
#'        are desired. If missing, then all draws' evals are returned.
#' @param methods character vector of method names of interest.  If missing,
#'        then all methods' evals are returned
#' @export
subset_simulation <- function(sim, ..., subset = NULL, index, methods) {
  if (is.null(subset)) subset = seq_along(sim@model_refs)
  mref <- model(sim, ..., subset = subset, reference = TRUE)
  dref <- draws(sim, ..., subset = subset, index = index, reference = TRUE)
  oref <- output(sim, ..., subset = subset, index = index, methods = methods,
                 reference = TRUE)
  eref <- evals(sim, ..., subset = subset, index = index, methods = methods,
                 reference = TRUE)
  new_simulation(name = sim@name, label = sim@label, dir = sim@dir,
                 refs = c(mref, dref, oref, eref), save_to_file = FALSE)
}
