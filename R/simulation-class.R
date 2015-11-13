#' @include reference-classes.R

check_simulation <- function(object) {
  errors <- check_component(object)
  str <- "%s must be a list of %s objects."
  if (any(lapply(object@model_refs, class) != "ModelRef"))
    errors <- c(errors, sprintf(str, "model_refs", "ModelRef"))
  if (any(lapply(object@draws_refs, class) != "DrawsRef"))
    errors <- c(errors, sprintf(str, "draws_refs", "DrawsRef"))
  if (any(lapply(object@output_refs, class) != "OutputRef"))
    errors <- c(errors, sprintf(str, "output_refs", "OutputRef"))
  if (any(lapply(object@evals_refs, class) != "EvalsRef"))
    errors <- c(errors, sprintf(str, "evals_refs", "EvalsRef"))
  if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing a simulation.
#'
#' A simulation is a set of references to simulator objects that have been
#' saved to file.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot model_refs a list of \code{\link{ModelRef}} objects
#' @slot draws_refs a list of \code{\link{DrawsRef}} objects
#' @slot output_refs a list of \code{\link{OutputRef}} objects
#' @slot evals_refs a list of \code{\link{EvalsRef}} objects
#'
#' @rdname Simulation
#' @export
setClass("Simulation",
         representation(
           name = "character", # shortname identifier
           label = "character", # human readable label
           model_refs = "list",
           draws_refs = "list",
           output_refs = "list",
           evals_refs = "list"
         ),
         contains = "Component",
         validity = check_component
)

setMethod("show", "Simulation", function(object) {
  validObject(object)
  callNextMethod()
  cat(sprintf("Has %s model(s).", length(object@model_refs)),
      fill = TRUE)
  model_names <- unlist(lapply(object@model_refs, function(a) a@name))
  dref_model_names <- unlist(lapply(object@draws_refs,
                                    function(a) a@model_name))
  dref_model <- match(dref_model_names, model_names)
  indices <- unlist(lapply(object@draws_refs, function(a) a@index))
  oref_model_names <- unlist(lapply(object@output_refs,
                                    function(a) a@model_name))
  oref_model <- match(oref_model_names, model_names)
  oref_indices <- unlist(lapply(object@output_refs,
                                    function(a) a@index))
  oref_indices <- match(oref_indices, indices)
  for (i in seq_along(object@model_refs)) {
    cat(paste0(" ", i, ") "))
    cat("model_name:", object@model_refs[[i]]@name, fill = TRUE)
    dref <- object@draws_refs[dref_model == i]
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
      oref <- object@output_refs[oref_model == i & oref_indices == ind[d]]
      if (length(oref) == 0) next
      for (o in seq_along(oref)) {
        cat("     outputs:", oref[[o]]@method_name, fill = TRUE)
      }
    }
  }
})

add <- function(sim, ref) stop("add not implemented for this class of ref.")

#' Add a reference to a simulation
#'
#' Adds a ModelRef, DrawsRef, OutputRef, or EvalsRef to a simulation object.
#' To add a DrawsRef, the corresponding ModelRef must already be added.
#' Likewise, to add an OutputRef, the corresponding DrawsRef must already be
#' added.  And to add an EvalsRef, the corresponding OutputRef must be added.
#' One can also pass a list of such objects.
#'
#' @export
setGeneric("add")

setMethod("add", signature(sim = "Simulation", ref = "ModelRef"),
          function(sim, ref) {
            mnames <- lapply(sim@model_refs, function(ref) ref@name)
            if (ref@name %in% mnames) {
              stop("A model of this name is already in this simulation.")
            } else {
              sim@model_refs <- c(sim@model_refs, ref)
            }
            return(sim)
          })

setMethod("add", signature(sim = "Simulation", ref = "DrawsRef"),
          function(sim, ref) {
            mnames <- lapply(sim@model_refs, function(mref) mref@name)
            dnames <- lapply(sim@draws_refs, function(dref) dref@model_name)
            dindex <- lapply(sim@draws_refs, function(dref) dref@index)
            if (!(ref@model_name %in% mnames))
              stop("Cannot add draws until model named", ref@model_name,
                   "has been added to simulation.")
            if (any(dnames == ref@model_name & dindex == ref@index)) {
              cat("This draws is already in this simulation.\n")
            } else {
              sim@draws_refs <- c(sim@draws_refs, ref)
            }
            return(sim)
          })

setMethod("add", signature(sim = "Simulation", ref = "OutputRef"),
          function(sim, ref) {
            dnames <- lapply(sim@draws_refs, function(dref) dref@model_name)
            dindex <- lapply(sim@draws_refs, function(dref) dref@index)
            if (!any(dnames == ref@model_name & dindex == ref@index))
              stop("Cannot add output until draws with name ", ref@model_name,
                   " and index ", ref@index,
                   " has been added to simulation.")
            onames <- lapply(sim@output_refs, function(oref) oref@model_name)
            oindex <- lapply(sim@output_refs, function(oref) oref@index)
            omethods <- lapply(sim@output_refs,
                               function(oref) oref@method_name)
            matching <- onames == ref@model_name & oindex == ref@index
            matching <- matching & omethods == ref@method_name
            if (any(matching)) {
              cat("This output is already in this simulation.\n")
            } else {
              sim@output_refs <- c(sim@output_refs, ref)
            }
            return(sim)
          })

setMethod("add", signature(sim = "Simulation", ref = "EvalsRef"),
          function(sim, ref) {
            onames <- lapply(sim@output_refs, function(oref) oref@model_name)
            oindex <- lapply(sim@output_refs, function(oref) oref@index)
            omethod <- lapply(sim@output_refs, function(oref) oref@method_name)
            match <- onames == ref@model_name & oindex == ref@index
            match <- match & omethod == ref@method_name
            if (!any(match))
              stop("Cannot add eval until output with model_name ",
                   ref@model_name,
                   " and index ", ref@index,
                   " and method_name ", ref@method_name,
                   " has been added to simulation.")
            enames <- lapply(sim@evals_refs, function(eref) eref@model_name)
            eindex <- lapply(sim@evals_refs, function(eref) eref@index)
            emethods <- lapply(sim@evals_refs,
                               function(eref) eref@method_name)
            matching <- enames == ref@model_name & eindex == ref@index
            matching <- matching & emethods == ref@method_name
            if (any(matching)) {
              cat("This evals is already in this simulation.\n")
            } else {
              sim@evals_refs <- c(sim@evals_refs, ref)
            }
            return(sim)
          })

setMethod("add", signature(sim = "Simulation", ref = "list"),
          function(sim, ref) {
            ref <- unlist(ref)
            classes <- unlist(lapply(ref, class))
            proper_order <- c("ModelRef", "DrawsRef", "OutputRef", "EvalsRef")
            for (cl in proper_order) {
              ii <- which(classes == cl)
              for (i in ii) {
                sim <- add(sim, ref[[i]])
              }
            }
            return(sim)
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
#' @slot model_refs a list of \code{\link{ModelRef}} objects
#' @slot draws_refs a list of \code{\link{DrawsRef}} objects
#' @slot output_refs a list of \code{\link{OutputRef}} objects
#' @slot evals_refs a list of \code{\link{EvalsRef}} objects
#' @export
#' @seealso \code{\link{add}}
new_simulation <- function(name, label, refs = list()) {
  sim <- new("Simulation", name = name, label = label)
  return(add(sim, refs))
}
