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
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
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
            mnames <- lapply(unlist(sim@model_refs), function(mref) mref@name)
            if (!(ref@model_name %in% mnames))
              stop("Cannot add draws until model named", ref@model_name,
                   "has been added to simulation.")
            # search for list containing DrawsRef objects from same model
            for (i in seq_along(sim@draws_refs)) {
              if (ref@model_name == sim@draws_refs[[i]][[1]]@model_name) {
                sim@draws_refs[[i]] <- add_dref_to_list(ref,
                                                        sim@draws_refs[[i]])
                return(sim)
              }
            }
            # it's the first DrawsRef from this model
            sim@draws_refs <- c(sim@draws_refs, list(list(ref)))
            return(sim)
          })

#' Internal function to add DrawsRef to a list of DrawsRef objects
#'
#' Makes sure that DrawsRef with this same index is not already in list.
#' Although not checked, it is assumed that this is only called on a list
#' of DrawsRef objects all coming from same model.
#'
#' @param dref DrawsRef to add
#' @param dref_list list of DrawsRef objects
add_dref_to_list <- function(dref, dref_list) {
  str <- "DrawsRef with model name %s and index %s is already in simulation."
  if (dref@index %in% lapply(dref_list, function(d) d@index))
    cat(sprintf(str, dref@model_name, dref@index), fill = TRUE)
  c(dref_list, dref)
}

setMethod("add", signature(sim = "Simulation", ref = "OutputRef"),
          function(sim, ref) {
            drefs <- unlist(sim@draws_refs)
            dnames <- lapply(drefs, function(dref) dref@model_name)
            dindex <- lapply(drefs, function(dref) dref@index)
            if (!any(dnames == ref@model_name & dindex == ref@index))
              stop("Cannot add output until draws with name ", ref@model_name,
                   " and index ", ref@index,
                   " has been added to simulation.")
            # search for list containing OutputRef objects from same model
            for (i in seq_along(sim@output_refs)) {
              if (ref@model_name == sim@output_refs[[i]][[1]]@model_name) {
                sim@output_refs[[i]] <- add_oref_to_list(ref,
                                                        sim@output_refs[[i]])
                return(sim)
              }
            }
            # it's the first OutputRef from this model
            sim@output_refs <- c(sim@output_refs, list(list(ref)))
            return(sim)
          })

#' Internal function to add OutputRef to a list of OutputRef objects
#'
#' Makes sure that OutputRef with this same index and method is not already in
#' list. Although not checked, it is assumed that this is only called on a list
#' of OutputRef objects all coming from same model.
#'
#' @param oref OutputRef to add
#' @param oref_list list of OutputRef objects
add_oref_to_list <- function(oref, oref_list) {
  str <- "OutputRef with model name %s, index %s, and method %s is already in simulation."
  same_index <- lapply(oref_list, function(o) o@index) == oref@index
  same_method <- lapply(oref_list,
                        function(o) o@method_name) == oref@method_name
  if (any(same_index & same_method))
    cat(sprintf(str, oref@model_name, oref@index, oref@method_name),
        fill = TRUE)
  c(oref_list, oref)
}

setMethod("add", signature(sim = "Simulation", ref = "EvalsRef"),
          function(sim, ref) {
            orefs <- unlist(sim@output_refs)
            onames <- lapply(orefs, function(oref) oref@model_name)
            oindex <- lapply(orefs, function(oref) oref@index)
            omethod <- lapply(orefs, function(oref) oref@method_name)
            match <- onames == ref@model_name & oindex == ref@index
            match <- match & omethod == ref@method_name
            if (!any(match))
              stop("Cannot add eval until output with model_name ",
                   ref@model_name,
                   " and index ", ref@index,
                   " and method_name ", ref@method_name,
                   " has been added to simulation.")
            # search for list containing EvalsRef objects from same model
            for (i in seq_along(sim@evals_refs)) {
              if (ref@model_name == sim@evals_refs[[i]][[1]]@model_name) {
                sim@evals_refs[[i]] <- add_eref_to_list(ref,
                                                         sim@evals_refs[[i]])
                return(sim)
              }
            }
            # it's the first EvalsRef from this model
            sim@evals_refs <- c(sim@evals_refs, list(list(ref)))
            return(sim)
          })

#' Internal function to add EvalsRef to a list of EvalsRef objects
#'
#' Makes sure that EvalsRef with this same index and method is not already in
#' list. Although not checked, it is assumed that this is only called on a list
#' of EvalsRef objects all coming from same model.
#'
#' @param oref EvalsRef to add
#' @param oref_list list of EvalsRef objects
add_eref_to_list <- function(eref, eref_list) {
  str <- "EvalsRef with model name %s, index %s, and method %s is already in simulation."
  same_index <- lapply(eref_list, function(e) e@index) == eref@index
  same_method <- lapply(eref_list,
                        function(e) e@method_name) == eref@method_name
  if (any(same_index & same_method))
    cat(sprintf(str, eref@model_name, eref@index, eref@method_name),
        fill = TRUE)
  c(eref_list, eref)
}


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
