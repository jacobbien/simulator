#' @include simulation-class.R
NULL

#' Add a reference to a simulation
#'
#' Adds a ModelRef, DrawsRef, OutputRef, or EvalsRef to a simulation object.
#' To add a DrawsRef, the corresponding ModelRef must already be added.
#' Likewise, to add an OutputRef, the corresponding DrawsRef must already be
#' added.  And to add an EvalsRef, the corresponding OutputRef must be added.
#' One can also pass a list of such objects.
#'
#' The modified simulation object is saved to file if \code{update_saved} is
#' TRUE.
#'
#' @param sim simulation being added to
#' @param ref the reference object being added
#' @param update_saved default is TRUE. Determines whether change to simulation
#'        object should be saved to file
#' @param ... not used
#'
#' @export
add <- function(sim, ref, ...) {
  stop("add not implemented for this class of ref.")
}

setGeneric("add")

#' @rdname add
setMethod("add", signature(sim = "Simulation", ref = "ModelRef"),
          function(sim, ref, update_saved = TRUE) {
            mnames <- lapply(sim@model_refs, function(ref) ref@name)
            if (ref@name %in% mnames) {
              stop("A model of this name is already in this simulation.")
            } else {
              ref@dir <- get_relative_path(sim@dir, ref@dir)
              # change ref@dir so that file.path(sim@dir, ref@dir) is location
              # of file
              sim@model_refs <- c(sim@model_refs, ref)
            }
            if (update_saved) save_simulation(sim)
            return(sim)
          })

#' @rdname add
setMethod("add", signature(sim = "Simulation", ref = "DrawsRef"),
          function(sim, ref, update_saved = TRUE) {
            mnames <- lapply(unlist(sim@model_refs), function(mref) mref@name)
            if (!(ref@model_name %in% mnames))
              stop("Cannot add draws until model named ", ref@model_name,
                   " has been added to simulation.")
            # search for list containing DrawsRef objects from same model
            for (i in seq_along(sim@draws_refs)) {
              if (ref@model_name == sim@draws_refs[[i]][[1]]@model_name) {
                sim@draws_refs[[i]] <- add_dref_to_list(ref,
                                                        sim@draws_refs[[i]],
                                                        sim@dir)
                if (update_saved) save_simulation(sim)
                return(sim)
              }
            }
            # it's the first DrawsRef from this model
            ref@dir <- get_relative_path(sim@dir, ref@dir)
            sim@draws_refs <- c(sim@draws_refs, list(list(ref)))
            if (update_saved) save_simulation(sim)
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
#' @param sim_dir sim@@dir
#' @keywords internal
add_dref_to_list <- function(dref, dref_list, sim_dir) {
  # change dref@dir so that file.path(sim@dir, dref@dir) is location
  # of file
  dref@dir <- get_relative_path(sim_dir, dref@dir)
  if (any(unlist(lapply(dref_list, function(d) identical(dref, d)))))
    return(dref_list) # don't add because dref is already in dref_list
  c(dref_list, dref)
}

#' @rdname add
setMethod("add", signature(sim = "Simulation", ref = "OutputRef"),
          function(sim, ref, update_saved = TRUE) {
            drefs <- unlist(sim@draws_refs)
            dnames_match <- lapply(drefs,
                                   function(dref)
                                     dref@model_name == ref@model_name)
            dindex_match <- lapply(drefs,
                                   function(dref)
                                     all(ref@index %in% dref@index))
            if (!any(unlist(dnames_match) & unlist(dindex_match)))
              stop("Cannot add output until draws with name ", ref@model_name,
                   " and index ", ref@index,
                   " has been added to simulation.")
            # search for list containing OutputRef objects from same model
            for (i in seq_along(sim@output_refs)) {
              if (ref@model_name == sim@output_refs[[i]][[1]]@model_name) {
                sim@output_refs[[i]] <- add_oref_to_list(ref,
                                                         sim@output_refs[[i]],
                                                         sim@dir)
                if (update_saved) save_simulation(sim)
                return(sim)
              }
            }
            # it's the first OutputRef from this model
            ref@dir <- get_relative_path(sim@dir, ref@dir)
            sim@output_refs <- c(sim@output_refs, list(list(ref)))
            if (update_saved) save_simulation(sim)
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
#' @param sim_dir sim@@dir
#' @keywords internal
add_oref_to_list <- function(oref, oref_list, sim_dir) {
  # change oref@dir so that file.path(sim@dir, oref@dir) is location
  # of file
  oref@dir <- get_relative_path(sim_dir, oref@dir)
  if (any(unlist(lapply(oref_list, function(o) identical(oref, o)))))
    return(oref_list) # don't add because oref is already in oref_list
  c(oref_list, oref)
}

#' @rdname add
setMethod("add", signature(sim = "Simulation", ref = "EvalsRef"),
          function(sim, ref, update_saved = TRUE) {
            orefs <- unlist(sim@output_refs)
            onames <- lapply(orefs, function(oref) oref@model_name)
            oindex <- lapply(orefs, function(oref) oref@index)
            omethod <- lapply(orefs, function(oref) oref@method_name)
            match <- onames == ref@model_name & oindex == ref@index
            match <- match & omethod == ref@method_name
            if (!any(match)) {
              stop("Cannot add eval until output with model_name ",
                   ref@model_name,
                   " and index ", ref@index,
                   " and method_name ", ref@method_name,
                   " has been added to simulation.")
            }
            # search for list containing EvalsRef objects from same model
            for (i in seq_along(sim@evals_refs)) {
              if (ref@model_name == sim@evals_refs[[i]][[1]]@model_name) {
                sim@evals_refs[[i]] <- add_eref_to_list(ref,
                                                        sim@evals_refs[[i]],
                                                        sim@dir)
                if (update_saved) save_simulation(sim)
                return(sim)
              }
            }
            # it's the first EvalsRef from this model
            ref@dir <- get_relative_path(sim@dir, ref@dir)
            sim@evals_refs <- c(sim@evals_refs, list(list(ref)))
            if (update_saved) save_simulation(sim)
            return(sim)
          })

#' Internal function to add EvalsRef to a list of EvalsRef objects
#'
#' Makes sure that EvalsRef with this same index and method is not already in
#' list. Although not checked, it is assumed that this is only called on a list
#' of EvalsRef objects all coming from same model.
#'
#' @param eref EvalsRef to add
#' @param eref_list list of EvalsRef objects
#' @param sim_dir sim@@dir
#' @keywords internal
add_eref_to_list <- function(eref, eref_list, sim_dir) {
  # change eref@dir so that file.path(sim@dir, eref@dir) is location
  # of file
  eref@dir <- get_relative_path(sim_dir, eref@dir)
  if (any(unlist(lapply(eref_list, function(e) identical(eref, e)))))
    return(eref_list) # don't add because eref is already in eref_list
  c(eref_list, eref)
}

#' @rdname add
setMethod("add", signature(sim = "Simulation", ref = "list"),
          function(sim, ref, update_saved = TRUE) {
            ref <- unlist(ref)
            classes <- unlist(lapply(ref, class))
            proper_order <- c("ModelRef", "DrawsRef", "OutputRef", "EvalsRef")
            if (!all(classes %in% proper_order))
              stop("Cannot add an object of class ", 
                   classes[which(!(classes %in% proper_order))[1]],
                   " to simulation.")
            for (cl in proper_order) {
              ii <- which(classes == cl)
              for (i in ii) {
                sim <- add(sim, ref[[i]], update_saved = update_saved)
              }
            }
            return(sim)
          })
