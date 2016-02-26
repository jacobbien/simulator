#' @include reference-classes.R
NULL

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
#' to a single table.  Note that a Simulation object is light-weight even for
#' large simulations because it only stores references to the objects
#' not the objects themselves.  The functions \code{\link{model}},
#' \code{\link{draws}}, \code{\link{output}}, \code{\link{evals}} can be used
#' to load individual objects of a simulation.
#'
#' The Simulation object created is saved to a file so that it can be loaded
#' in a new R session.  The simulation is saved in dir/files/name.Rdata. Note:
#' while "files" is the default, the name of this directory is from
#' getOption("simulator.files"), which is the value of
#' getOption("simulator.files") when the model was created.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param dir a directory that reference's directories are relative to
#' @param model_refs a list of \code{\link{ModelRef}} objects
#' @param draws_refs a list of \code{\link{DrawsRef}} objects
#' @param output_refs a list of \code{\link{OutputRef}} objects
#' @param evals_refs a list of \code{\link{EvalsRef}} objects
#' @param save_to_file whether this new simulation should be saved to file.
#'        Default is TRUE. If TRUE, then this simulation can be loaded
#'        in a new R session using \code{dir} and \code{name}.
#' @export
#' @seealso \code{\link{add}} \code{\link{load_simulation}}
new_simulation <- function(name, label, dir = ".", refs = list(),
                           save_to_file = TRUE) {
  sim <- new("Simulation", name = name, label = label, dir = dir)
  sim <- add(sim, refs, update_saved = save_to_file)
  if (save_to_file) save_simulation(sim)
  return(sim)
}

#' Save a simulation object
#'
#' Saves an object of class \code{\link{Simulation}} to
#' sim@@dir/files/sim@@name.Rdata. Note: while "files" is the default, the name
#' of this directory is from getOption("simulator.files"), which is the value of
#' getOption("simulator.files") when the model was created.
#'
#' This function overwrites any pre-existing file in that location without
#' apology.
#'
#' @param sim an object of class \code{\link{Simulation}}
#' @export
#' @seealso \code{\link{load_simulation}}
save_simulation <- function(sim) {
  files_dir <- file.path(sim@dir, getOption("simulator.files"))
  if (!file.exists(files_dir)) dir.create(files_dir, recursive = TRUE)
  file <- sprintf("%s/sim-%s.Rdata", files_dir, sim@name)
  save(sim, file = file)
}

#' Load a simulation object
#'
#' Loads an object of class \code{\link{Simulation}}.  Note that \code{dir}
#' gives the directory where the Simulation object is stored.  Thus, if the
#' working directory is different from the working directory when the Simulation
#' object was created, then \code{dir} will be different from the one passed to
#' \code{\link{new_simulation}}.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param dir directory that contains "files" directory for this simulation
#' @export
#' @seealso \code{\link{new_simulation}}
load_simulation <- function(name, dir = ".") {
  files_dir <- file.path(dir, getOption("simulator.files"))
  file <- sprintf("%s/sim-%s.Rdata", files_dir, name)
  load(file)
  return(sim)
}
