#' @include model-class.R draws-class.R output-class.R evals-class.R

check_modelref <- function(object) {
  errors <- is_valid_component_name(object@name, "name", require_unique = TRUE,
                                    allow_slash = TRUE)
  if (length(object@label) == 0)
    errors <- c(errors,
                "Missing \"label\" for object. Make this human-readable.")
  if (length(errors) == 0) TRUE else errors
}

check_drawsref <- function(object) {
  errors <- is_valid_component_name(object@model_name, "name",
                                    require_unique = TRUE, allow_slash = TRUE)
  if (length(object@index) < 1) errors <- c(errors,
                                            "index must have length >= 1.")
  if (any(round(object@index) != object@index))
    errors <- c(errors, "index must be an integer.")
  if (length(errors) == 0) TRUE else errors
}

check_outputref <- function(object) {
  errors <- is_valid_component_name(object@model_name, "name",
                                    require_unique = TRUE, allow_slash = TRUE)
  errors <- c(errors,
              is_valid_component_name(object@method_name, "method_name",
                                      allow_slash = FALSE))
  if (length(object@index) < 1) errors <- c(errors,
                                            "index must have length >= 1.")
  if (any(round(object@index) != object@index))
    errors <- c(errors, "index must be an integer.")
  if (length(errors) == 0) TRUE else errors
}

check_evalsref <- function(object) {
  errors <- is_valid_component_name(object@model_name, "name",
                                    require_unique = TRUE, allow_slash = TRUE)
  errors <- c(errors,
              is_valid_component_name(object@method_name, "method_name",
                                      allow_slash = FALSE))
  if (length(object@index) != 1) errors <- c(errors,
                                            "index must have length 1.")
  if (any(round(object@index) != object@index))
    errors <- c(errors, "index must be an integer.")
  if (length(errors) == 0) TRUE else errors
}

#' An S4 class representing a reference to an object of class Model.
#'
#' This identifies the necessary information to locate a saved object
#' of class \code{\linkS4class{Model}}.
#'
#' @slot dir directory where the directory "files" is that contains the
#'        referenced \code{\linkS4class{Model}} object
#' @slot name a short name identifier.
#' @slot label a longer, human readable label that can have other characters
#' @slot simulator.files simulator functions will use
#'       \code{getOption("simulator.files")} if simulator.files not provided.
#'
#' @export
setClass("ModelRef", representation(dir = "character", name = "character",
                                    label = "character",
                                    simulator.files = "character"),
         validity = check_modelref)

setMethod("show", "ModelRef", function(object) {
  validObject(object)
  cat("Model Reference", fill = TRUE)
  cat(" name:", object@name, fill = TRUE)
  cat(" label:", object@label, fill = TRUE)
  cat(" dir:", object@dir, fill = TRUE)
  if (object@simulator.files != getOption("simulator.files"))
    cat(" simulator.files:", object@simulator.files, fill = TRUE)
})

#' An S4 class representing a reference to an object of class Draws.
#'
#' This identifies the necessary information to locate a saved object
#' of class \code{\linkS4class{Draws}}.
#'
#' @slot dir directory where the directory \code{getOption("simulator.files")}
#'        is that contains the referenced \code{\linkS4class{Model}} object
#' @slot model_name name of the referenced \code{\linkS4class{Model}} object
#' @slot index the index of the referenced \code{\linkS4class{Draws}} object.  Can
#'        alternately be a vector of such indices.
#' @slot simulator.files simulator functions will use
#'       \code{getOption("simulator.files")} if simulator.files not provided.
#'
#' @export
setClass("DrawsRef", representation(dir = "character", model_name = "character",
                                    index = "numeric",
                                    simulator.files = "character"),
         validity = check_drawsref)

setMethod("show", "DrawsRef", function(object) {
  validObject(object)
  cat("Draws Reference", fill = TRUE)
  cat(" model_name:", object@model_name, fill = TRUE)
  cat(" dir:", object@dir, fill = TRUE)
  cat(paste0(" index: ", paste(object@index, collapse = ", ")), fill = TRUE)
  if (object@simulator.files != getOption("simulator.files"))
    cat(" simulator.files:", object@simulator.files, fill = TRUE)
})

#' An S4 class representing a reference to an object of class Output.
#'
#' This identifies the necessary information to locate a saved object
#' of class \code{\linkS4class{Output}}.
#'
#' @slot dir directory where the directory \code{getOption("simulator.files")}
#'        is that contains the referenced \code{\linkS4class{Model}} object
#' @slot model_name name of the referenced \code{\linkS4class{Model}} object
#' @slot index the index of the referenced \code{\linkS4class{Draws}} object.  Can
#'        alternately be a vector of such indices.
#' @slot method_name the name of the \code{\linkS4class{Method}} object this output is
#'       derived from.
#' @slot out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.
#' @slot simulator.files simulator functions will use
#'       \code{getOption("simulator.files")} if simulator.files not provided.
#'
#' @export
setClass("OutputRef", representation(dir = "character", model_name = "character",
                                    index = "numeric", method_name = "character",
                                    out_loc = "character",
                                    simulator.files = "character"),
         validity = check_outputref)

setMethod("show", "OutputRef", function(object) {
  validObject(object)
  cat("Output Reference", fill = TRUE)
  cat(" model_name:", object@model_name, fill = TRUE)
  cat(" dir:", object@dir, fill = TRUE)
  cat(paste0(" index: ", paste(object@index, collapse = ", ")), fill = TRUE)
  cat(" method_name:", object@method_name, fill = TRUE)
  cat(" out_loc:", object@out_loc, fill = TRUE)
  if (object@simulator.files != options()$simulator.files)
    cat(" simulator.files:", object@simulator.files, fill = TRUE)
})

#' An S4 class representing a reference to an object of class Evals
#'
#' This identifies the necessary information to locate a saved object
#' of class \code{\linkS4class{Evals}}.  Note that \code{metric_names} is not
#' needed to identify an Evals object since Evals objects combine all metrics
#' together into a single file and object.
#'
#' @slot dir directory where the directory \code{getOption("simulator.files")}
#'        is that contains the referenced \code{\linkS4class{Model}} object
#' @slot model_name name of the referenced \code{\linkS4class{Model}} object
#' @slot index the index of the referenced \code{\linkS4class{Draws}} object.
#' @slot method_name the name of the \code{\linkS4class{Method}} object this output is
#'       derived from.
#' @slot out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.
#' @slot simulator.files simulator functions will use
#'       \code{getOption("simulator.files")} if simulator.files not provided.
#'
#' @export
setClass("EvalsRef", representation(dir = "character", model_name = "character",
                                     index = "numeric", method_name = "character",
                                     out_loc = "character",
                                     simulator.files = "character"),
         validity = check_evalsref)

setMethod("show", "EvalsRef", function(object) {
  validObject(object)
  cat("Evals Reference", fill = TRUE)
  cat(" model_name:", object@model_name, fill = TRUE)
  cat(" dir:", object@dir, fill = TRUE)
  cat(paste0(" index: ", paste(object@index, collapse = ", ")), fill = TRUE)
  cat(" method_name:", object@method_name, fill = TRUE)
  cat(" out_loc:", object@out_loc, fill = TRUE)
  if (object@simulator.files != options()$simulator.files)
    cat(" simulator.files:", object@simulator.files, fill = TRUE)
})

