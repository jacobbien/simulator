#' @include model-class.R
NULL

check_draws <- function(object) {
  errors <- check_component(object)
  if (length(errors) == 1)
    if(errors == TRUE) errors <- character()
    if (length(object@index) < 1) errors <- c(errors,
                                               "index must have length >= 1.")
    if (any(round(object@index) != object@index)) errors <- c(errors,
                                               "index must be an integer.")

    if (length(object@draws) < 1) {
      errors <- c(errors, "draws must be nonempty.")
    } else {
      str <- "incorrectly named elements of draws See documentation."
      # should be of format ri.j where i is in index and j starts at 1.
      pattern <- "^r([[:digit:]]+)[.]([[:digit:]]+)$"
      i <- as.numeric(gsub(pattern, "\\1", names(object@draws)))
      j <- as.numeric(gsub(pattern, "\\2", names(object@draws)))
      if (any(is.na(i))) errors <- c(errors, str)
      if (any(sort(unique(i)) != sort(object@index)))
        errors <- c(errors, "index does not match elements in draws list.")
    }
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing the random draws from a Model object.
#'
#' An object of class Draws represents the randomly drawn simulated data that is
#' generated when \code{\link{simulate_from_model}} is called on an object of
#' class Model.  In particular, it contains a named list of \code{nsim}
#' simulated draws from a model object.  The Model object's \code{simulate}
#' function populates this list.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric. Should use
#'       the name of the Model object that generated it.
#' @slot label a longer, human readable label that indicates what has been
#'       randomly drawn.
#' @slot draws a list with \code{nsim} elements as created by calling
#'       the \code{simulate} function of a Model object. This is a named
#'       list with each element labeled as \code{ri.j} where \code{i} is the
#'       \code{index} and \code{j} ranges from \code{1} to \code{nsim}.  The
#'       names are assigned by \code{\link{simulate_from_model}}.
#' @slot index an integer-valued numeric that indicates which block of random
#'       draws this is
#'
#' @export
setClass("Draws", representation(index = "numeric", draws = "list"),
         contains = "Component", validity = check_draws)

setMethod("show", "Draws", function(object) {
  validObject(object)
  callNextMethod()
  catsim(" (Add @draws to end of this object to see what was simulated.)",
         fill = TRUE)
  })
