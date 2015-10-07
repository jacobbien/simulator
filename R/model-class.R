#' @include component-class.R
NULL

check_model <- function(object) {
  errors <- check_component(object)
  if (length(errors) == 1)
    if(errors == TRUE) errors <- character()
    args <- names(formals(object@simulate))
    if (length(args) != 2 || any(args != c("params", "nsim")))
      errors <- c(errors,
                  "simulate must have arguments \"params\" and \"nsim\".")
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing the model component of the simulator.
#'
#' An object of class Model specifies the statistical model.  In particular,
#' all parameters are specified as is a function that allows one to draw
#' random samples from this model.
#'
#' This class inherits from the \code{\link{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot params a list that contains the Model object's parameters
#' @slot simulate a function that has two arguments, params and nsim (in
#'       that order) and returns a list of length nsim, where each element
#'       of the list represents a random draw from the Model object.
#'
#' @export
setClass("Model", representation(params = "list", simulate = "function"),
         contains = "Component", validity = check_model)

setMethod("show", "Model", function(object) {
  validObject(object)
  callNextMethod()
  if (length(object@params) == 0) cat(" params: empty", fill = TRUE)
  else {
    nams <- names(object@params)
    if (length(nams) <= 20) {
      cat(paste0(" params: ",
                 paste0(nams, collapse = " ")), fill = TRUE)
    } else {
      cat(paste0(sprintf(" params: (%s items) ", length(nams)),
                 paste0(nams[1:5], collapse = " ")),
          " ... ",
          nams[length(nams)],
          fill = TRUE)
    }
  }
})

