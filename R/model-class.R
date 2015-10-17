#' @include component-class.R
NULL

check_model <- function(object) {
  errors <- check_component(object)
  if (length(errors) == 1) {
    if(errors == TRUE)
      errors <- character()
  }
  if ("nsim" %in% names(object@params))
    errors <- c(errors, "\"nsim\" is not an allowed name within \"params\"")
  args <- names(formals(object@simulate))
  str <- "simulate's arguments must be \"nsim\" and those in \"params\""
  if (!all(args %in% c(names(object@params), "nsim")))
    errors <- c(errors, str)
  if (!("nsim" %in% args))
    errors <- c(errors, "simulate must have argument \"nsim\".")
  if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing the model component of the simulator.
#'
#' An object of class Model specifies the statistical model.  In particular,
#' all parameters are specified in addition to a function called \code{simulate}
#' that allows one to draw random samples from this model.
#'
#' To get parameters stored in a \code{Model} object, a shortcut for
#' \code{my_model@@params$my_parameter} is \code{my_model$my_parameter}.
#'
#' This class inherits from the \code{\link{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot params a list that contains the Model object's parameters
#' @slot simulate a function that has arguments \code{nsim} and names
#'       matching elements within \code{names(params)}. It returns a list of
#'       length nsim, where each element of the list represents a random draw
#'       from the \code{Model} object.
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

setMethod("$", "Model", function(x, name) return(x@params[[name]]))
