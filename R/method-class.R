#' @include component-class.R output-class.R
NULL

check_method <- function(object) {
  errors <- check_component(object)
  if (length(errors) == 1)
    if(errors == TRUE) errors <- character()
    args <- names(formals(object@method))
    str <- "method must be a function with arguments \"model\" and \"draw\"."
    if (length(args) != 2 || any(args != c("model", "draw")))
      errors <- c(errors, str)
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing a method to be run by simulator.
#'
#' An object of class \code{Method} consists of a name, label, and a function
#' \code{method} that takes arguments model and draw. A draw refers
#' to a single element of the list in an object of class \code{\link{Draws}}.
#'
#' This class inherits from the \code{\link{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot method a function with arguments "model" and "draw"
#' @export
setClass("Method", representation(method = "function"),
         contains = "Component", validity = check_method)

