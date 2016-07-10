#' @include component-class.R output-class.R
NULL

check_method <- function(object) {
  errors <- check_component(object)
  errors <- is_valid_component_name(object@name, "name", allow_slash = FALSE)
  if (length(errors) == 1)
    if(errors == TRUE) errors <- character()
    args <- names(formals(object@method))
    str <- "method must be a function with arguments \"model\" and \"draw\"."
    if (!all((c("model", "draw") %in% args)))
      errors <- c(errors, str)
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing a method to be run by simulator.
#'
#' An object of class \code{Method} consists of a name, label, and a function
#' \code{method} that takes arguments model and draw. A draw refers
#' to a single element of the list in an object of class \code{\linkS4class{Draws}}.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot settings (optional) a list of "settings" for the method (e.g., tuning
#'       parameters or related information that might distinguish two otherwise
#'       identical methods).
#' @slot method a function that has arguments "model", "draw"
#'       and (optionally) names matching elements within \code{names(settings)}
#' @export
setClass("Method", representation(settings = "list", method = "function"),
         contains = "Component", validity = check_method)

#' Create a Method object
#'
#' Creates a new \code{\linkS4class{Method}} object.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param method a function that has arguments "model", "draw"
#'       and (optionally) names matching elements within \code{names(settings)}
#' @param settings (optional) a list of "settings" for the method (e.g., tuning
#'       parameters or related information that might distinguish two otherwise
#'       identical methods).
#' @export
new_method <- function(name, label, method, settings = list()) {
  new("Method", name = name, label = label, settings = settings, method = method)
}

setMethod("show", "Method", function(object) {
  validObject(object)
  callNextMethod()
  catsim(" (Add @method to end of this object to see function.)",
         fill = TRUE)
  if (length(object@settings) > 0)
    catsim(" (Add @settings to end of this object to see list of settings.)",
           fill = TRUE)
})
