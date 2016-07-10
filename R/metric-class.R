#' @include component-class.R output-class.R
NULL

check_metric <- function(object) {
  errors <- check_component(object)
  name_is_alphanumeric <- grepl("^[[:alnum:]_]+$", object@name)
  if (!name_is_alphanumeric)
    errors <- c(errors,
                "metric name must be alphanumeric (and can have underscores).")
  if (length(errors) == 1)
    if(errors == TRUE) errors <- character()
    args <- names(formals(object@metric))
    str <- paste("metric must be a function with arguments \"model\" and",
                 "\"out\" (and optionally \"draw\")")
    if (length(args) == 2) {
      if (!all(sort(args) == c("model", "out")))
        errors <- c(errors, str)
    }
    else if (length(args) == 3) {
      if (!all(sort(args) == c("draw", "model", "out")))
        errors <- c(errors, str)
    }
    else
      errors <- c(errors, str)
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing an evaluation metric to be used by simulator.
#'
#' An object of class \code{Metric} consists of a name, label, and a function
#' \code{metric} that takes arguments \code{model} (of class
#' \code{\linkS4class{Model}}) and \code{out} (of class \code{\linkS4class{Output}}), which
#' is the output of a method.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot metric a function with arguments "model" and "out" (and optionally
#'        "draw")
#' @export
setClass("Metric", representation(metric = "function"),
         contains = "Component", validity = check_metric)

#' Create a Metric object
#'
#' Creates a new \code{\linkS4class{Metric}} object.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param metric a function with arguments "model" and "out" (and optionally
#'        "draw")
#' @export
new_metric <- function(name, label, metric) {
  new("Metric", name = name, label = label, metric = metric)
}

setMethod("show", "Metric", function(object) {
  validObject(object)
  callNextMethod()
  catsim(" (Add @metric to end of this object to see function.)",
         fill = TRUE)
})
