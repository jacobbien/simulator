#' @include component-class.R output-class.R
NULL

check_metric <- function(object) {
  errors <- check_component(object)
  if (length(errors) == 1)
    if(errors == TRUE) errors <- character()
    args <- names(formals(object@metric))
    str <- "metric must be a function with arguments \"model\" and \"out\"."
    if (length(args) != 2 || any(args != c("model", "out")))
      errors <- c(errors, str)
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing an evaluation metric to be used by simulator.
#'
#' An object of class \code{Metric} consists of a name, label, and a function
#' \code{metric} that takes arguments \code{model} (of class
#' \code{\link{Model}}) and \code{out} (of class \code{\link{Output}}), which
#' is the output of a method.
#'
#' This class inherits from the \code{\link{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot metric a function with arguments "model" and "out"
#' @export
setClass("Metric", representation(metric = "function"),
         contains = "Component", validity = check_metric)

#' Metric giving timing information
#'
#' This is a default metric that applies across all simulations.  Gives the
#' "user" time returned from \code{system.time}.
computing_time <- new("Metric",
                      name = "time",
                      label = "Computing time (s)",
                      metric = function(model, out) return(as.numeric(out$time[1])))
