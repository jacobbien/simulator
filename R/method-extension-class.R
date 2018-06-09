#' @include extended-method-class.R
NULL

check_method_extension <- function(object) {
    errors <- check_component(object)
    errors <- is_valid_component_name(object@name, "name", allow_slash = FALSE)
    if (length(errors) == 1)
      if(errors == TRUE) errors <- character()
    args <- names(formals(object@method_extension))
    str <- paste("method_extension must be a function with arguments",
                 "\"model\", \"draw\", \"out\", and \"base_method\".")
    if (length(args) != 4 || any(args != c("model", "draw", "out",
                                           "base_method")))
      errors <- c(errors, str)
    if (length(errors) == 0) TRUE else errors
  }

#' An S4 class used to create an extended version of a method
#'
#' An object of class \code{MethodExtension} when added to a \code{Method}
#' creates a \code{\linkS4class{ExtendedMethod}}.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot method_extension a function with arguments "model", "draw", "out", and
#'       "base_method".  This will become the function \code{extended_method}
#'       in the \code{ExtendedMethod} object that is created.
#' @export
setClass("MethodExtension",
         representation(method_extension = "function"),
         contains = "Component", validity = check_method_extension)

#' Create an object that can be used to make an extended version of a method
#'
#' Creates an object of class \code{MethodExtension}, which when added to a
#' \code{Method} creates an \code{\linkS4class{ExtendedMethod}}.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param method_extension a function with arguments "model", "draw", "out", and
#'       "base_method".  This will become the function \code{extended_method}
#'       in the \code{ExtendedMethod} object that is created.
#' @export
new_method_extension <- function(name, label, method_extension) {
  new("MethodExtension", name = name, label = label,
      method_extension = method_extension)
}

#' Create an ExtendedMethod from a Method and MethodExtension
#'
#' @param e1 an object of class \code{\linkS4class{Method}}
#' @param e2 an object of class \code{\linkS4class{MethodExtension}}
#' @export
setMethod("+", signature(e1 = "Method", e2 = "MethodExtension"),
          function(e1, e2) {
            new_extended_method(paste0(e1@name, "_", e2@name),
                                paste0(e1@label, " ", e2@label),
                                base_method = e1,
                                extended_method = e2@method_extension)
          })

#' Create an ExtendedMethod from an ExtendedMethod and MethodExtension
#'
#' @param e1 an object of class \code{\linkS4class{ExtendedMethod}}
#' @param e2 an object of class \code{\linkS4class{MethodExtension}}
#' @export
setMethod("+", signature(e1 = "ExtendedMethod", e2 = "MethodExtension"),
          function(e1, e2) {
            new_extended_method(paste0(e1@name, "_", e2@name),
                                paste0(e1@label, " ", e2@label),
                                base_method = e1,
                                extended_method = e2@method_extension)
          })

#' Create a list of ExtendedMethod from a list of Methods and a MethodExtension
#'
#' @param e1 a list of objects of class \code{\linkS4class{Method}} or of class
#' \code{\linkS4class{ExtendedMethod}}
#' @param e2 an object of class \code{\linkS4class{MethodExtension}}
#' @export
setMethod("+", signature(e1 = "list", e2 = "MethodExtension"),
          function(e1, e2) {
            stopifnot(lapply(e1, class) %in% c("Method", "ExtendedMethod"))
            lapply(e1, function(m) {
              new_extended_method(paste0(m@name, "_", e2@name),
                                  paste0(m@label, " ", e2@label),
                                  base_method = m,
                                  extended_method = e2@method_extension)
            })
          })

