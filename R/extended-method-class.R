#' @include method-class.R
NULL

check_extended_method <- function(object) {
  errors <- check_component(object)
  errors <- is_valid_component_name(object@name, "name", allow_slash = FALSE)
  if (length(errors) == 1)
    if(errors == TRUE) errors <- character()
  args <- names(formals(object@extended_method))
  str <- paste("extended_method must be a function with arguments",
               "\"model\", \"draw\", \"out\", and \"base_method\".")
  if (length(args) != 4 || any(args != c("model", "draw", "out",
                                         "base_method")))
    errors <- c(errors, str)
  if (length(errors) == 0) TRUE else errors
}

#' An S4 class representing the extension of a method
#'
#' An object of class \code{ExtendedMethod} is like a
#' \code{\linkS4class{Method}} except it uses the output of another method in
#'  addition to the \code{\linkS4class{Model}} and
#'  \code{\linkS4class{Draws}}.
#'
#' While one can create an \code{\linkS4class{ExtendedMethod}} from scratch,
#' typically it will be cleaner to write a \code{MethodExtension} object
#' and then use the addition operator:
#' \code{my_extended_method = my_base_method + my_method_extension}. For
#' example, if \code{my_base_method} is the lasso, \code{my_method_extension}
#' might be cross-validation, and the resulting \code{my_extended_method} would
#' be the lasso with tuning parameter chosen by cross-validation.  The advantage
#' is that if we have several methods, we only have to write the
#' cross-validation \code{MethodExtension} object once.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot base_method the object of class \code{\linkS4class{Method}} that is
#'       being extended
#' @slot extended_method a function with arguments "model", "draw", "out", and
#'       "base_method".
#' @export
setClass("ExtendedMethod",
         representation(base_method = "Method", extended_method = "function"),
         contains = "Component", validity = check_extended_method)

#' Create an ExtendedMethod object
#'
#' Creates a new \code{\linkS4class{ExtendedMethod}} object.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param base_method the object of class \code{\linkS4class{Method}} that is
#'        being extended
#' @param extended_method a function with arguments "model", "draw", "out", and
#'       "base_method".
#' @export
new_extended_method <- function(name, label, base_method, extended_method) {
  new("ExtendedMethod", name = name, label = label, base_method = base_method,
      extended_method = extended_method)
}
