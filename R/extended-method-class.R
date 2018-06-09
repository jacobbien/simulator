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
  if (length(object@base_method) != 1)
    errors <- c(errors, "base_method must be a list of length 1")
  if (!(class(object@base_method[[1]]) %in% c("Method", "ExtendedMethod"))) {
    str <- "base_method[[1]] must be of class Method or ExtendedMethod"
    errors <- c(errors, str)
  }
  if (length(errors) == 0) TRUE else errors
}

#setClassUnion("Method_or_ExtendedMethod", c("Method", "ExtendedMethod"))

#' An S4 class representing the extension of a method
#'
#' An object of class \code{ExtendedMethod} is like a
#' \code{\linkS4class{Method}} except it uses the output of another method in
#'  addition to the \code{\linkS4class{Model}} and
#'  \code{\linkS4class{Draws}}.  We can also form chains of
#'  \code{ExtendedMethod}'s, in which one \code{ExtendedMethod} is taken to be
#'  the "\code{base_method}" of a subsequent \code{ExtendedMethod}.  This means
#'  that the latter \code{ExtendedMethod} would use the output of the former
#'  \code{ExtendedMethod}.
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
#' For an example in which one has a chain of \code{ExtendedMethod}'s, consider
#' the lasso example in which we have a \code{MethodExtension} called, say,
#' \code{refit}, which takes the nonzeros from the lasso's output and
#' performs least squares on these selected variables.  Let \code{cv} be another
#' \code{MethodExtension}.  Then, \code{refitted_lasso = lasso + refit} is
#' an \code{ExtendedMethod} and \code{refitted_lasso + cv} is as well.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot base_method a list of length 1 containing the object of class
#'       \code{\linkS4class{Method}} or \code{\linkS4class{ExtendedMethod}}
#'       that is being extended
#' @slot extended_method a function with arguments "model", "draw", "out", and
#'       "base_method".
#' @export
setClass("ExtendedMethod",
         #representation(base_method = "Method_or_ExtendedMethod",
         # this doesn't work because it depends on ExtendedMethod being defined
         representation(base_method = "list", # this must be of length one and
         #  contain either a Method or an ExtendedMethod
         extended_method = "function"),
         contains = "Component", validity = check_extended_method)

#' Create an ExtendedMethod object
#'
#' Creates a new \code{\linkS4class{ExtendedMethod}} object.
#'
#' @param name a short name identifier.  Must be alphanumeric.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param base_method the object of class \code{\linkS4class{Method}} or
#'        of class \code{\linkS4class{Method}} that is being extended
#' @param extended_method a function with arguments "model", "draw", "out", and
#'       "base_method".
#' @export
new_extended_method <- function(name, label, base_method, extended_method) {
  new("ExtendedMethod", name = name, label = label,
      base_method = list(base_method),
      extended_method = extended_method)
}
