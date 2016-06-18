check_component <- function(object) {
  errors <- character()
  errors <- is_valid_component_name(object@name,
                                    sprintf("%s's name", class(object)))
  if (length(object@label) == 0)
    errors <- c(errors,
                "Missing \"label\" for object. Make this human-readable.")
  if (length(errors) == 0) TRUE else errors
}

is_valid_component_name <- function(name, name_of_name,
                                    require_unique = TRUE,
                                    allow_slash = TRUE) {
  errors <- character()
  if (require_unique) {
    if (length(name) != 1)
      errors <- c(errors,
                  sprintf("%s must be of length 1. Make this something short.",
                          name_of_name))
  }
  if (length(name) > 0) {
    sub_pattern <- paste0("[[:alnum:]]+((_[[:alnum:]]+)*(-[[:alnum:]]+)*",
                          "(\\.[[:alnum:]]+)*(_-[[:alnum:]]+)*)*")
    # ... of form a or a_b or a-b or a.b (or a-b_c, etc) where a, b, c are
    # alphanumeric.  Also, allowed to have a_-b allowing for "param_-1.0"
    if (allow_slash) {
      pattern <- sprintf("^%s(/%s)*$", sub_pattern, sub_pattern)
      # ... of form sub_pattern or sub_pattern/sub_pattern etc
      str <- paste(name_of_name,
                   "must be of form a or a/b etc where a and b are",
                   "alphanumeric strings that can have - or _ or . within.")
      if (!all(grepl(pattern, name)))
        errors <- c(errors, str)
    } else {
      pattern <- sprintf("^%s$", sub_pattern)
      if (!all(grepl(pattern, name)))
      errors <- c(errors,
                  paste0(name_of_name,
                         " must be an alphanumeric string (which can also",
                         " have - or _ within)."))
    }
  }
    errors
}

#' An S4 class representing a component of the simulator.
#'
#' This is a virtual class.
#'
#' @slot name a short name identifier.  Must be alphanumeric.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#'
#' @rdname Component
#' @export
setClass("Component",
         representation(
           name = "character", # shortname identifier
           label = "character", # human readable label
           "VIRTUAL" # this is a virtual class
         ),
         validity = check_component
)

setMethod("show", "Component", function(object) {
  validObject(object)
  cat(paste0(class(object), " Component"), fill = TRUE)
  cat(paste0(" name: ", object@name), fill = TRUE)
  cat(paste0(" label: ", object@label), fill = TRUE)
})
