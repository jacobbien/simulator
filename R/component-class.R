check_component <- function(object) {
  errors <- character()
  if (length(object@name) == 0)
    errors <- c(errors,
                "Missing name for object. Make this something short.")
  if (length(object@name) > 1)
    errors <- c(errors,
                "Object must have a single name. Make this something short.")
  if (length(object@label) == 0)
    errors <- c(errors,
                "Missing \"label\" for object. Make this human-readable.")
  if (length(object@name) == 1) if (grepl("[^[:alnum:]]", object@name))
    errors <- c(errors, "Object name must be alphanumeric.")
  if (length(errors) == 0) TRUE else errors
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
