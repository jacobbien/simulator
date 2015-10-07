
check_output <- function(object) {
    errors <- character()
    if (length(object@model_name) != 1)
      errors <- c(errors, "model_name must be of length 1.")
    else if (grepl("[^[:alnum:]]", object@model_name))
      errors <- c(errors, "model_name must be alphanumeric.")
    if (length(object@index) < 1)
      errors <- c(errors, "index must be of length >= 1.")
    else if (any(object@index != round(object@index)))
      errors <- c(errors, "index must be an integer-valued numeric.")
    if (length(object@method_name) != 1)
      errors <- c(errors, "method_name must be of length 1.")
    else if (grepl("[^[:alnum:]]", object@method_name))
      errors <- c(errors, "method_name must be alphanumeric.")
    if (length(object@method_label) != 1)
      errors <- c(errors, "method_label must be of length 1.")
    if (length(object@out) < 1) {
      errors <- c(errors, "out must be nonempty.")
    } else {
      str <- "incorrectly named elements of out. See documentation."
      # should be of format ri.j where i is in index and j starts at 1.
      pattern <- "^r([[:digit:]]+)[.]([[:digit:]]+)$"
      i <- as.numeric(gsub(pattern, "\\1", names(object@out)))
      j <- as.numeric(gsub(pattern, "\\2", names(object@out)))
      if (any(is.na(i))) errors <- c(errors, str)
      if (any(sort(unique(i)) != sort(object@index)))
        errors <- c(errors, "index does not match elements in out list.")
    }
    if (!all(unlist(lapply(object@out, is.list))))
      errors <- c(errors, "out$ri.j should be a list.")
    nams <- lapply(object@out, function(r) sort(names(r)))
    if (!all(unlist(lapply(nams, function(nam) identical(nam, nams[[1]])))))
      errors <- c(errors, "all out$ri.j must have same elements.")
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing the output of a method run by simulator.
#'
#' An object of class \code{Output} consists of a model_name, a method_name, a
#' method_label, and a list of contents.
#'
#' @slot model_name the name of the \code{\link{Model}} object this output is
#'       derived from.  Must be alphanumeric.
#' @slot index the index of the \code{\link{Draws}} object this output is
#'       derived from.  Must be an integer-valued numeric.
#' @slot method_name the name of the \code{\link{Method}} object this output is
#'       derived from.  Must be alphanumeric.
#' @slot method_label the label of the \code{\link{Method}} object this output
#'       is derived from.
#' @slot out a named list with each element labeled as \code{ri.j} where
#'       \code{i} is the \code{index} and \code{j} ranges from \code{1} to
#'       \code{nsim}.  Element \code{out$ri.j} is output of method
#'       \code{method_name} on random draw \code{ri.j}.
#' @export
setClass("Output", representation(model_name = "character",
                                  method_name = "character",
                                  method_label = "character",
                                  index = "numeric",
                                  out = "list"),
         validity = check_output)

setMethod("show", "Output", function(object) {
  validObject(object)
  cat(paste0("Output Component"), fill = TRUE)
  cat(paste0(" model_name: ", object@model_name), fill = TRUE)
  cat(paste0(" index: ", paste(object@index, collapse = ", ")), fill = TRUE)
  if (length(object@index) == 1)
    cat(paste0(" nsim: ", length(object@out)), fill = TRUE)
  else
    cat(paste0(" nsim (total): ", length(object@out)), fill = TRUE)
  cat(paste0(" method_name: ", object@method_name), fill = TRUE)
  cat(paste0(" method_label: ", object@method_label), fill = TRUE)
  cat(paste0(" out: ", paste(names(object@out[[1]]), collapse = ", ")),
      fill = TRUE)
})
