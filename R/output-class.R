is_valid_rij_list <- function(out, index) {
  if (length(out) < 1) {
    errors <- c(errors, "out must be nonempty.")
  } else {
    str <- "incorrectly named elements of out. See documentation."
    # should be of format ri.j where i is in index and j starts at 1.
    pattern <- "^r([[:digit:]]+)[.]([[:digit:]]+)$"
    i <- as.numeric(gsub(pattern, "\\1", names(out)))
    j <- as.numeric(gsub(pattern, "\\2", names(out)))
    if (any(is.na(i))) errors <- c(errors, str)
    if (any(sort(unique(i)) != sort(index)))
      errors <- c(errors, "index does not match elements in out list.")
  }
  if (!all(unlist(lapply(out, is.list))))
    errors <- c(errors, "out$ri.j should be a list.")
  nams <- lapply(out, function(r) sort(names(r)))
  if (!all(unlist(lapply(nams, function(nam) identical(nam, nams[[1]])))))
    errors <- c(errors, "all out$ri.j must have same elements.")
}

check_output <- function(object) {
    errors <- is_valid_component_name(object@model_name, "model_name")
    errors <- c(errors,
                is_valid_component_name(object@method_name, "method_name",
                                        allow_slash = FALSE))

    if (length(object@index) < 1)
      errors <- c(errors, "index must be of length >= 1.")
    else if (any(object@index != round(object@index)))
      errors <- c(errors, "index must be an integer-valued numeric.")
    if (length(object@method_label) != 1)
      errors <- c(errors, "method_label must be of length 1.")
    errors <- c(errors, is_valid_rij_list(object@out, object@index))
    if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing the output of a method run by simulator.
#'
#' An object of class \code{Output} consists of information to identify the
#' model, draws, and method objects this output was derived from.  It also has
#' a list called \code{out}, which is where the output of the method is stored.
#'
#' @slot model_name the name of the \code{\linkS4class{Model}} object this output is
#'       derived from.
#' @slot index the index of the \code{\linkS4class{Draws}} object this output is
#'       derived from.
#' @slot method_name the name of the \code{\linkS4class{Method}} object this output is
#'       derived from.
#' @slot method_label the label of the \code{\linkS4class{Method}} object this output
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
  catsim(" (Add @out to end of this object to see what method(s) returned.)",
         fill = TRUE)
})
