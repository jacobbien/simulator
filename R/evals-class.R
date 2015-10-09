check_evals <- function(object) {
  errors <- is_valid_name(object@model_name, "model_name")
  errors <- c(errors, is_valid_name(object@method_name, "method_name"))
  errors <- c(errors, is_valid_name(object@metric_name, "metric_name",
                                    require_unique = FALSE))
  if (length(object@index) < 1)
    errors <- c(errors, "index must be of length >= 1.")
  else if (any(object@index != round(object@index)))
    errors <- c(errors, "index must be an integer-valued numeric.")
  if (length(object@metric_label) != length(object@metric_name))
    errors <- c(errors, "metric_label must have same length as metric_name.")
  errors <- c(errors, is_valid_rij_list(object@evals, object@index))
  if (length(errors) == 0) TRUE else errors
}

#' An S4 class representing the evaluation of a metric run by simulator.
#'
#' An object of class \code{Evals} consists of information to identify the
#' model, draws, method, and metric objects this output was derived from. It
#' also has a list called \code{evals}, which is where the output of the metric
#' is stored.  Currently, the labels of all these objects are also included so
#' that plot functions can use human-readable labels without requiring
#' re-loading these.
#'
#' @slot model_name the name of the \code{\link{Model}} object this output is
#'       derived from.
#' @slot model_label the label of the \code{\link{Model}} object this output is
#'       derived from.
#' @slot index the index of the \code{\link{Draws}} object this output is
#'       derived from.
#' @slot method_name the name of the \code{\link{Method}} object this output is
#'       derived from.
#' @slot method_label the label of the \code{\link{Method}} object this output
#'       is derived from.
#' @slot metric_name the name of the \code{\link{Metric}} object this output is
#'       derived from.
#' @slot metric_label the label of the \code{\link{Metric}} object this output
#'       is derived from.
#' @slot evals a named list with each element labeled as \code{ri.j} where
#'       \code{i} is the \code{index} and \code{j} ranges from \code{1} to
#'       \code{nsim}.  Element \code{out$ri.j} is output of metric
#'       \code{metric_name} on random draw \code{ri.j}.
#' @export
setClass("Evals", representation(model_name = "character",
                                 model_label = "character",
                                 index = "numeric",
                                 method_name = "character",
                                 method_label = "character",
                                 metric_name = "character",
                                 metric_label = "character",
                                 evals = "list"),
         validity = check_evals)

setMethod("show", "Evals", function(object) {
  validObject(object)
  cat(paste0("Evals Component"), fill = TRUE)
  cat(sprintf(" model_name: %s    index: %s (%s nsim%s)", object@model_name,
              paste(object@index, collapse = ", "),
              length(object@evals), ifelse(length(object@index) > 1,
                                           " total", "")), fill = TRUE)
  cat(sprintf(" method_name: %s (label: %s)", object@method_name,
              object@method_label), fill = TRUE)
  cat(paste0(" metric_name(s): ", paste(object@metric_name, collapse = ", ")),
      fill = TRUE)
  cat(paste0(" metric_label(s): ", paste(object@metric_label, collapse = ", ")),
      fill = TRUE)
})
