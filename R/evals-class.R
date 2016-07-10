check_evals <- function(object) {
  errors <- is_valid_component_name(object@model_name, "model_name")
  errors <- c(errors, is_valid_component_name(object@method_name,
                                              "method_name",
                                              require_unique = FALSE))
  errors <- c(errors, is_valid_component_name(object@metric_name,
                                              "metric_name",
                                              require_unique = FALSE))
  if (length(object@index) < 1)
    errors <- c(errors, "index must be of length >= 1.")
  else if (any(object@index != round(object@index)))
    errors <- c(errors, "index must be an integer-valued numeric.")
  if (length(object@method_label) != length(object@method_name))
    errors <- c(errors, "method_label must have same length as method_name.")
  if (length(object@metric_label) != length(object@metric_name))
    errors <- c(errors, "metric_label must have same length as metric_name.")
  if (length(object@evals) != length(object@method_name)) {
    errors <- c(errors, "length of object@evals must be same as method_name")
    if (any(names(object@evals) != object@method_name))
      errors <- c(errors, "names(object@evals) must match method_name")
  }
  for (m in seq_along(object@method_name)) {
    errors <- c(errors, is_valid_rij_list(object@evals[[m]], object@index))
    for (r in seq_along(object@evals[[m]])) {
      nams <- names(object@evals[[m]][[r]])
      if (length(nams) != length(object@metric_name))
        errors <- c(errors, paste0("length of metric_name does not match ",
                                   "length of object@evals[[m]][[r]]"))
      if (any(nams != object@metric_name))
        errors <- c(errors, paste0("metric_name does not match ",
                                   "names(object@evals[[m]][[r]])"))
    }
  }
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
#' @slot model_name the name of the \code{\linkS4class{Model}} object this output is
#'       derived from.
#' @slot model_label the label of the \code{\linkS4class{Model}} object this output is
#'       derived from.
#' @slot index the index of the \code{\linkS4class{Draws}} object this output is
#'       derived from.
#' @slot method_name the name of the \code{\linkS4class{Method}} object this output is
#'       derived from.
#' @slot method_label the label of the \code{\linkS4class{Method}} object this output
#'       is derived from.
#' @slot metric_name the name of the \code{\linkS4class{Metric}} object this output is
#'       derived from.
#' @slot metric_label the label of the \code{\linkS4class{Metric}} object this output
#'       is derived from.
#' @slot evals a named list with each element labeled by a method_name
#'       each evals[[m]] is itself a named list with each element labeled
#'       as \code{ri.j} where \code{i} is the \code{index} and \code{j} ranges
#'       from \code{1} to \code{nsim}.  Element \code{out$ri.j} is output of
#'       metric \code{metric_name} on random draw \code{ri.j}.
#' @seealso \code{\link{evaluate}} \code{\link{as.data.frame.Evals}}
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
              length(object@evals[[1]]), ifelse(length(object@index) > 1,
                                           " total", "")), fill = TRUE)
  cat(sprintf(" method_name(s): %s (labeled: %s)",
              paste(object@method_name, collapse = ", "),
              paste(object@method_label, collapse = ", ")), fill = TRUE)
  cat(paste0(" metric_name(s): ", paste(object@metric_name, collapse = ", ")),
      fill = TRUE)
  cat(paste0(" metric_label(s): ", paste(object@metric_label, collapse = ", ")),
      fill = TRUE)
  catsim(" (Add @evals to end of this object or use as.data.frame to see more.)",
         fill = TRUE)
})

#' Convert an Evals to a data.frame
#'
#' This is equivalent to calling \code{as(x, "data.frame")}
#' @param x object of class \code{\linkS4class{Evals}}
#' @param row.names not used
#' @param optional not used
#' @param ... not used
#' @export
as.data.frame.Evals <- function(x, row.names = NULL, optional = FALSE, ...)
  as(x, "data.frame")

setAs(from = "Evals", to = "data.frame",
      def = function(from) {
        num_methods <- length(from@method_name)
        for (m in seq(num_methods)) {
          list_of_df <- lapply(from@evals[[m]], as.data.frame)
          num_sim <- length(from@evals[[m]])
          name_of_draw <- names(from@evals[[m]])
          dfm <- cbind(Draw = name_of_draw[1], list_of_df[[1]])
          if (num_sim > 1) {
            for (r in seq(2, num_sim)) {
              dfm <- rbind(dfm,
                          cbind(Draw = name_of_draw[r], list_of_df[[r]]))
            }
          }
          dfm <- cbind(Model = from@model_name,
                       Method = from@method_name[m],
                       dfm)
          if (m == 1)
            df <- dfm
          else
            df <- rbind(df, dfm)
        }
        names(df)[-(1:3)] <- from@metric_name
        df
        })

#' Convert a list of Evals to a data.frame
#'
#' When \code{\link{load}} generates a list of Evals, it assigns this
#' to be of (S3) class listofEvals, inherited from list, so that this function
#' will be invoked instead of as.data.frame.list, which is defined in base.
#'
#' @param x a listofEvals object
#' @param row.names not used
#' @param optional not used
#' @param ... not used
#' @export
as.data.frame.listofEvals <- function(x, row.names = NULL, optional = FALSE, ...)
  do.call("rbind", lapply(x, as.data.frame))
