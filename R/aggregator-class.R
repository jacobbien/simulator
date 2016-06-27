#' @include evals-class.R
NULL

check_aggregator <- function(object) {
  errors <- character()
  if (length(object@label) == 0)
    errors <- c(errors,
                "Missing \"label\" for object. Make this human-readable.")
  args <- names(formals(object@aggregate))
  str <- "aggregate must be a function with argument \"ev\""
  if (length(args) == 1) {
    if (args != "ev")
      errors <- c(errors, str)
  } else if (length(args) != 1) {
    errors <- c(errors, str)
  }
  if (length(errors) == 0) TRUE else errors
}


#' An S4 class for aggregating evaluated metrics
#'
#' An object of class \code{Aggregator} consists of a label and a function
#' \code{aggregate} that has a single argument \code{ev} that is a list of
#' length \code{nsim}.  This list consists of the evaluated values of a single
#' metric on a single method for a single model.
#'
#' @slot label a human readable label that will be a prefix to the Eval's label
#' @slot aggregate a function with argument \code{ev} that is a list of
#' length \code{nsim} and returns a scalar.
#' @export
setClass("Aggregator",
         representation(label = "character", aggregate = "function"),
         validity = check_aggregator)

#' Create an Aggregator object
#'
#' Creates a new \code{\linkS4class{Aggregator}} object.
#'
#' @param label a human readable label
#' @param aggregate a function with argument \code{ev} that is a list of
#'        length \code{nsim} with each element itself being a named list. Each
#'        element of this list corresponds to a metric that has been computed.
#'        In particular, given an \code{\linkS4class{Evals}} object \code{o},
#'        \code{aggregate} takes as input \code{o@@evals[[method_name]]} (which
#'        is a list of the kind just described).
#'        The function aggregate should return a scalar.
#' @export
new_aggregator <- function(label, aggregate) {
  new("Aggregator", label = label, aggregate = aggregate)
}

make_scalar_aggregator <- function(label, metric_name, metric_label, fun) {
  new_aggregator(label = paste(label, metric_label),
                 aggregate = function(ev) {
                   if (!(metric_name %in% names(ev[[1]])))
                     return(NA)
                   e <- lapply(ev, function(aa) aa[[metric_name]])
                   if (all(lapply(e, length) == 1))
                     return(fun(unlist(e)))
                   stop("This aggregator only defined for scalar-",
                        "valued metrics")
                   })
}

#' Apply aggregator to a list of Evals objects
#'
#' Returns a num_models by num_methods matrix
#'
#' @param evals_list a list of Evals objects
#' @param aggregator object of class Aggregator
aggregate_evals <- function(evals_list, aggregator) {
  num_models <- length(evals_list)
  method_names <- unique(unlist(lapply(evals_list,
                                       function(e) e@method_name)))
  ag <- matrix(NA, num_models, length(method_names))
  for (i in seq(num_models)) {
    for (method_name in evals_list[[i]]@method_name) {
      j <- which(method_names == method_name)
      ag[i, j] <- aggregator@aggregate(evals_list[[i]]@evals[[method_name]])
    }
  }
  rownames(ag) <- unlist(lapply(evals_list, function(e) e@model_name))
  colnames(ag) <- method_names
  ag
}
