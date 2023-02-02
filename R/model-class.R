#' @include component-class.R
NULL

check_model <- function(object) {
  errors <- check_component(object)
  if (length(errors) == 1) {
    if(errors == TRUE)
      errors <- character()
  }
  if ("nsim" %in% names(object@params))
    errors <- c(errors, "\"nsim\" is not an allowed name within \"params\"")
  args <- names(formals(object@simulate))
  str <- "simulate's arguments must be \"nsim\" and those in \"params\""
  if (!all(args %in% c(names(object@params), "nsim")))
    errors <- c(errors, str)
  if (!("nsim" %in% args))
    errors <- c(errors, "simulate must have argument \"nsim\".")
  if (length(errors) == 0) TRUE else errors
}


#' An S4 class representing the model component of the simulator.
#'
#' An object of class Model specifies the statistical model.  In particular,
#' all parameters are specified in addition to a function called \code{simulate}
#' that allows one to draw random samples from this model.
#'
#' To get parameters stored in a \code{Model} object, a shortcut for
#' \code{my_model@@params$my_parameter} is \code{my_model$my_parameter}.
#'
#' This class inherits from the \code{\linkS4class{Component}} class.
#'
#' @slot name a short name identifier.  Must be alphanumeric (though -, _, and
#'       / are allowed as long as they are not at the start or end of name.
#' @slot label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @slot params a list that contains the Model object's parameters
#' @slot simulate a function that has arguments \code{nsim} and names
#'       matching elements within \code{names(params)}. It returns a list of
#'       length nsim, where each element of the list represents a random draw
#'       from the \code{Model} object.
#'
#' @export
setClass("Model", representation(params = "list", simulate = "function"),
         contains = "Component", validity = check_model)

setMethod("show", "Model", function(object) {
  validObject(object)
  callNextMethod()
  if (length(object@params) == 0) cat(" params: empty", fill = TRUE)
  else {
    nams <- names(object@params)
    if (length(nams) <= 20) {
      cat(paste0(" params: ",
                 paste0(nams, collapse = " ")), fill = TRUE)
    } else {
      cat(paste0(sprintf(" params: (%s items) ", length(nams)),
                 paste0(nams[1:5], collapse = " ")),
          " ... ",
          nams[length(nams)],
          fill = TRUE)
    }
    catsim(" (Add @params to end of this object to see parameters.)",
           fill = TRUE)
    catsim(" (Add @simulate to end of this object to see how data is simulated.)",
           fill = TRUE)
  }
})

#' Get element of \code{\linkS4class{Model}}'s \code{params} list
#' @param x object of class \code{\linkS4class{Model}}
#' @param name name of an element appearing in \code{x@@params}
#' @export
setMethod("$", "Model", function(x, name) return(x@params[[name]]))

#' Create a Model object
#'
#' Creates a new \code{\linkS4class{Model}} object.
#'
#' @param name a short name identifier.  Must be alphanumeric (though -, _, and
#'       / are allowed as long as they are not at the start or end of name.
#' @param label a longer, human readable label that can have other characters
#'       such as spaces, hyphens, etc.
#' @param params a list that contains the Model object's parameters
#' @param simulate a function that has arguments \code{nsim} and names
#'       matching elements within \code{names(params)}. It returns a list of
#'       length nsim, where each element of the list represents a random draw
#'       from the \code{Model} object.
#'
#' @export
#' @examples
#' make_my_example_model <- function(n) {
#'   new_model(name = "normal-data",
#'             label = sprintf("Normal (n = %s)", n),
#'             params = list(n = n, mu = 2),
#'             simulate = function(n, mu, nsim) {
#'               # this function must return a list of length nsim
#'               x <- matrix(rnorm(n * nsim), n, nsim)
#'               x <- mu + x # true mean is mu
#'               return(split(x, col(x))) # make each col its own list element
#'             })
#' }
new_model <- function(name, label, params = list(), simulate) {
  new("Model", name = name, label = label, params = params,
      simulate = simulate)
}

#' Convert a list of Model objects into a data.frame
#'
#' Ignores any params that are not length 1 and numeric or character
#'
#' @param m model object
models_as_data.frame <- function(m) {
  getattr <- function(x, att, element = NULL, fun = function(xx) xx) {
    # gets x[[i]]@att for each i when element is NULL (fun ignored)
    # gets fun(x[[i]]@att$element) for each i otherwise
    if (is.null(element)) {
      ll <- lapply(x, function(xx) {
        a <- attr(xx, att)
        if (is.null(a)) return(NA)
        return(a)
      })
      return(unlist(ll))
    } else {
      ll <- lapply(x, function(xx) {
        a <- fun(attr(xx, att)[[element]])
        if (is.null(a)) return(NA)
        return(a)
      })
      return(unlist(ll))
    }
  }
  if (!identical(class(m), c("listofModels", "list")))
    if (length(class(m)) != 1)
      stop("Must be a list of Models or a listofModels")
  else if (is(m, "Model")) m <- list(m)
  else if (is(m, "list")) {
    if (any(getattr(m, "class") != "Model"))
      stop("Must be a list of Model objects.")
  }
  else
    stop("Must be a list of Model objects.")
  df <- data.frame(row.names = seq_along(m))
  df[["name"]] <- getattr(m, "name")
  df[["label"]] <- getattr(m, "label")
  param_names <- unique(unlist(lapply(m, function(mm) names(mm@params))))
  check_if_fine <- function(param) {
    if (length(param) == 1)
      if (is.numeric(param) || is.character(param))
        return(TRUE)
    return(FALSE)
  }
  for (nam in param_names) {
    is_fine_as_is <- getattr(m, "params", nam, check_if_fine)
    if (any(is_fine_as_is))
      df[is_fine_as_is, nam] <- getattr(m[is_fine_as_is], "params", nam)
  }
  df
}

#' Subset Models
#'
#' Given a list of \code{\linkS4class{Model}} objects, returns model names
#' which meet conditions.  Uses \code{\link{subset}}
#'
#' @param m list of \code{\linkS4class{Model}} objects
#' @param ... logical expression involving parameters of Models.  For now, can
#' only be parameters that are of length 1 and either of class numeric or
#' character
#' @export
subset_models <- function(m, ...) {
  df <- models_as_data.frame(m)
  subset(df, ...)[["name"]]
}

#' Convert a Model to a data.frame
#'
#' Ignores any params that are not length 1 and numeric or character. This is
#' equivalent to calling \code{as(x, "data.frame")}
#' @param x object of class \code{\linkS4class{Model}}
#' @param row.names not used
#' @param optional not used
#' @param ... not used
#' @export
as.data.frame.Model <- function(x, row.names = NULL, optional = FALSE, ...)
  as(x, "data.frame")

setAs(from = "Model", to = "data.frame",
      def = function(from) models_as_data.frame(from))

#' Convert a List of Models to a data.frame
#'
#' When \code{\link{load}} generates a list of Models, it assigns this
#' to be of (S3) class listofModels, inherited from list, so that this function
#' will be invoked instead of as.data.frame.list, which is defined in base.
#'
#' @param x list
#' @param row.names not used
#' @param optional not used
#' @param ... not used
#' @export
as.data.frame.listofModels <- function(x, row.names = NULL, optional = FALSE,
                                       ...) {
  return(models_as_data.frame(x))
}
