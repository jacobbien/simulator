#' @include reference-classes.R

#' Load simulator objects
#' @exportMethod load
setGeneric("load")

#' Load a ModelRef
#'
#' @param file object to load
setMethod("load", signature(file = "ModelRef"), function(file) {
  return(load_model(dir = file@dir, model_name = file@name, more_info = FALSE,
                    simulator.files = file@simulator.files))
})

#' Load a DrawsRef
#'
#' @param file object to load
setMethod("load", signature(file = "DrawsRef"), function(file) {
  return(load_draws(dir = file@dir, model_name = file@model_name,
                    index = file@index,
                    more_info = FALSE,
                    simulator.files = file@simulator.files))
})

#' Load an OutputRef
#'
#' @param file object to load
setMethod("load", signature(file = "OutputRef"), function(file) {
  load_outputs_from_ref(file)
})

#' Load an EvalsRef
#'
#' @param file object to load
setMethod("load", signature(file = "EvalsRef"), function(file) {
  load_evals_from_ref(file)
})


#' Load a list of reference objects
#'
#' @param file list of objects to load
setMethod("load", signature(file = "list"), function(file) {
  if (length(file) == 0) return(list())
  if (all(unlist(lapply(file, class)) == "list")) {
    # recursively call load till not a list of lists
    list_of_loaded_objects <- lapply(file, load)
    if (all(lapply(list_of_loaded_objects, class) == "Evals"))
      class(list_of_loaded_objects) <- c("listofEvals", "list")
    # ...so that as.data.frame.listofEvals is invoked when passed one of these
    return(list_of_loaded_objects)
  }
  if (length(file) == 1) return(load(file[[1]]))
  if (all(unlist(lapply(file, class)) == "ModelRef")) {
    list_of_loaded_models <- lapply(file, load)
    class(list_of_loaded_models) <- c("listofModels", "list")
    # ...so that as.data.frame.listofModels is invoked when passed one of these
    return(list_of_loaded_models)
  }
  if (all(unlist(lapply(file, class)) == "DrawsRef")) {
    for (i in seq_along(file)) {
      if (i == 1) {
        model_name <- file[[i]]@model_name
        dir <- file[[i]]@dir
        simulator.files <- file[[i]]@simulator.files
        next
      }
      if (file[[i]]@model_name != model_name)
        stop("Can only load a list of DrawsRef having the same model_name.")
      if (file[[i]]@dir != dir)
        stop("Can only load a list of DrawsRef having the same dir.")
      if (file[[i]]@simulator.files != simulator.files)
        stop("Can only load a list of DrawsRef having the same simulator.files")
    }
    # all DrawsRef have the same dir, model_name, and simulator.files
    index <- unlist(lapply(file, function(dref) dref@index))
    return(load_draws(dir = dir, model_name = model_name, index = index,
                      more_info = FALSE, simulator.files = simulator.files))
  }
  if (all(unlist(lapply(file, class)) == "OutputRef")) {
    for (i in seq_along(file)) {
      if (i == 1) {
        model_name <- file[[i]]@model_name
        dir <- file[[i]]@dir
        simulator.files <- file[[i]]@simulator.files
        method_name <- file[[i]]@method_name
        out_loc <- file[[i]]@out_loc
        next
      }
      str <- "Can only load a list of OutputRef having the same %s."
      if (file[[i]]@model_name != model_name) stop(sprintf(str, "model_name"))
      if (file[[i]]@dir != dir) stop(sprintf(str, "dir"))
      if (file[[i]]@simulator.files != simulator.files)
        stop(sprintf(str, "simulator.files"))
      if (file[[i]]@method_name != method_name)
        stop(sprintf(str, "method_name"))
      if (file[[i]]@out_loc != out_loc) stop(sprintf(str, "out_loc"))
    }
    # all OutputRef have the same dir, model_name, simulator.files,
    # method_name, and out_loc
    index <- unlist(lapply(file, function(ref) ref@index))
    return(load_outputs(dir = dir, model_name = model_name, index = index,
                        method_name = method_name, out_names = NULL,
                        out_loc = out_loc,
                        simulator.files = simulator.files))
  }
  if (all(unlist(lapply(file, class)) == "EvalsRef")) {
    for (i in seq_along(file)) {
      if (i == 1) {
        model_name <- file[[i]]@model_name
        dir <- file[[i]]@dir
        simulator.files <- file[[i]]@simulator.files
        method_name <- file[[i]]@method_name
        out_loc <- file[[i]]@out_loc
        next
      }
      str <- "Can only load a list of EvalsRef having the same %s."
      if (file[[i]]@model_name != model_name) stop(sprintf(str, "model_name"))
      if (file[[i]]@dir != dir) stop(sprintf(str, "dir"))
      if (file[[i]]@simulator.files != simulator.files)
        stop(sprintf(str, "simulator.files"))
      if (file[[i]]@out_loc != out_loc) stop(sprintf(str, "out_loc"))
    }
    # all OutputRef have the same dir, model_name, simulator.files,
    # and out_loc
    index <- unlist(lapply(file, function(ref) ref@index))
    method_names <- unlist(lapply(file, function(ref) ref@method_name))
    # make sure each method has the same indices computed
    mnames <- unique(method_names)
    for (m in seq_along(mnames)) {
      ii <- which(method_names == mnames[m])
      if (m == 1) indices_for_method <- index[ii]
      else if (any(index[ii] != indices_for_method))
        stop("To be loaded together, each method must be computed on same ",
              "set of indices")
    }
    return(load_evals(dir = dir, model_name = model_name, index = index,
                      method_names = mnames,
                      metric_names = NULL,
                      out_loc = file[[i]]@out_loc,
                      simulator.files = file[[i]]@simulator.files))
  }
  stop("load is not defined for a list of such objects.")
})
