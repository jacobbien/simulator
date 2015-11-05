#' @include reference-classes.R

#' @export
setGeneric("load")

setMethod("load", signature(file = "ModelRef"), function(file) {
  return(load_model(dir = file@dir, model_name = file@name, more_info = FALSE,
                    simulator.files = file@simulator.files))
})

setMethod("load", signature(file = "DrawsRef"), function(file) {
  return(load_draws(dir = file@dir, model_name = file@model_name,
                    index = file@index,
                    more_info = FALSE,
                    simulator.files = file@simulator.files))
})

setMethod("load", signature(file = "OutputRef"), function(file) {
  load_outputs_from_ref(file)
})

setMethod("load", signature(file = "list"), function(file) {
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
      if (file[[i]]@method_name != method_name)
        stop(sprintf(str, "method_name"))
      if (file[[i]]@out_loc != out_loc) stop(sprintf(str, "out_loc"))
    }
    # all OutputRef have the same dir, model_name, simulator.files,
    # method_name, and out_loc
    index <- unlist(lapply(file, function(ref) ref@index))
    return(load_evals(dir = dir, model_name = model_name, index = index,
                      method_name = file[[i]]@method_name,
                      metric_names = NULL,
                      out_loc = file[[i]]@out_loc,
                      simulator.files = file[[i]]@simulator.files))
  }
  stop("load is not defined for a list of such objects.")
})
