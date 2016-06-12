#' Load necessary libraries on a cluster
#' @keywords internal
load_libraries_on_cluster <- function(cl, libs) {
  libs <- unique(libs)
  stopifnot(is.character(libs))
  parallel::clusterExport(cl, "libs", envir = environment())
  parallel::clusterEvalQ(cl, sapply(libs,
                                    function(pkgnam) { do.call("library",
                                                               list(pkgnam))}))
}

check_parallel_list <- function(parallel) {
  if (!is.list(parallel)) stop("parallel must be a list.")
  parallel_args <- c("socket_names", "libraries", "save_locally")
  if (length(setdiff(names(parallel), parallel_args)) > 0) {
    args <- paste(parallel_args, collapse = ", ")
    stop("parallel must be a list containing only ", args)
  }
  if (!("socket_names" %in% names(parallel)))
    stop("parallel must be a list containing \"socket_names\".")
  if ("libraries" %in% names(parallel))
    stopifnot(is.character(parallel$libraries))
  if ("save_locally" %in% names(parallel))
    stopifnot(is.logical(parallel$save_locally))
}

#' Do a function in parallel.
#'
#' This is an internal function. The function_to_do is done in parallel
#' and its output is saved either on slave (locally) or on master.
#'
#' @param function_to_do this is the function that will be done in parallel
#' @param function_params a list where \code{function_params[[i]]} is
#'        a named list of parameters to be passed to \code{function_to_do} for
#'        job \code{i}
#' @param save_to_file function that saves stuff to file
#' @param save_params a list where \code{save_params[[i]]}
#'        is a named list of parameters to be passed to \code{save_to_file} for
#'        job \code{i}. Each \code{save_params[[i]]} must include \code{out_dir},
#'        which is location where file is to be saved.
#' @param socket_names quoting from \code{\link[parallel]{makePSOCKcluster}}:
#'        "either a character vector of host names on which to run the worker
#'        copies of R, or a positive integer (in which case that number of
#'        copies is run on localhost)."
#' @param libraries character vector of R packages that will be needed on the
#'        slaves.
#' @param save_locally if TRUE, then files will be saved on slaves.  If FALSE,
#'        they will be saved on master.
#' @keywords internal
do_in_parallel <- function(function_to_do, function_params,
                           save_to_file, save_params, socket_names,
                           libraries, save_locally = TRUE) {
  njobs <- length(function_params)
  for (i in seq(njobs)) stopifnot("out_dir" %in% names(save_params[[i]]))
  inner_wrapper <- function(i) {
    do.call("function_to_do", function_params[[i]])
  }
  if (save_locally) {
    wrapper <- function(i) {
      # create directory on slave if it doesn't yet exist:
      if (!dir.exists(save_params[[i]]$out_dir))
        dir.create(save_params[[i]]$out_dir, recursive = TRUE)
      d <- inner_wrapper(i)
      if (any(names(d) %in% names(save_params[[i]])))
        stop("save_params must not contain any elements with
             same name as list output of function_to_do.")
      # save this on slave
      file <- do.call("save_to_file", c(d, save_params[[i]]))
      return(file)
    }
  } else {
    wrapper <- inner_wrapper
  }
  if (getOption("simulator.slave_stdout_stderr_to_master"))
    cl <- parallel::makePSOCKcluster(names = socket_names, outfile = "")
  else
    cl <- parallel::makePSOCKcluster(names = socket_names)
  if (!("simulator" %in% libraries)) libraries <- c("simulator", libraries)

  tryCatch({
    load_libraries_on_cluster(cl, libraries)
    # export the parameters that will be needed by function_to_do on slaves
    parallel::clusterExport(cl, varlist = c("function_params", "save_params"),
                            envir = environment())
    parallel::clusterExport(cl, varlist = ls(envir = globalenv()))
    out <- parallel::parLapplyLB(cl, seq(njobs), wrapper)
  }, finally = {
    # regardless of whether an error occurs, do close cluster.
    message("Shutting down cluster.")
    parallel::stopCluster(cl)})
  if (save_locally) {
    # out is a list of file names on slave where output was saved
    refs <- out
  } else {
    # out is a list of outputs of function_to_do
    # we now save these to file (on master)
    refs <- list()
    for (i in seq(njobs))
      refs[[i]] <- do.call("save_to_file", c(out[[i]], save_params[[i]]))
  }
  catsim("..Created", as.character(refs), "in parallel.", fill = TRUE)
  if (save_locally) catsim("(saved on slaves)", fill = TRUE)
  invisible(refs)
}
