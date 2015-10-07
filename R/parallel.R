#' @export
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
