
remove_slash <- function(s) {
  # if ends with slash, remove it
  n <- nchar(s)
  ifelse(substr(s, n, n) == "/", substr(s, 1, n - 1), s)
}

get_model_dir_and_file <- function(dir, model_name, simulator.files = NULL) {
  dir <- remove_slash(dir)
  if (is.null(simulator.files)) simulator.files <- getOption("simulator.files")
  model_dir <- file.path(dir, simulator.files, model_name)
  model_file <- file.path(model_dir, "model.Rdata")
  if (!file.exists(model_dir))
    stop("Could not find models directory ", model_dir, call. = FALSE)
  if (!file.info(model_dir)$isdir)
    stop(model_dir, " is not a directory.",call. = FALSE)
  if (!file.exists(model_file))
    stop("Could not find file ", model_file, call. = FALSE)
  list(dir = model_dir, file = model_file)
}

#' Concatenate and print for the simulator
#'
#' For internal use.  This calls \code{\link{cat}} only when
#' \code{getOption("simulator.verbose")}.
#' @param ... arguments to be passed to \code{\link{cat}}
catsim <- function(...) {
  if (getOption("simulator.verbose")) cat(...)
}


#' Get relative path
#'
#' Given a base path and a specific path, returns a string str such that
#' file.path(base_path, str) is the same location as path.
#'
#' @param base_path the base path
#' @param path a specific path
get_relative_path <- function(base_path, path) {
  b <- strsplit(normalizePath(base_path, mustWork = FALSE, winslash = "/"),
                split = "/")[[1]]
  p <- strsplit(normalizePath(path, mustWork = FALSE, winslash = "/"),
                split = "/")[[1]]
  len <- min(length(b), length(p))
  ncommon <- max(which(b[1:len] == p[1:len]))
  str <- rep("..", length(b) - ncommon)
  if (length(p) > ncommon)
    str <- c(str, p[(ncommon + 1):length(p)])
  return(paste0(str, collapse = "/"))
}

#' Write memory in human readable way
#'
#' @param memory_in_bytes the amount of memory in Bytes.
#' @export
memory_as_string <- function(memory_in_bytes) {
  units <- c("Bytes", "KB", "MB", "GB", "TB")
  ii <- floor(log(memory_in_bytes, 1000))
  if (ii >= length(units)) stop("Really? That's an enormous simulation!")
  if (ii < 0) stop("Negative memory?  How strange.")
  paste(round(memory_in_bytes / 1000^ii, 2), units[ii+1])
}
