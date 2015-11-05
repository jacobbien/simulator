
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
