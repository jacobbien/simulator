.onLoad <- function(libname, pkgname) {
  # most of this function is copied from Hadley Wickham's dplyr/zzz.R
  op <- options()
  op.simulator <- list(
    simulator.verbose = TRUE,
    simulator.files = "files" # directory created in user supplied "dir"
    )
  toset <- !(names(op.simulator) %in% names(op))
  if(any(toset)) options(op.simulator[toset])

  invisible()
}
