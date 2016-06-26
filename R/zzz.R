.onLoad <- function(libname, pkgname) {
  # most of this function is copied from Hadley Wickham's dplyr/zzz.R
  op <- options()
  op.simulator <- list(
    simulator.verbose = TRUE, # controls catsim
    simulator.ndecimal = 5, # how long can a decimal be before "digest" called?
    simulator.files = "files", # directory created in user supplied "dir"
    simulator.slave_stdout_stderr_to_master = FALSE, # useful for debugging
    simulator.color_palette = c("#000000", "#e41a1c", "#377eb8", "#4daf4a",
                                "#984ea3", "#ff7f00", "#ffff33")
    # from http://colorbrewer2.org/ print-friendly, 6 classes (added black)
    )
  toset <- !(names(op.simulator) %in% names(op))
  if(any(toset)) options(op.simulator[toset])

  invisible()
}
