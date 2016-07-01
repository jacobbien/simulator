
#' Get the contents of a simulator directory
#'
#' @param dir name of the directory where directory named "files" exists
#' @param out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.  Usually this is just "out"
#' @export
get_contents <- function(dir = ".", out_loc = "out") {
  stopifnot(is.character(dir))
  path <- file.path(dir, options("simulator.files"))
  if (!dir.exists(path))
    stop("Could not find a directory named", path)
  files <- list.files(path, recursive = TRUE)
  dir_name <- dirname(files)
  file_name <- basename(files)

  # find simulation files
  sim_files <- which(dir_name == ".")
  sim_names <- gsub("^sim-(.*).Rdata$", "\\1", files[sim_files])
  mem <- sum(file.size(file.path(path, files)))

  # find model files
  model_files <- which(file_name == "model.Rdata")
  model_names <- dir_name[model_files]

  # find draw indices for each model
  objects <- lapply(model_names, function(m) {
    ii <- setdiff(which(dir_name == m), model_files)
    index <- gsub("^r([[:digit:]]*).Rdata$", "\\1", file_name[ii])
    return(list(draws = as.numeric(index)))
    })
  names(objects) <- model_names

  # find output/eval files
  out_and_evals_files <- grep(paste0(out_loc, "$"), dir_name)
  evals_files <- out_and_evals_files[grep("_evals.Rdata",
                                          file_name[out_and_evals_files])]
  out_files <- setdiff(out_and_evals_files, evals_files)
  for (m in model_names) {
    objects[[m]]$out <- objects[[m]]$evals <- list()
    for (d in objects[[m]]$draws) {
      pattern <- sprintf("^%s/%s/r%s_(.*).Rdata$", m, out_loc, d)
      ii <- grep(pattern, files[out_files])
      objects[[m]]$out[[d]] <- gsub(pattern, "\\1", files[out_files][ii])
      pattern <- sprintf("^%s/%s/r%s_(.*)_evals.Rdata$", m, out_loc, d)
      ii <- grep(pattern, files[evals_files])
      objects[[m]]$evals[[d]] <- gsub(pattern, "\\1", files[evals_files][ii])
    }
  }
  return(list(sim_names = sim_names, mem = mem, objects = objects,
              nfiles = length(files)))
}


#' Describe the contents of a simulator directory
#'
#' @param dir name of the directory where directory named "files" exists
#' @export
describe <- function(dir = ".") {
  con <- get_contents(dir)
  cat(sprintf("There are a total of %s files (%s) stored.\n\n",
              con$nfiles, memory_as_string(con$mem),
              fill = TRUE))
  if (length(con$sim_names) == 0) {
    cat("There are no simulations.", fill = TRUE)
  } else {
    # extract "whatever" from sim-whatever.Rdata"
    if (length(con$sim_names) == 1) cat("There is one simulation file: ",
                                        con$sim_names, fill = TRUE)
    else {
      cat(sprintf("There are %s simulation file:\n", length(con$sim_names)))
      for (i in seq_along(con$sim_names))
        cat(sprintf("%s) %s\n", i, con$sim_names[i]))
    }
  }
  cat(fill = TRUE)
  if (length(con$objects) == 0) {
    cat("There are no model files.", fill = TRUE)
    return()
  }
  cat(sprintf("There are %s models:", length(con$objects)), fill = TRUE)
  for (i in seq_along(con$objects)) {
    cat(sprintf(" %s) model_name: %s", i, names(con$objects)[i]), fill = TRUE)
    cat(sprintf("    draws for index %s\n",
                paste(con$objects[[i]]$draws,  collapse = ", ")))
    for (d in seq_along(con$objects[[i]]$draws)) {
      cat(sprintf("    %s) draw index %s\n", d, d))
      for (o in con$objects[[i]]$out[[d]]) {
        cat(sprintf("     outputs: %s", o))
        if (o %in% con$objects[[i]]$evals[[d]])
          cat(" (with evals)", fill = TRUE)
        else
          cat(fill = TRUE)
      }
    }
  }
}

#' Find files in simulator directory not referred to by any simulations
#'
#' Once one has completed all simulation studies, this function can be called
#' to identify any files that may have been created along the way that are no
#' longer being used in any simulations.  It would then be safe to delete these
#' files.
#'
#' @param dir name of the directory where directory named "files" exists
#' @param out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.  Usually this is just "out"
#' @export
get_files_not_in_simulations <- function(dir, out_loc = "out") {
  con <- get_contents(dir, out_loc = out_loc)
  sims <- sapply(con$sim_names, load_simulation, dir = dir)
  path <- file.path(dir, options("simulator.files"))
  files <- normalizePath(file.path(path, list.files(path, recursive = TRUE)),
                         winslash = "/")
  in_sims <- rep(FALSE, length(files))
  for (sim in sims) {
    for (mref in model(sim, reference = TRUE)) {
      file <- sprintf("%s/%s/%s/model.Rdata", mref@dir, mref@simulator.files,
                      mref@name)
      in_sims[files == file] <- TRUE
    }
    for (m in draws(sim, reference = TRUE)) {
      for (dref in m) {
        file <- sprintf("%s/%s/%s/r%s.Rdata", dref@dir, dref@simulator.files,
                        dref@model_name, dref@index)
        in_sims[files == file] <- TRUE
      }
    }
    for (m in output(sim, reference = TRUE)) {
      for (d in m) {
        if (length(d) == 1) {
          file <- sprintf("%s/%s/%s/%s/r%s_%s.Rdata", d@dir,
                          d@simulator.files,
                          d@model_name, out_loc, d@index, d@method_name)
          in_sims[files == file] <- TRUE
          next
        } else {
          for (oref in d) {
            file <- sprintf("%s/%s/%s/%s/r%s_%s.Rdata", oref@dir,
                            oref@simulator.files,
                            oref@model_name, out_loc, oref@index, oref@method_name)
            in_sims[files == file] <- TRUE
          }
        }
      }
    }
  }
  files[!in_sims]
}

#' Returns a simulation object containing references to all files in directory
#'
#' @param dir name of the directory where directory named "files" exists
#' @param out_loc a length-1 character vector that gives location
#'        (relative to model's path) that method outputs are stored.This can be
#'        useful for staying organized when multiple simulations are based on
#'        the same Model and Draws objects.  Usually this is just "out"
#' @export
get_simulation_with_all_files <- function(dir, out_loc = "out") {
  con <- get_contents(dir, out_loc = out_loc)
  sim <- new_simulation(name = "all_files",
                        label = "A simulation with all files", dir = dir,
                        save_to_file = FALSE)
  m <- lapply(names(con$objects), load_model, dir = dir)
  simulator.files <- getOption("simulator.files")
  mref <- lapply(m, function(mm) new("ModelRef", dir = dir, name = mm@name,
                                     label = mm@label,
                                     simulator.files = simulator.files))
  rm(m)
  dref <- list()
  for (i in seq_along(con$objects)) {
    sim <- add(sim, ref = mref[[i]], update_saved = FALSE)
    tryCatch({sim <- add(sim, new("DrawsRef", dir = dir,
                                  model_name = names(con$objects)[i],
                                  index = con$objects[[i]]$draws,
                                  simulator.files = simulator.files),
                         update_saved = FALSE)},
             error = function(e) message(e))
    index <- con$objects[[i]]$draws
    for (d in index) {
      for (o in con$objects[[i]]$out[[d]]) {
        oref <- new("OutputRef", dir = dir,
                    model_name = names(con$objects)[i],
                    index = d, method_name = o, out_loc = out_loc,
                    simulator.files = simulator.files)
        tryCatch({sim <- add(sim, oref, update_saved = FALSE)},
                 error = function(e) message(e))
      }
      for (e in con$objects[[i]]$evals[[d]]) {
        eref <- new("EvalsRef", dir = dir,
                    model_name = names(con$objects)[i],
                    index = d, method_name = e, out_loc = out_loc,
                    simulator.files = simulator.files)
        tryCatch({sim <- add(sim, eref, update_saved = FALSE)},
                 error = function(e) message(e))
      }
    }
  }
  save_simulation(sim)
  sim
}
