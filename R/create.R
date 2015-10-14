#' @include utils.R
NULL

#' Create a new simulation.
#'
#' This function is the fastest way to get started.  Creates the skeleton of a
#' simulation.
#'
#' @export
#' @param where to create the new simulation
#' @examples
#' \dontrun{
#'  create("./examples")
#'  }
create <- function(path = "../example") {
  path <- remove_slash(path)
  if (dir.exists(path))
    stop ("This directory already exists.  Choose a new directory name.")
  else
    dir.create(path)
  write_models(file.path(path, "model_functions.R"))
  write_methods(file.path(path, "method_functions.R"))
  write_evals(file.path(path, "eval_functions.R"))
  write_main(file.path(path, "main.R"), path)
  cat(sprintf("New simulation created!  Go to %s to get started.",
              file.path(path, "main.R")), fill = TRUE)
}

write_models <- function(filename) {
  str <-
"make_my_%s_model <- function() {
  # this function returns an object of class Model
  n <- 20
  params <- list(n = n, mu = rnorm(n)) # model parameters
  simulate <- function(mu, n, nsim) {
    # define function here that returns a list of length nsim
    y <- list()
    for (i in seq(nsim))  y[[i]] <- mu + rnorm(n)
    return(y)
  }
  return(new(\"Model\", name = \"%s\", label = \"My %s Model\",
             params = params, simulate = simulate))
}\n"
  dcat(sprintf(str, "first", "fm", "First"), outfile = filename, append = FALSE)
  dcat(sprintf(str, "second", "sm", "Second"), outfile = filename)
}

write_methods <- function(filename) {
  str <-
"%s_method <- new(\"Method\",
                  name = \"%s\",
                  label = \"%s Method\",
                  method = function(model, draw) {
                      return(%s)
                  })\n\n"
  dcat(sprintf(str, "my", "mm", "My New", "list(est = median(draw))"),
       outfile = filename, append = FALSE)
  dcat(sprintf(str, "their", "tm", "Their", "list(est = mean(draw))"),
       outfile = filename)
}

write_evals <- function(filename) {
  str <-
    "%s_err <- new(\"Metric\",
  name = \"%s\",
  label = \"%s\",
  metric = function(model, out) {
  return(%s)
  })\n\n"
  dcat(sprintf(str, "abs", "abserr", "Absolute Error",
               "sum(abs(out$est - model$mu))"), outfile = filename,
       append = FALSE)
  dcat(sprintf(str, "sq", "sqerr", "Squared Error",
               "sum((out$est - model$mu)^2)"), outfile = filename)
}


write_main <- function(filename, dir) {
  dcat("# This is the main simulation file.", outfile = filename, append = FALSE)
  dcat("library(simulator) # this file was created under simulator version ",
       installed.packages()["simulator", "Version"], outfile = filename)
  dcat(sprintf("source(\"%s\")", normalizePath(file.path(dir, "model_functions.R"))),
       outfile = filename)
  dcat(sprintf("source(\"%s\")", normalizePath(file.path(dir, "method_functions.R"))),
       outfile = filename)
  dcat(sprintf("source(\"%s\")", normalizePath(file.path(dir, "eval_functions.R"))),
       outfile = filename)
  dcat(sprintf("dir <- \"%s\"", normalizePath(dir)), outfile = filename)
  hline(outfile = filename)
  dcat("generate_model(make_my_first_model, dir = dir)", outfile = filename)
  dcat("simulate_from_model(dir = dir, model_name = \"fm\", nsim = 20, index = 1)",
       outfile = filename)
  str <-
"run_method(my_methods = list(my_method, their_method), dir = dir,
           model_name = \"fm\", index = 1)"
  dcat(str, outfile = filename)
  str <-
"evaluate(metrics = list(abs_err, sq_err),
         dir = dir,
         model_name = \"fm\",
         index = 1,
         method_names = c(\"mm\", \"tm\"))"
  dcat(str, outfile = filename)
}

dcat <- function(..., append, outfile = "") {
  if (missing(append)) append <- TRUE
  cat(..., file = outfile, fill = TRUE, sep = "", append = append)
}

hline <- function(..., append=TRUE) dcat(..., rep("#",80), append=append)

test <- function() {
  create("../example")
}
