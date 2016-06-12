options(simulator.verbose = FALSE)

context("tables")

make_testmodel <- function(n) {
  return(new("Model", name = "tm",
             label = sprintf("Test model (n = %s)", n),
             params = list(n = n, mu = 2),
             simulate = function(mu, n, nsim) {
               y <- list()
               for (i in seq(nsim))
                 y[[i]] <- mu + rnorm(n)
               return(y)
             }))
}

his_method <- new("Method",
                  name = "his",
                  label = "His method",
                  method = function(model, draw) {
                    return(list(est = median(draw)))
                  })

my_method <- new("Method",
                 name = "my",
                 label = "My method",
                 method = function(model, draw) {
                   return(list(est = mean(draw)))
                 })

squared_error <- new("Metric",
                     name = "sqerr",
                     label = "Squared Error",
                     metric = function(model, out) {
                       sum((out$est - model$mu)^2)
                     })

l1_error <- new("Metric",
                name = "l1",
                label = "L1 Error",
                metric = function(model, out) {
                  sum(abs(out$est - model$mu))
                })

test_that("can make a table", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodel, vary_along = "n",
                         n = list(5, 100))
  dref1 <- simulate_from_model(mref[[1]], nsim = 3, index = 1:3)
  dref2 <- simulate_from_model(mref[[2]], nsim = 3, index = 1:3)
  oref1 <- run_method(dref1, list(my_method, his_method))
  oref2 <- run_method(dref2, list(my_method))
  eref1 <- evaluate(oref1, list(squared_error, l1_error))
  eref2 <- evaluate(oref2, list(l1_error))
  evals1 <- load(eref1)
  evals2 <- load(eref2)
  evlist <- list(evals1, evals2)
  expect_error(tabulate_eval(evlist, "sqerr", method_names = "yours"),
               "not found in any evals")
  expect_error(tabulate_eval(evlist, "sqerr", method_names = "his",
                format_args = list(digits = 2)), NA)
  unlink(dir, recursive = TRUE)
})
