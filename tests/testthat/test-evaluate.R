options(simulator.verbose = FALSE)

context("evaluate")

make_testmodel <- function() {
  return(new("Model", name = "tm",
             label = sprintf("Test model"),
             params = list(n = 2, x = runif(2)),
             simulate = function(params, nsim) {
               y <- list()
               for (i in seq(nsim))
                 y[[i]] <- params$x + rnorm(params$n)
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
                   return(list(est = mean(draw),
                               f = draw[1]))
                 })

squared_error <- new("Metric",
                     name = "se",
                     label = "Squared Error",
                     metric = function(model, out) {
                       sum((out$est - model@params$x)^2)
                     })

l1_error <- new("Metric",
                name = "l1",
                label = "L1 Error",
                metric = function(model, out) {
                  sum(abs(out$est - model@params$x))
                })

linf_error <- new("Metric",
                name = "li",
                label = "L Infinity Error",
                metric = function(model, out) {
                  max(abs(out$est - model@params$x))
                })

test_that("evaluate handles arguments as desired", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  generate_model(make_testmodel, dir = dir)
  simulate_from_model(dir, "tm", nsim = 20, 1:3)
  run_method(list(my_method, his_method), dir, "tm", 1:3)
  evaluate(list(squared_error, l1_error, linf_error),
           dir, "tm", 1:2, c("my", "his"))
  evals <- load_evals(dir, "tm", 2, c("my", "his"), metric_names = "l1")
  evaluate(l1_error, dir, "tm", 2:3, "his")
  evals2 <- load_evals(dir, "tm", 2, "his", metric_names = "l1")
  expect_identical(evals[[2]], evals2)
  unlink(dir, recursive = TRUE)
})
