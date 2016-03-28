options(simulator.verbose = FALSE)

context("evaluate")

make_testmodel <- function() {
  return(new_model(name = "tm",
                   label = sprintf("Test model"),
                   params = list(n = 2, x = runif(2)),
                   simulate = function(x, n, nsim) {
                     y <- list()
                     for (i in seq(nsim))
                       y[[i]] <- x + rnorm(n)
                     return(y)
                   }))
}

his_method <- new_method(name = "his",
                         label = "His method",
                         method = function(model, draw) {
                           return(list(est = median(draw)))
                         })
my_method <- new_method(name = "my",
                        label = "My method",
                        method = function(model, draw) {
                          return(list(est = mean(draw),
                                      f = draw[1]))
                        })

squared_error <- new_metric(name = "se",
                            label = "Squared Error",
                            metric = function(model, out) {
                              sum((out$est - model$x)^2)
                            })

l1_error <- new_metric(name = "l1",
                       label = "L1 Error",
                       metric = function(model, out) {
                         sum(abs(out$est - model$x))
                       })

linf_error <- new_metric(name = "li",
                         label = "L Infinity Error",
                         metric = function(model, out) {
                           max(abs(out$est - model$x))
                         })

normy <- new_metric(name = "normy",
                         label = "Norm of y",
                         metric = function(model, out, draw) {
                           sqrt(sum(draw^2))
                         })

test_that("evaluate handles arguments as desired", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodel)
  dref <- simulate_from_model(mref, nsim = 20, 1:3)
  oref <- run_method(dref, list(my_method, his_method))
  eref <- evaluate(oref, list(squared_error, l1_error, linf_error))
  expect_identical(load(eref[c(1,3,5)]), load_evals(dir, "tm", 1:3, "my"))
  evals2a <- load_evals(dir, "tm", 2, c("my", "his"), metric_names = "l1")
  evals12a <- load_evals(dir, "tm", 1:2, c("my", "his"), metric_names = "l1")
  evaluate(oref, l1_error)
  evals2b <- load_evals(dir, "tm", 2, "his", metric_names = "l1")
  expect_identical(subset_evals(evals2a, method_names = "his"), evals2b)
  evaluate(oref, l1_error)
  evals12b <- load_evals(dir, "tm", 1:2, "his", metric_names = "l1")
  expect_identical(subset_evals(evals12a, method = "his"), evals12b)
  evaluate(oref, list(l1_error, linf_error, normy))
  unlink(dir, recursive = TRUE)
})
