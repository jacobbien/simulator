options(simulator.verbose = FALSE)

context("simulation")

make_testmodel <- function() {
  return(new("Model", name = "tm",
             label = sprintf("Test model"),
             params = list(n = 2, x = runif(2)),
             simulate = function(x, n, nsim) {
               y <- list()
               for (i in seq(nsim))
                 y[[i]] <- x + rnorm(n)
               return(y)
             }))
}

make_testmodel2 <- function() {
  return(new("Model", name = "tm2",
             label = sprintf("Test model 2"),
             params = list(n = 2, x = runif(2)),
             simulate = function(x, n, nsim) {
               y <- list()
               for (i in seq(nsim))
                 y[[i]] <- 1 + x + rnorm(n)
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
                       sum((out$est - model$x)^2)
                     })

l1_error <- new("Metric",
                name = "l1",
                label = "L1 Error",
                metric = function(model, out) {
                  sum(abs(out$est - model$x))
                })

linf_error <- new("Metric",
                name = "li",
                label = "L Infinity Error",
                metric = function(model, out) {
                  max(abs(out$est - model$x))
                })

test_that("show and add to a simulation object", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  sim <- new("Simulation", name = "sim1", label = "test simulation")
  expect_output(show(sim), "0 model")
  mref <- generate_model(make_testmodel, dir = dir)
  mref2 <- generate_model(make_testmodel2, dir = dir)
  sim <- add(sim, mref)
  expect_error(add(sim, mref), "already")
  dref <- simulate_from_model(mref, nsim = 1, 1:3)
  dref2 <- simulate_from_model(mref2, nsim = 1, 1:2)
  dref2b <- simulate_from_model(mref2, nsim = 1, 2:4)
  dref2c <- simulate_from_model(mref2, nsim = 1, 5:6)
  sim <- add(sim, dref)
  expect_error(add(sim, dref2), "until model named")
  sim <- add(sim, mref2)
  sim <- add(sim, dref2)
  sim <- add(sim, dref2b[2:3])
  expect_output(add(sim, dref2b[[1]]), "already")
  oref <- run_method(dref, list(my_method, his_method))
  oref2c <- run_method(dref2c, my_method)
  sim <- add(sim, oref)
  expect_output(add(sim, oref[[1]]), "already")
  expect_error(add(sim, oref2c), "until draws")
  eref <- evaluate(oref, list(squared_error, l1_error, linf_error))
  eref2c <- evaluate(oref2c, list(squared_error))
  sim <- add(sim, eref)
  expect_output(add(sim, eref[[1]]), "already")
  expect_error(add(sim, eref2c), "until output")
  # check that adding a list of references works:
  sim <- new("Simulation", name = "sim1", label = "test simulation")
  sim2 <- add(sim, list(oref, mref2, dref, mref))
  sim3 <- add(sim, mref2)
  sim3 <- add(sim3, mref)
  sim3 <- add(sim3, dref)
  sim3 <- add(sim3, oref)
  expect_identical(sim2, sim3)
  unlink(dir, recursive = TRUE)
})
