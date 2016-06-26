options(simulator.verbose = FALSE)

context("simulation")

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

make_testmodel2 <- function() {
  return(new_model(name = "tm2",
                   label = sprintf("Test model 2"),
                   params = list(n = 2, x = runif(2)),
                   simulate = function(x, n, nsim) {
                     y <- list()
                     for (i in seq(nsim))
                       y[[i]] <- 1 + x + rnorm(n)
                     return(y)
                   }))
}

make_testmodels <- function(n, p) {
  return(new_model(name = "tms",
                   label = sprintf("Test models n=%s, p=%s", n, p),
                   params = list(n = n, p = p),
                   simulate = function(n, nsim) {
                     y <- list()
                     for (i in seq(nsim))
                       y[[i]] <- rnorm(n)
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
                         if (is.null(model$x)) return(0)
                         sum(abs(out$est - model$x))
                       })

linf_error <- new_metric(name = "li",
                         label = "L Infinity Error",
                         metric = function(model, out) {
                           if (is.null(model$x)) return(0)
                           max(abs(out$est - model$x))
                         })

test_that("show and add to a simulation object", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  sim <- new_simulation(name = "sim1", label = "test simulation", dir = dir)
  rm(sim)
  sim <- load_simulation("sim1", dir)
  expect_output(show(sim), "0 model")
  mref <- generate_model(dir, make_testmodel)
  mref2 <- generate_model(dir, make_testmodel2)
  sim <- add(sim, mref)
  expect_error(add(sim, mref), "already")
  dref <- simulate_from_model(mref, nsim = 1, 1:3)
  dref2 <- simulate_from_model(mref2, nsim = 1, 1:2)
  dref2b <- simulate_from_model(mref2, nsim = 1, 2:4)
  dref2c <- simulate_from_model(mref2, nsim = 1, 5:6)
  sim <- add(sim, dref)
  expect_error(add(sim, dref2), "until model named")
  rm(sim)
  sim <- load_simulation("sim1", dir)
  sim <- add(sim, mref2)
  sim <- add(sim, dref2)
  sim <- add(sim, dref2b[2:3])
  expect_identical(sim, add(sim, dref2b[[1]]))
  oref <- run_method(dref, list(my_method, his_method))
  oref2c <- run_method(dref2c, my_method)
  sim <- add(sim, oref)
  expect_identical(sim, add(sim, oref[[1]]))
  expect_error(add(sim, oref2c), "until draws")
  eref <- evaluate(oref, list(squared_error, l1_error, linf_error))
  eref2c <- evaluate(oref2c, list(squared_error))
  rm(sim)
  sim <- load_simulation("sim1", dir)
  sim <- add(sim, eref)
  expect_identical(sim, add(sim, eref[[1]]))
  expect_error(add(sim, eref2c), "until output")
  # check that adding a list of references works:
  sim <- new("Simulation", name = "sim1", label = "test simulation", dir = dir)
  sim2 <- add(sim, list(oref, mref2, dref, mref))
  sim3 <- add(sim, mref2)
  sim3 <- add(sim3, mref)
  sim3 <- add(sim3, dref)
  sim3 <- add(sim3, oref)
  expect_identical(sim2, sim3)
  unlink(dir, recursive = TRUE)
})

test_that("get model from sim", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodels, vary_along = c("n", "p"),
                         n = as.list(1:3), p = as.list(letters[1:4]))
  sim <- new_simulation("testmodels", "Some test models", dir = dir,
                        refs = mref)
  expect_error(model(sim, subset = "a"), "unrecognized")
  expect_error(model(sim, subset = -1), "must be")
  expect_error(model(sim, subset = 1.2), "must be")
  expect_error(model(sim, subset = 1.2), "must be")
  expect_error(model(sim, subset = 100), "must be")
  m1 <- model(sim, subset = 2:3, reference = TRUE); m2 <- mref[2:3]
  expect_identical(lapply(m1, function(m) m@name),
                   lapply(m2, function(m) m@name))
  # using expect_equal instead of expect_identical because
  # function environments of [...]@simulate differ
  expect_equal(model(sim, subset = c(mref[[5]]@name, mref[[2]]@name)),
                   model(sim, subset = c(5, 2)))
  unlink(dir, recursive = TRUE)
  })


test_that("get draws from sim", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodels, vary_along = c("n", "p"),
                         n = as.list(1:2), p = as.list(letters[1:2]))
  dref <- simulate_from_model(mref[1:2], nsim = 2, index = 1:3)
  sim <- new_simulation("testmodels", "Some test models", dir = dir,
                        refs = mref)
  expect_identical(draws(sim), list())
  sim <- add(sim, dref)
  expect_identical(draws(sim, subset = 3), list())
  expect_identical(draws(sim, subset = 2, index = 4), list())
  expect_identical(draws(sim, subset = 2, index = 3), load(dref[[2]][[3]]))
  expect_identical(draws(sim, subset = 2, index = 2:3), load(dref[[2]][2:3]))
  expect_identical(draws(sim, subset = 1:2, index = 1:3), load(dref[1:2]))
  unlink(dir, recursive = TRUE)
})


test_that("get outputs and evals from sim", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodels, vary_along = c("n", "p"),
                         n = as.list(1:2), p = as.list(letters[1:2]))
  dref <- simulate_from_model(mref[2:3], nsim = 1, index = 1:3)
  oref <- run_method(dref, list(my_method, his_method))
  sim <- new_simulation("testmodels", "Some test models", dir = dir,
                        refs = c(mref, dref))
  expect_identical(output(sim), list())
  sim <- add(sim, oref)
  expect_identical(output(sim, subset = 1), list())
  expect_identical(output(sim, subset = 2, index = 4), list())
  expect_identical(output(sim, subset = 2, index = 3, methods = "not_a_method"),
                   list())
  expect_identical(output(sim, subset = 2, index = 3, methods = "his"),
                   load(oref[[1]][[6]]))
  dref2 <- simulate_from_model(mref[c(1, 4)], nsim = 1, index = 2)
  oref2 <- run_method(dref2, list(my_method, his_method))
  sim <- add(sim, c(dref2, oref2))
  oo <- list(oref[[2]][c(3, 5)], list(oref2[[1]][[1]]))
  expect_identical(output(sim, n == 1, index = 2:3, methods = "my"),
                   load(oo))
  oo <- list()
  oo[[1]] <- list(oref[[2]][c(3, 5)], oref[[2]][c(4, 6)])
  oo[[2]] <- list(list(oref2[[1]][[1]]), list(oref2[[1]][[2]]))
  expect_identical(output(sim, n == 1, index = 2:3), load(oo))
  oref <- output(sim, reference = TRUE)
  eref <- evaluate(oref, list(l1_error, linf_error))
  sim <- add(sim, eref)
  expect_identical(lapply(eref, unlist), evals(sim, reference = TRUE))
  expect_identical(unlist(eref[[2]]), evals(sim, n == 1 & p == "b",
                                    reference = TRUE))
  unlink(dir, recursive = TRUE)
})


test_that("passing simulation instead of references works", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  # a single model
  mref <- generate_model(dir, make_testmodels, n = 1, p = "a")
  sim <- new_simulation("mysim", "My simulation", dir = dir)
  sim2 <- generate_model(sim, make_testmodels, n = 1, p = "a")
  expect_identical(load(mref)@params, model(sim2)@params)
  # a sequence of models
  mref <- generate_model(dir, make_testmodels, vary_along = c("n", "p"),
                         n = as.list(1:2), p = as.list(letters[1:2]))
  sim2 <- generate_model(sim, make_testmodels, vary_along = c("n", "p"),
                        n = as.list(1:2), p = as.list(letters[1:2]))
  f <- function(m) m@params
  expect_identical(lapply(load(mref), f), lapply(model(sim2), f))
  # draws
  dref <- simulate_from_model(mref, nsim = 1, index = 1:2)
  sim3 <- simulate_from_model(sim2, nsim = 1, index = 1:2)
  expect_identical(load(dref), draws(sim3))
  # outputs
  oref <- run_method(dref, list(my_method))
  sim4 <- run_method(sim3, list(my_method))
  expect_identical(load(oref), output(sim4))
  # evals
  eref <- evaluate(oref, list(l1_error, linf_error))
  sim5 <- evaluate(sim4, list(l1_error, linf_error))
  expect_identical(load(eref), evals(sim5))
  unlink(dir, recursive = TRUE)
})
