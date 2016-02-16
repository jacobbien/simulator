options(simulator.verbose = FALSE)

context("generate_model")

make_regmodel <- function(n, p, sigma) {
  x <- matrix(rnorm(n * p), n, p)
  beta <- runif(p)
  return(new_model(name = "reg",
             label = sprintf("Regression model"),
             params = list(x = x, beta = beta, signal = x %*% beta,
                           n = 1, sigma = sigma),
             simulate = function(signal, n, sigma, nsim) {
               y <- list()
               for (i in seq(nsim))
                 y[[i]] <- signal + sigma * rnorm(n)
               return(y)
             }))
}

make_regmodel2 <- function(n, p, sigma) {
  x <- matrix(rnorm(n * p), n, p)
  beta <- runif(p)
  return(new_model(name = "reg2",
             label = sprintf("Regression model"),
             params = list(x = x, beta = beta, signal = x %*% beta,
                           n = n, sigma = sigma),
             simulate = function(signal, n, sigma, nsim) {
               y <- list()
               for (i in seq(nsim))
                 y[[i]] <- signal + sigma * rnorm(n)
               return(y)
             }))
}

test_that("generate_model works with ... when vary_params = NULL", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  expect_error(generate_model(dir, make_regmodel), "missing")
  expect_error(generate_model(dir, make_regmodel, n = 5, p = 2), "missing")
  expect_error(not(generate_model(dir, make_regmodel, n = 5, p = 2,
                                  sigma = 2)))
  expect_warning(generate_model(dir, make_regmodel, n = 5, p = 2, sigma = 2),
                 "sets n to a value different from")
  expect_error(not(generate_model(dir, make_regmodel, n = 1, p = 2,
                                  sigma = 2)))
  unlink(dir, recursive = TRUE)
})

test_that("generate_model works when vary_params in non-NULL", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  expect_error(generate_model(dir, make_regmodel, vary_along = "n"),
               "vary_along")
  expect_error(generate_model(dir, make_regmodel, vary_along = "n", n = 2,
                              p = 4), "list")
  expect_error(generate_model(dir, make_regmodel, vary_along = "n",
                              n = as.list(c(2,4)), p = 4), "sigma")
  expect_error(not(generate_model(dir, make_regmodel2, vary_along = "n",
                                  n = as.list(c(2,4)), p = 4, sigma = 1)))
  expect_error(generate_model(dir, make_regmodel2, vary_along = "n",
                              n = as.list(c(2,4)), p = as.list(c(2, 10)),
                              sigma = 1), "non-numeric argument")
  generate_model(dir, make_regmodel2, vary_along = c("n", "p"),
                              n = as.list(c(2,4)), p = as.list(c(2, 3, 10)),
                              sigma = 1)
  model1 <- load_model(dir = dir, "reg2/n_7fb7dc4afcf762daf6636854257587a1c8e7b144/p_710ded514e2bf24f2703c031577d5b5e10c1963a")
  generate_model(dir, make_regmodel2, n = 2, p = 10, sigma = 1)
  model2 <- load_model(dir = dir, "reg2")
  expect_identical(model1@params, model2@params)
  unlink(dir, recursive = TRUE)
})

test_that("generate_model works when make_model is a list", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  m1 <- load(generate_model(dir, make_regmodel, n = 1, p = 1, sigma = 1))
  m2 <- load(generate_model(dir, make_regmodel2, n = 1, p = 1, sigma = 1))
  m <- load(generate_model(dir, list(make_regmodel, make_regmodel2), n = 1,
                          p = 1, sigma = 1))
  expect_equal(list(m1, m2), m)
  unlink(dir, recursive = TRUE)
})


test_that("loading ModelRef works", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_regmodel2, n = 5, p = 2, sigma = 2)
  model1 <- load(mref)
  model2 <- load_model(dir, "reg2")
  expect_equal(model1, model2) # not identical since function enivronments differ
  unlink(dir, recursive = TRUE)
})
