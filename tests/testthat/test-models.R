options(simulator.verbose = FALSE)

context("generate_model")

make_regmodel <- function(n, p, sigma) {
  x <- matrix(rnorm(n * p), n, p)
  beta <- runif(p)
  return(new_model(name = "reg",
             label = "Regression model",
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
             label = "Regression model",
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
  expect_warning(generate_model(dir, make_regmodel, n = 5, p = 2, sigma = 2),
                 "sets n to a value different from")
  expect_error(generate_model(dir, make_regmodel, n = 1, p = 2,
                                  sigma = 2), NA)
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
                              n = as.list(c(2, 4)), p = 4), "sigma")
  expect_warning(generate_model(dir, make_regmodel2, vary_along = "n",
                                  n = as.list(c(2, 4)), p = 4, sigma = 1),
                 "Labels are not unique")
  expect_error(generate_model(dir, make_regmodel2, vary_along = "n",
                              n = as.list(c(2, 4)), p = as.list(c(2, 10)),
                              sigma = 1), "non-numeric argument")
  expect_warning(generate_model(dir, make_regmodel2, vary_along = c("n", "p"),
                              n = as.list(c(2, 4)), p = as.list(c(2, 3, 10)),
                              sigma = 1), "Labels are not unique")
  model1 <- load_model(dir = dir, "reg2/n_2/p_10/sigma_1")
  generate_model(dir, make_regmodel2, n = 2, p = 10, sigma = 1)
  model2 <- load_model(dir = dir, "reg2/n_2/p_10/sigma_1")
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
  class(m) <- "list"
  expect_equal(list(m1, m2), m)
  unlink(dir, recursive = TRUE)
})


test_that("loading ModelRef works", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_regmodel2, n = 5, p = 2, sigma = 2)
  model1 <- load(mref)
  model2 <- load_model(dir, "reg2/n_5/p_2/sigma_2")
  expect_equal(model1, model2) # not identical since function enivronments differ
  unlink(dir, recursive = TRUE)
})

test_that("subsetting models works", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  expect_warning({sim <- new_simulation("test", "test", dir = dir) %>%
    generate_model(make_regmodel2, n = as.list(1:5), p = as.list(10:12),
                   vary_along = c("n", "p"), sigma = "a")})
  m <- model(sim)
  expect_equal(model(sim, n==2 & p == 12),
               model(sim, n==2 & p == 12, subset = 12))
  expect_equal(unlist(model(sim, subset = 10:12)), m[10:12])
  expect_equal(model(sim, n == 4), m[c(4, 9, 14)])
  expect_equal(model(sim, n == 4 & sigma == "a"), m[c(4, 9, 14)])
  expect_equal(model(sim, n == 4 & sigma == "b"), list())
  expect_equal(model(sim, n < 4 & p < 11), m[1:3])
  expect_error(model(sim, n < 4 & p < 11 & b == 2), "not found")
  unlink(dir, recursive = TRUE)
})
