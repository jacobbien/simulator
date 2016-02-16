options(simulator.verbose = FALSE)

context("draws related")

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

test_that("loading DrawsRef works (even with changing simulator.files", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  options(simulator.files = "hello")
  mref <- generate_model(dir, make_regmodel2, n = 5, p = 2, sigma = 2)
  dref <- simulate_from_model(mref, nsim = 2, index = 1:3)
  options(simulator.files = "files")
  draws1 <- load_draws(dir, "reg2", 3, simulator.files = "hello")
  draws2 <- load(dref[[3]])
  expect_identical(draws1, draws2)
  unlink(dir, recursive = TRUE)
})

