options(simulator.verbose = FALSE)

context("Parallel")

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


test_that("seeds is deterministic", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  generate_model(make_testmodel, dir = dir)
  model_seed <- load_model(dir, "tm", more_info = TRUE)$rng$rng_seed
  seeds <- get_seeds_for_draws(model_seed, 1:4)
  seeds2 <- get_seeds_for_draws(model_seed, 1:4)
  expect_identical(seeds, seeds2)

  seeds3 <- get_seeds_for_draws(model_seed, c(2,4))
  expect_identical(seeds[c(2,4)], seeds3[1:2])
  unlink(dir, recursive = TRUE)
})

test_that("get same draws if chunks done together or separately", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  tryCatch({
    generate_model(make_testmodel, dir = dir)
    # done together:
    simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:4)
    draws <- load_draws(dir, "tm", 1:4)
    file.remove(file.path(dir, sprintf("files/tm/r%s.Rdata", 1:4)))
    # done separately, and out of order:
    simulate_from_model(dir, model_name = "tm", nsim = 2, index = 3:4)
    simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:2)
    draws2 <- load_draws(dir, "tm", 1:4)
  }, finally={sink()})
  expect_identical(draws, draws2)
  unlink(dir, recursive = TRUE)
})

test_that("simulate_from_model output same parallel v. sequential", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  generate_model(make_testmodel, dir = dir)
  # in sequence:
  simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:5)
  seqdraws <- load_draws(dir, "tm", 1:5)
  file.remove(file.path(dir, sprintf("files/tm/r%s.Rdata", 1:5)))
  simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:5,
                      parallel = list(socket_names = 2, save_locally = FALSE))
  pardraws <- load_draws(dir, "tm", 1:5)
  expect_identical(seqdraws, pardraws)

  file.remove(file.path(dir, sprintf("files/tm/r%s.Rdata", 1:5)))
  simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:5,
                      parallel = list(socket_names = 2, save_locally = TRUE))
  pardraws2 <- load_draws(dir, "tm", 1:5)
  expect_identical(pardraws, pardraws2)
  unlink(dir, recursive = TRUE)
})
