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
  seeds <- get_seeds_for_draws(dir, "tm", 1:4)
  seeds2 <- get_seeds_for_draws(dir, "tm", 1:4)
  expect_identical(seeds, seeds2)

  seeds3 <- get_seeds_for_draws(dir, "tm", c(2,4))
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
  sink(file.path(dir,"out.txt"))
  tryCatch({
    generate_model(make_testmodel, dir = dir)
    # in sequence:
    simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:5)
    seqdraws <- load_draws(dir, "tm", 1:5)
    #simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:5,
    #                    parallel = list(socket_names = 2))
    simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:5)
    pardraws <- load_draws(dir, "tm", 1:5)
  }, finally={sink()})
  expect_identical(seqdraws, pardraws)
  unlink(dir, recursive = TRUE)
})
