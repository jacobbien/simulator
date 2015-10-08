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

my_method <- new("Method",
                 name = "my",
                 label = "My method",
                 method = function(model, draw) {
                   return(list(est = mean(draw),
                               f = draw[1]))
                 })

other_method <- new("Method",
                         name = "om",
                         label = "Other method",
                         method = function(model, draw) {
                           return(median(draw))
                         })

remove_time_from_out <- function(out) {
  for (i in seq(length(out@out))) out@out[[i]] <- out@out$time <- 0
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

test_that("run_method output same parallel v. sequential", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  generate_model(make_testmodel, dir = dir)
  simulate_from_model(dir, model_name = "tm", nsim = 2, index = 1:5)
  # in sequence:
  run_method(my_method, dir, model_name = "tm", index = 3:5)
  seqout <- load_outputs(dir, "tm", 4, method_name = "my")
  file.remove(file.path(dir, sprintf("files/tm/out/r%s_my.Rdata", 3:5)))
  run_method(my_method, dir, model_name = "tm", index = 3:5,
             parallel = list(socket_names = 2, save_locally = FALSE))
  parout <- load_outputs(dir, "tm", 4, method_name = "my")
  expect_identical(remove_time_from_out(seqout),
                   remove_time_from_out(parout))
  file.remove(file.path(dir, sprintf("files/tm/out/r%s_my.Rdata", 3:5)))
  run_method(list(my_method, other_method), dir, model_name = "tm",
             index = 3:5,
             parallel = list(socket_names = 3, save_locally = TRUE))
  parout2 <- load_outputs(dir, "tm", 4, method_name = "my")
  expect_identical(remove_time_from_out(parout),
                   remove_time_from_out(parout2))

  unlink(dir, recursive = TRUE)
})
