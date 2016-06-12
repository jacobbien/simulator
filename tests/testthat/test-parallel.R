options(simulator.verbose = FALSE)

context("Parallel")

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

my_mean <- function(x) mean(x) # this must be exported to slaves

my_method <- new_method(name = "my",
                        label = "My method",
                        method = function(model, draw) {
                          return(list(est = my_mean(draw),
                                      f = draw[1]))
                        })

other_method <- new_method(name = "om",
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
  generate_model(dir, make_testmodel)
  model_seed <- load_model(dir, "tm", more_info = TRUE)$rng$rng_seed
  seeds <- get_seeds_for_draws(model_seed, 1:4)
  seeds2 <- get_seeds_for_draws(model_seed, 1:4)
  expect_identical(seeds, seeds2)

  seeds3 <- get_seeds_for_draws(model_seed, c(2,4))
  expect_identical(seeds[c(2,4)], seeds3[1:2])
  unlink(dir, recursive = TRUE)
})

test_that("get same draws if chunks done together or separately", {
  skip_on_cran() # http://r-pkgs.had.co.nz/tests.html says risky to
  # test parallel code on CRAN
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodel)
  # done together:
  dref <- simulate_from_model(mref, nsim = 2, index = 1:4)
  draws <- lapply(dref, load)
  file.remove(file.path(dir, options("simulator.files"),
                        sprintf("tm/r%s.Rdata", 1:4)))
  # done separately, and out of order:
  dref2a <- simulate_from_model(mref, nsim = 2, index = 3:4)
  dref2b <- simulate_from_model(mref, nsim = 2, index = 1:2)
  draws2 <- c(lapply(dref2b, load), lapply(dref2a, load))
  expect_identical(draws, draws2)
  unlink(dir, recursive = TRUE)
})

test_that("simulate_from_model output same parallel v. sequential", {
  skip_on_cran() # http://r-pkgs.had.co.nz/tests.html says risky to
  # test parallel code on CRAN
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodel)
  # in sequence:
  dref <- simulate_from_model(mref, nsim = 2, index = 1:5)
  seqdraws <- load(dref)
  file.remove(file.path(dir, options("simulator.files"),
                        sprintf("tm/r%s.Rdata", 1:5)))
  dref <- simulate_from_model(mref, nsim = 2, index = 1:5,
                      parallel = list(socket_names = 2, save_locally = FALSE))
  pardraws <- load(dref)
  expect_identical(seqdraws, pardraws)

  file.remove(file.path(dir, options("simulator.files"),
                        sprintf("tm/r%s.Rdata", 1:5)))
  dref2 <- simulate_from_model(mref, nsim = 2, index = 1:5,
                              parallel = list(socket_names = 2,
                                              save_locally = TRUE))
  pardraws2 <- load(dref2)
  expect_identical(pardraws, pardraws2)
  unlink(dir, recursive = TRUE)
})

test_that("run_method output same parallel v. sequential", {
  skip_on_cran() # http://r-pkgs.had.co.nz/tests.html says risky to
  # test parallel code on CRAN
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodel)
  dref <- simulate_from_model(mref, nsim = 2, index = 1:5)
  # in sequence:
  oref <- run_method(dref[3:5], my_method)
  seqout <- load(oref[[2]])
  file.remove(file.path(dir, options("simulator.files"),
                        sprintf("tm/out/r%s_my.Rdata", 3:5)))
  run_method(dref[3:5], my_method,
             parallel = list(socket_names = 2, save_locally = FALSE))
  parout <- load_outputs(dir, "tm", 4, method_name = "my")
  expect_identical(remove_time_from_out(seqout),
                   remove_time_from_out(parout))
  file.remove(file.path(dir, options("simulator.files"),
                        sprintf("tm/out/r%s_my.Rdata", 3:5)))
  run_method(dref[3:5], list(my_method, other_method),
             parallel = list(socket_names = 3, save_locally = TRUE))
  parout2 <- load_outputs(dir, "tm", 4, method_name = "my")
  expect_identical(remove_time_from_out(parout),
                   remove_time_from_out(parout2))

  unlink(dir, recursive = TRUE)
})
