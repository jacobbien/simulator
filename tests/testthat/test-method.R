options(simulator.verbose = FALSE)

context("run_method")

make_testmodel <- function() {
  return(new_model(name = "tm/n2",
             label = sprintf("Test model"),
             params = list(n = 2, x = runif(2)),
             simulate = function(x, n, nsim) {
               y <- list()
               for (i in seq(nsim))
                 y[[i]] <- x + rnorm(n)
               return(y)
             }))
}

my_method_no_list <- new_method(name = "mynl",
                                label = "My method",
                                method = function(model, draw) {
                                  return(mean(draw))
                                })
my_method <- new_method(name = "my", label = "My method",
                        method = function(model, draw) {
                          return(list(est = mean(draw), f = draw[1]))
                        })

remove_time_from_out <- function(out) {
  for (i in seq(length(out@out))) out@out[[i]] <- out@out$time <- 0
}

test_that("run_method handles multiple indices as expected", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodel)
  dref <- simulate_from_model(mref, 2, 1:3)
  out1 <- run_method(dref[1:2], my_method)
  out2 <- load_outputs(dir, "tm/n2", 1:2, "my")
  expect_identical(load(out1), out2)
  run_method(dref[2:3], my_method)
  out3 <- load_outputs(dir, "tm/n2", 2, "my")
  out4 <- load(dref[[2]])
  expect_identical(remove_time_from_out(out3),
                   remove_time_from_out(out2))
  unlink(dir, recursive = TRUE)
})

test_that("run_method handles multiple methods as expected", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  mref <- generate_model(dir, make_testmodel)
  dref <- simulate_from_model(mref, nsim = 2, 1:2)
  run_method(dref, my_method)
  run_method(dref, my_method_no_list)
  out <- load_outputs(dir, "tm/n2", 2, "my")
  outt <- load_outputs(dir, "tm/n2", 2, "mynl")

  run_method(dref, list(my_method, my_method_no_list))
  out2 <- load_outputs(dir, "tm/n2", 2, "my")
  outt2 <- load_outputs(dir, "tm/n2", 2, "mynl")
  expect_identical(remove_time_from_out(out),
                   remove_time_from_out(out2))
  expect_identical(remove_time_from_out(outt),
                   remove_time_from_out(outt2))
  unlink(dir, recursive = TRUE)
})
