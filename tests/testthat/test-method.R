options(simulator.verbose = FALSE)

context("run_methods")

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

my_method_no_list <- new("Method",
                         name = "mynl",
                         label = "My method",
                         method = function(model, draw) {
                           return(mean(draw))
                         })
my_method <- new("Method",
                 name = "my",
                 label = "My method",
                 method = function(model, draw) {
                   return(list(est = mean(draw),
                               f = draw[1]))
                 })

remove_time_from_out <- function(out) {
  for (i in seq(length(out@out))) out@out[[i]] <- out@out$time <- 0
}

test_that("run_methods handles multiple indices as expected", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  generate_model(make_testmodel, dir = dir)
  simulate_from_model(dir, "tm", 2, 1:3)
  run_method(my_method, dir, "tm", index=1:2)
  out <- load_outputs(dir, "tm", 2, "my")
  run_method(my_method, dir, "tm", index=2:3)
  out2 <- load_outputs(dir, "tm", 2, "my")
  expect_identical(remove_time_from_out(out),
                   remove_time_from_out(out2))
  unlink(dir, recursive = TRUE)
})
