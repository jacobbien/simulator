options(simulator.verbose = FALSE)

context("simulator.files")

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


test_that("simulator.files is managed properly", {
  dir <- file.path(tempdir(), "example")
  sf <- "standard_name_for_files"
  sf2 <- "where_my_files_are_stored"
  options(simulator.files = sf)
  if (!dir.exists(dir)) dir.create(dir)
  # generate_model using sf
  mref <- generate_model(dir, make_testmodel)
  model <- load(mref)
  # now change to sf2
  options(simulator.files = sf2)
  model2 <- load(mref)
  expect_equal(model, model2) # not identical b/c function enivronments differ
  expect_output(show(mref), sf) # show(mref) should show simulator.files
  expect_error(simulate_from_model(mref, nsim = 1), "match getOption")
  options(simulator.files = sf)
  dref <- simulate_from_model(mref, nsim = 1)
  draws <- load(dref)
  options(simulator.files = sf2)
  draws2 <- load(dref)
  expect_identical(draws, draws2)
  expect_output(show(dref), sf) # show(dref) should show simulator.files
  expect_error(run_method(dref, my_method), "match getOption")
  options(simulator.files = sf)
  oref <- run_method(dref, my_method)
  options(simulator.files = sf2)
  expect_output(show(oref), sf) # show(oref) should show simulator.files
  expect_error(evaluate(oref, squared_error), "match getOption")
  options(simulator.files = sf)
  eref <- evaluate(oref, squared_error)
  unlink(dir, recursive = TRUE)
})
