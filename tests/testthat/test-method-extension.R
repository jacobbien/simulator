options(simulator.verbose = FALSE)

context("method-extension")

make_testmodel <- function() {
  return(new_model(name = "tm",
                   label = "Test model",
                   params = list(n = 20, mu = rep(c(0, 2), c(15, 5))),
                   simulate = function(mu, n, nsim) {
                     y <- list()
                     for (i in seq(nsim))
                       y[[i]] <- mu + 0.5 * rnorm(n)
                     return(y)
                   }))
}

soft <- new_method("st", "Soft threshold",
                   method = function(model, draw) {
                     lamlist <- seq(max(model$mu), 0, length = 10)
                     st <- function(lam) sign(draw) * pmax(draw - lam, 0)
                     list(fit = sapply(lamlist, st))
                   })

refit <- new_method_extension(name = "refit", label = "refitted",
                               method_extension = function(model, draw, out,
                                                          base_method) {
                                 fit <- apply(out$fit, 2, function(a) {
                                   ii <- which(a != 0)
                                   a[ii] <- draw[ii]
                                   return(a)
                                   })
                                list(fit = fit)
                              })



mse <- new_metric("mse", "MSE", metric = function(model, out) {
  colSums(as.matrix((model$mu - out$fit)^2))
})


test_that("extend a method", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  sim <- new_simulation("sparse_vector", "Estimation of a sparse vector", dir = dir)
  sim <- generate_model(sim, make_testmodel)
  sim <- simulate_from_model(sim, nsim = 2, index = 1:4)
  expect_error(run_method(sim, list(soft + refit)), "Could not find output of method")
  sim <- run_method(sim, soft)
  sim <- run_method(sim, soft + refit)
  #sim <- run_method(sim, list(soft, soft + refit))
  sim <- evaluate(sim, mse)
  unlink(dir, recursive = TRUE)
})

