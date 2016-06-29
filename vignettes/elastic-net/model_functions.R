
## @knitr models

library(mvtnorm)
make_sparse_linear_model_with_corr_design <- function(n, p, k, snr, rho) {
  sig <- matrix(rho, p, p)
  diag(sig) <- 1
  x <- rmvnorm(n, sigma = sig)
  beta <- rep(c(1, 0), c(k, p - k))
  mu <- as.numeric(x %*% beta)
  sigma <- sqrt(sum(mu^2) / (n * snr)) # taking snr = ||mu||^2 / (n * sigma^2)
  new_model(name = "slm", label = sprintf("rho = %s", rho),
            params = list(x = x, beta = beta, mu = mu, sigma = sigma, n = n,
                          p = p, k = k),
            simulate = function(mu, sigma, nsim) {
              y <- mu + sigma * matrix(rnorm(nsim * n), n, nsim)
              return(split(y, col(y))) # make each col its own list element
            })
}
