
## @knitr models

make_sparse_linear_model <- function(n, p, k, snr) {
  x <- matrix(rnorm(n * p), n, p)
  beta <- rep(c(1, 0), c(k, p - k))
  mu <- as.numeric(x %*% beta)
  sigma <- sqrt(sum(mu^2) / (n * snr)) # taking snr = ||mu||^2 / (n * sigma^2)
  new_model(name = "slm", label = sprintf("n = %s, p = %s, k = %s", n, p, k),
            params = list(x = x, beta = beta, mu = mu, sigma = sigma, n = n,
                          p = p, k = k),
            simulate = function(mu, sigma, nsim) {
              y <- mu + sigma * matrix(rnorm(nsim * n), n, nsim)
              return(split(y, col(y))) # make each col its own list element
            })
}
