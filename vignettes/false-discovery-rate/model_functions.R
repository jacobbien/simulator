## @knitr models
library(mvtnorm)
make_correlated_pvalues <- function(n, pi0, rho) {
  # Gaussian copula model...
  #
  # n pvalues, the first n*pi0 of which are null, coming from a multivariate
  # normal with all correlations rho.
  sigma <- matrix(rho, n, n)
  diag(sigma) <- 1
  n0 <- round(n * pi0)
  delta <- 2 # size of signal
  mu <- rep(c(0, delta), c(n0, n - n0)) # n0 are null
  new_model(name = "correlated-pvalues",
            label = sprintf("pi0 = %s, rho = %s", pi0, rho),
            params = list(n = n, rho = rho, sigma = sigma,
                          pi0 = pi0, mu = mu, delta = delta,
                          nonnull = which(mu != 0)),
            simulate = function(n, mu, sigma, nsim) {
              # this function must return a list of length nsim
              x <- rmvnorm(nsim, mean = mu, sigma = sigma)
              pvals <- 1 - pnorm(x)
              return(split(pvals, row(pvals))) # make each row its own list element
            })
}
