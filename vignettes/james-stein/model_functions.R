## @knitr models

make_normal_model <- function(theta_norm, p) {
  new_model(name = "norm",
            label = sprintf("p = %s, theta_norm = %s", p, theta_norm),
            params = list(theta_norm = theta_norm, p = p,
                          theta = c(theta_norm, rep(0, p - 1))),
            simulate = function(theta, p, nsim) {
              Y <- theta + matrix(rnorm(nsim * p), p, nsim)
              return(split(Y, col(Y))) # make each col its own list element
            })
}
