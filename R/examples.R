#' @include model-class.R method-class.R metric-class.R
NULL

#' Make My Example Model
#'
#' This function is used in the examples.  It returns a
#' \code{\linkS4class{Model}} object.  In particular, it represents
#' \code{n} i.i.d. draws from a normal with mean 2 and variance 1.
#'
#' @param n number of i.i.d. draws
#'
#' @export
#' @seealso \code{\link{my_example_method}} \code{\link{my_example_loss}}
make_my_example_model <- function(n) {
  new_model(name = "normal-data",
            label = sprintf("Normal (n = %s)", n),
            params = list(n = n, mu = 2),
            simulate = function(n, mu, nsim) {
              # this function must return a list of length nsim
              x <- matrix(rnorm(n * nsim), n, nsim)
              x <- mu + x # true mean is mu
              return(split(x, col(x))) # make each col its own list element
            })
}

#' My Example Method
#'
#' This \code{\linkS4class{Method}} object is used in the examples.  It is
#' the sample mean of the data.
#' @export
#' @seealso \code{\link{make_my_example_model}} \code{\link{my_example_loss}}
my_example_method <- new_method("my-method", "My Example Method",
                        method = function(model, draw) {
                          list(fit = mean(draw))
                          })

#' My Example Loss
#'
#' This \code{\linkS4class{Metric}} object is used in the examples.  It is
#' squared error loss.
#' @export
#' @seealso \code{\link{make_my_example_model}} \code{\link{my_example_loss}}
my_example_loss <- new_metric("myloss", "My Example Loss",
                         metric = function(model, out) {
                           return((model$mu - out$fit)^2)
                         })

