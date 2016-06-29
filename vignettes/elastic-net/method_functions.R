
## @knitr methods

library(glmnet)
make_elastic_net <- function(alpha) {
  new_method(name = sprintf("en%s", alpha),
             label = sprintf("EN(a = %s)", alpha),
             settings = list(alpha = alpha, nlam = 50),
             method = function(model, draw, alpha, nlam, lambda = NULL) {
                      if (is.null(lambda))
                        fit <- glmnet(x = model$x, y = draw, alpha = alpha,
                                      nlambda = nlam, intercept = FALSE)
                      else
                        fit <- glmnet(x = model$x, y = draw, alpha = alpha,
                                      lambda = lambda, intercept = FALSE)
                      list(beta = fit$beta, yhat = model$x %*% fit$beta,
                           lambda = fit$lambda, df = fit$df, alpha = alpha)
                    })
}

list_of_elastic_nets <- sapply(c(0, 0.5, 1), make_elastic_net)


## @knitr cv

#' Make folds for cross validation
#'
#' Divides the indices \code{1:n} into \code{nfolds} random folds of about the same size.
#'
#' @param n sample size
#' @param nfolds number of folds
make_folds <- function(n, nfolds) {
  nn <- round(n / nfolds)
  sizes <- rep(nn, nfolds)
  sizes[nfolds] <- sizes[nfolds] + n - nn * nfolds
  b <- c(0, cumsum(sizes))
  ii <- sample(n)
  folds <- list()
  for (i in seq(nfolds))
    folds[[i]] <- ii[seq(b[i] + 1, b[i + 1])]
  folds
}

cv <- new_method_extension("cv", "cross validated",
                           method_extension = function(model, draw, out,
                                                       base_method) {
                             nfolds <- 5
                             alpha <- base_method@settings$alpha
                             err <- matrix(NA, ncol(out$beta), nfolds)
                             ii <- make_folds(model$n, nfolds)
                             for (i in seq_along(ii)) {
                               train <- model
                               train@params$x <- model@params$x[-ii[[i]], ]
                               train@params$n <- model@params$x[-ii[[i]], ]
                               train_draw <- draw[-ii[[i]]]

                               test <- model
                               test@params$x <- model@params$x[ii[[i]], ]
                               test@params$n <- model@params$x[ii[[i]], ]
                               test_draw <- draw[ii[[i]]]
                               fit <- base_method@method(model = train,
                                                         draw = train_draw,
                                                         alpha = alpha,
                                                         lambda = out$lambda)
                               yhat <- test$x %*% fit$beta
                               ll <- seq(ncol(yhat))
                               err[ll, i] <- colMeans((yhat - test_draw)^2)
                             }
                             m <- rowMeans(err)
                             se <- apply(err, 1, sd) / sqrt(nfolds)
                             imin <- which.min(m)
                             ioneserule <- max(which(m <= m[imin] + se[imin]))
                             list(err = err, m = m, se = se, imin = imin,
                                  ioneserule = ioneserule,
                                  beta = out$beta[, imin],
                                  yhat = model$x %*% out$beta[, imin],
                                  alpha = alpha)
                           })

