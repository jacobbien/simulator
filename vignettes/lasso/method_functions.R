
## @knitr methods

library(glmnet)
lasso <- new_method("lasso", "Lasso",
                    method = function(model, draw, lambda = NULL) {
                      if (is.null(lambda))
                        fit <- glmnet(x = model$x, y = draw, nlambda = 50,
                                      intercept = FALSE)
                      else {
                        fit <- glmnet(x = model$x, y = draw, lambda = lambda,
                                      intercept = FALSE)
                      }
                      list(beta = fit$beta, yhat = model$x %*% fit$beta,
                           lambda = fit$lambda, df = fit$df)
                    })

ridge <- new_method("ridge", "Ridge",
                    method = function(model, draw, lambda = NULL) {
                      sv <- svd(model$x)
                      df_fun <- function(lam) {
                        # degrees of freedom when tuning param is lam
                        sum(sv$d^2 / (sv$d^2 + lam))
                      }
                      if (is.null(lambda)) {
                        nlambda <- 50
                        get_lam <- function(target_df) {
                          f <- function(lam) df_fun(lam) - target_df
                          uniroot(f, c(0, 100 * max(sv$d^2)))$root
                        }
                        lambda <- sapply(seq(1, nrow(model$x),
                                             length = nlambda), get_lam)
                      }
                      df <- sapply(lambda, df_fun)
                      beta <- sapply(lambda, function(r) {
                        d <- sv$d / (sv$d^2 + r)
                        return(sv$v %*% (d * crossprod(sv$u, draw)))
                      })
                      list(beta = beta, yhat = model$x %*% beta,
                           lambda = lambda, df = df)
                    })

## @knitr refit

refit <- new_method_extension(name = "refit", label = "refitted",
                              method_extension = function(model, draw, out,
                                                          base_method) {
                                beta <- apply(out$beta, 2, function(b) {
                                  ii <- b != 0
                                  if (sum(ii) == 0)
                                    return(b)
                                  xtx <- crossprod(model$x[, ii])
                                  # add small ridge in case solution has more
                                  # than n nonzeros:
                                  diag(xtx) <- diag(xtx) + 1e-4
                                  bb <- solve(xtx,
                                              crossprod(model$x[, ii], draw))
                                  b[ii] <- bb
                                  return(b)
                                })
                                return(list(beta = beta,
                                            yhat = model$x %*% beta,
                                            lambda = out$lambda,
                                            df = rep(NA, ncol(beta))))
                              })

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
                                  yhat = model$x %*% out$beta[, imin])
                           })

