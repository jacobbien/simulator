
## @knitr metrics

sqr_err <- new_metric("sqr_err", "squared error",
                  metric = function(model, out) {
                    colMeans(as.matrix(out$beta - model$beta)^2)
                  })

best_sqr_err <- new_metric("best_sqr_err", "best squared error",
                      metric = function(model, out) {
                        min(colMeans(as.matrix(out$beta - model$beta)^2))
                      })

nnz <- new_metric("nnz", "number of nonzeros",
                  metric = function(model, out) {
                    colSums(as.matrix(out$beta) != 0)
                  })
