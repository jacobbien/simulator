
## @knitr metrics

sqrerr <- new_metric("sqrerr", "squared error",
                  metric = function(model, out) {
                    colMeans(as.matrix(out$beta - model$beta)^2)
                  })

best_sqrerr <- new_metric("best_sqrerr", "best squared error",
                      metric = function(model, out) {
                        min(colMeans(as.matrix(out$beta - model$beta)^2))
                      })

nnz <- new_metric("nnz", "number of nonzeros",
                  metric = function(model, out) {
                    colSums(as.matrix(out$beta) != 0)
                  })

df <- new_metric("df", "degrees of freedom",
                 metric = function(model, out) out$df)
