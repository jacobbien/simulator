## @knitr metrics

sqr_err <- new_metric(name = "sqrerr", label = "Squared Error Loss",
                      metric = function(model, out) {
                        mean((out$est - model$theta)^2)
                      })
