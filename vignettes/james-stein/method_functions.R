## @knitr methods

mle <- new_method(name = "mle", label = "MLE",
                  method = function(model, draw) return(list(est = draw)))

js <- new_method(name = "jse", label = "James-Stein",
                 method = function(model, draw) {
                   l2 <- sum(draw^2)
                   return(list(est = (1 - (model$p - 2) / l2) * draw))
                 })
