## @knitr metrics

fdp <- new_metric(name = "fdp",
                  label = "false discovery proportion",
                  metric = function(model, out) {
                    fp <- setdiff(out$rejected, model$nonnull)
                    nd <- max(length(out$rejected), 1)
                    return(length(fp) / nd)
                    })

nd <- new_metric(name = "nd",
                 label = "number of discoveries",
                 metric = function(model, out) length(out$rejected))
