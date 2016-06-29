## @knitr methods
make_bh <- function(q) {
  # q is the desired level of control for the FDR
  new_method(name = paste0("bh", q),
             label = sprintf("BH (q = %s)", q),
             settings = list(q = q),
             method = function(model, draw, q) {
               p <- sort(draw)
               cutline <- seq(model$n) * q / model$n
               threshold <- max(p[p <= cutline], 0)
               list(rejected = which(draw <= threshold))
             })
}

qvalues <- c(0.05, 0.1, 0.2)
bh_methods <- sapply(qvalues, make_bh)
