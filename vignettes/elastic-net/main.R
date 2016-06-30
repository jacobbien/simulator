# This is the main simulator file

library(simulator) # this file was created under simulator version 0.0.0.9000

source("model_functions.R")
source("method_functions.R")
source("eval_functions.R")

## @knitr init

name_of_simulation <- "elastic-net"

## @knitr main

sim <- new_simulation(name_of_simulation, "Elastic Nets") %>%
  generate_model(make_sparse_linear_model_with_corr_design,
                 n = 100, p = 50, snr = 2, k = 10,
                 rho = as.list(seq(0, 0.8, length = 6)),
                 vary_along = "rho") %>%
  simulate_from_model(nsim = 3, index = 1:4) %>%
  run_method(list_of_elastic_nets,
             parallel = list(socket_names = 2, libraries = "glmnet")) %>%
  evaluate(list(sqr_err, nnz, best_sqr_err))

## @knitr maincv

sim_cv <- sim %>% subset_simulation(methods = "") %>%
  rename("elastic-net-cv") %>%
  relabel("Elastic Nets with CV") %>%
  run_method(list_of_elastic_nets + cv,
             parallel = list(socket_names = 2, libraries = "glmnet")) %>%
  evaluate(list(sqr_err, nnz))

## @knitr plots

## @knitr plotscv

plot_eval_by(sim_cv, "sqr_err", varying = "rho", include_zero = TRUE)

## @knitr tables

