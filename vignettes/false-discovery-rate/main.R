# This is the main simulator file

library(simulator) # this file was created under simulator version 0.0.0.9000

source("model_functions.R")
source("method_functions.R")
source("eval_functions.R")

## @knitr init

name_of_simulation <- "fdr"

## @knitr main

sim <- new_simulation(name = name_of_simulation,
                      label = "False Discovery Rate") %>%
  generate_model(make_correlated_pvalues, seed = 123,
                 n = 20,
                 pi0 = list(0.8, 1),
                 rho = list(-0.01, 0, 0.1, 0.9),
                 vary_along = c("pi0", "rho")) %>%
  simulate_from_model(nsim = 25, index = 1:4) %>%
  run_method(bh_methods, parallel = list(socket_names = 2)) %>%
  evaluate(list(fdp, nd))

## @knitr main2

sim <- sim %>%
  simulate_from_model(nsim = 25, index = 5:8) %>%
  run_method(bh_methods, parallel = list(socket_names = 2)) %>%
  evaluate(list(fdp, nd))

## @knitr main3

sim2 <- subset_simulation(sim, pi0 == 1 & rho == -0.01) %>%
  rename("fdr-negdep") %>%
  relabel("BH Procedure under negative dependence") %>%
  simulate_from_model(nsim = 25, index = 9:20) %>%
  run_method(bh_methods, parallel = list(socket_names = 2)) %>%
  evaluate(list(fdp, nd))

