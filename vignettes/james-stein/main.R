# This is the main simulator file

library(simulator) # this file was created under simulator version 0.0.0.9000

source("model_functions.R")
source("method_functions.R")
source("eval_functions.R")

## @knitr init

## @knitr main1

sim1 <- new_simulation(name = "js-v-mle",
                       label = "Investigating the James-Stein Estimator") %>%
  generate_model(make_normal_model, theta_norm = 1, p = list(2, 6),
                 vary_along = "p", seed = 123) %>%
  simulate_from_model(nsim = 20) %>%
  run_method(list(js, mle)) %>%
  evaluate(sqr_err)

## @knitr main2

sim2 <- new_simulation(name = "js-v-mle2",
                       label = "Investigating James-Stein Estimator") %>%
  generate_model(make_normal_model, vary_along = "p",
                 theta_norm = 1, p = as.list(seq(1, 30, by = 5))) %>%
  simulate_from_model(nsim = 20) %>%
  run_method(list(js, mle)) %>%
  evaluate(sqr_err)

## @knitr main3

sim3 <- new_simulation(name = "js-v-mle3",
                       label = "Investigating the James-Stein Estimator") %>%
  generate_model(make_normal_model, vary_along = c("p", "theta_norm"),
                 theta_norm = as.list(round(seq(0, 5, length = 10), 2)),
                 p = as.list(seq(1, 30, by = 10))) %>%
  simulate_from_model(nsim = 20) %>%
  run_method(list(js, mle)) %>%
  evaluate(sqr_err)
