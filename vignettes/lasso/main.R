
## @knitr main1

sim <- new_simulation("bet-on-sparsity", "Bet on sparsity") %>%
  generate_model(make_sparse_linear_model, n = 200, p = 500, snr = 2,
                 k = as.list(seq(5, 80, by = 5)),
                 vary_along = "k") %>%
  simulate_from_model(nsim = 2, index = 1:2) %>%
  run_method(list(lasso, ridge),
             parallel = list(socket_names = 2, libraries = "glmnet")) %>%
  evaluate(list(sqrerr, nnz, df, best_sqrerr))

## @knitr main2

sim2 <- sim %>%
  subset_simulation(methods = "lasso") %>%
  rename("relaxing-the-lasso") %>%
  relabel("Effect of relaxing lasso") %>%
  run_method(methods = lasso + refit) %>%
  evaluate(list(sqrerr, nnz, df, best_sqrerr))

## @knitr main3

sim3 <- sim %>% subset_simulation(methods = "") %>%
  rename("bet-on-sparsity-cv") %>%
  relabel("Bet on sparsity (with cross validation)") %>%
  run_method(list(lasso + cv, ridge + cv)) %>%
  evaluate(list(sqrerr, nnz))
