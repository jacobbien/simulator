# Got this Sys.setenv line from https://github.com/luckyrandom/cmaker/blob/b85813ac2b7aef69932eca8fbb4fa0ec225e0af0/tests/testthat.R
# so that devtools:check() won't hang.
# setting R_TESTS to empty string because of
# https://github.com/hadley/testthat/issues/144
# revert this when that issue in R is fixed.
Sys.setenv("R_TESTS" = "")

library(testthat)
library(simulator)

test_check("simulator")
