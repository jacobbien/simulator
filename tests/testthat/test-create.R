options(simulator.verbose = FALSE)

context("create_template")

test_that("create makes a template that can be run without error", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  expect_error(create(file.path(dir, "hello")), NA)
  knitr::opts_knit$set(base.dir = file.path(dir, "hello"))
  a <- knitr::knit(file.path(dir, "hello", "writeup.Rmd"),
              output = file.path(dir, "hello/writeup.md"),
              quiet = TRUE)
  expect_false(any(grepl("error", readLines(a), ignore.case = TRUE)))
  # let's check that running twice gives same answer (testing caching logic)
  b <- knitr::knit(file.path(dir, "hello", "writeup.Rmd"),
                   output = file.path(dir, "hello/writeup_2ndtime.md"),
                   quiet = TRUE)
  # only the "generated on" line should differ (depending on time)
  aa <- readLines(a)
  bb <- readLines(b)
  expect_true(sum(aa != bb) %in% c(0, 1))
  unlink(dir, recursive = TRUE)
})
