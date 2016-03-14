options(simulator.verbose = FALSE)

context("create_template")

test_that("create makes a template that can be run without error", {
  dir <- file.path(tempdir(), "example")
  if (!dir.exists(dir)) dir.create(dir)
  expect_error(not(create(file.path(dir, "hello"))))
  a <- knitr::knit(file.path(dir, "hello", "writeup.Rmd"),
              output = file.path(dir, "hello/writeup.md"))
  expect_false(any(grepl("ERROR", readLines(a))))
  unlink(dir, recursive = TRUE)
})
