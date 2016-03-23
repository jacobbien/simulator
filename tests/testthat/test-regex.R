options(simulator.verbose = FALSE)

context("component name checking")

test_that("is_valid_component_name works", {
  expect_true(length(is_valid_component_name("a-2/1_b", "")) == 0)
  expect_true(length(is_valid_component_name("a.2/1b", "")) == 0)
  expect_true(length(is_valid_component_name("a.2.4-a.5/1b", "")) == 0)
  expect_true(length(is_valid_component_name("a,b", "")) == 1)
  expect_true(length(is_valid_component_name("a2/1%b", "")) == 1)
  expect_true(length(is_valid_component_name("/a2/1b", "")) == 1)
  expect_true(length(is_valid_component_name("a2/1b/", "")) == 1)
  expect_true(length(is_valid_component_name("a2//a", "")) == 1)
  expect_true(length(is_valid_component_name("a-_2/1b", "")) == 1)
  expect_true(length(is_valid_component_name("a2/_1b", "")) == 1)
  expect_true(length(is_valid_component_name("a2/_1b", "")) == 1)
  expect_true(length(is_valid_component_name("-a2/1b", "")) == 1)
  expect_true(length(is_valid_component_name("a2-/1b", "")) == 1)
  expect_true(length(is_valid_component_name("a2/1b_", "")) == 1)
  expect_true(length(is_valid_component_name("a2/1b/c/d/e/a", "")) == 0)
  expect_true(length(is_valid_component_name("a/a", "",
                                             allow_slash = FALSE)) == 1)
})
