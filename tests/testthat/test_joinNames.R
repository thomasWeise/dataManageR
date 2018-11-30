library("dataManageR")
library("testthat")
context("datasets.names.join")

test_that("Test datasets.names.join", {
  expect_identical(datasets.names.join("a"), "a");
  expect_identical(datasets.names.join("a/n"), "a/n");
  expect_identical(datasets.names.join(c("a", "a")), "a");
  expect_identical(datasets.names.join(c("a/n", "a/n")), "a/n");
  expect_identical(datasets.names.join(c("a/b", "a/n")), "a");
  expect_identical(datasets.names.join(c("1/a", "2/a")), "a");
  expect_identical(datasets.names.join(c("1/a/b", "2/a/b")), "a/b");
  expect_identical(datasets.names.join(c("1/a/6/c/h/h/7", "2/a/8/c/e/h/9")), "a/c/h");
})
