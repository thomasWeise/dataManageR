library("dataManageR")
library("testthat")
context("datasets.names.namer")

test_that("Test datasets.names.namer", {
  expect_identical(datasets.names.namer("a"), "a");
  expect_identical(datasets.names.namer(c("a")), "a");
  expect_identical(datasets.names.namer(list("a")), "a");
  expect_identical(datasets.names.namer(c("a", "n")), "a/n");
  expect_identical(datasets.names.namer(list("a", "n")), "a/n");
  expect_identical(datasets.names.namer(c("a", "n", "a")), "a/n/a");
  expect_identical(datasets.names.namer(list("a", "n", "a")), "a/n/a");

  expect_identical(datasets.names.namer(NULL), "unnamed");
  expect_identical(datasets.names.namer(""), "unnamed");
  expect_identical(datasets.names.namer(c("")), "unnamed");
  expect_identical(datasets.names.namer(list("")), "unnamed");
  expect_identical(datasets.names.namer(character(0)), "unnamed");

  expect_identical(datasets.names.namer(c("a", "", "b")), "a/b");
})
