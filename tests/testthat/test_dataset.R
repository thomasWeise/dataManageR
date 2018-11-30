library("dataManageR")
library("testthat")
context("dataset")

test_that("Test dataset", {
  ds <- new("dataset", name="bla", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));

  expect_identical(ds@name, "bla");
  expect_identical(ds@features, list(c="c", a="a", b="b"));
  expect_identical(ds@data, list(7, 2, c(3, 4), list(a="b")));

  expect_error(new("dataset", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b"))));
  expect_error(new("dataset", name="bla", features=list(c="c", a="a", b="b", a=5), data= list(7, 2, c(3, 4), list(a="b"))));
  expect_error(new("dataset", name="", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b"))));
})


test_that("Test dataset.new", {
  ds <- dataset.new(name="bla", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));

  expect_identical(ds@name, "bla");
  expect_identical(ds@features, list(a="a", b="b", c="c"));
  expect_identical(ds@data, list(7, 2, c(3, 4), list(a="b")));

  expect_error(dataset.new(name="", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b"))));
  expect_error(dataset.new(features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b"))));
  expect_error(dataset.new(name="bla", features=list(c="c", a="a", c="d", b="b"), data= list(7, 2, c(3, 4), list(a="b"))));
  expect_error(dataset.new(name="bla", data= list(7, 2, c(3, 4), list(a="b"))));
  expect_error(dataset.new(name="bla", features=list(c="c", a="a", b="b")));
})
