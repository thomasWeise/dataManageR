library("dataManageR")
context("datasets.names.get")

test_that("datasets.names.get", {
  d1 <- dataset.new(name="bla", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));
  d2 <- dataset.new(name="bla", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));
  d3 <- dataset.new(name="blub", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));
  d4 <- dataset.new(name="xyz", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));

  expect_identical(datasets.names.get(c(d1, d2, d3, d4)), c("bla", "bla", "blub", "xyz"));
  expect_identical(datasets.names.get(c(d1, d3, d4)), c("bla", "blub", "xyz"));
  expect_identical(datasets.names.get(c(d1)), c("bla"));
})
