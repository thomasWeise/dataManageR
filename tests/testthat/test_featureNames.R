library("dataManageR")
context("datasets.feature.names")

test_that("Test datasets.feature.names", {
  ds1 <- dataset.new(name="bla", features=list(c="c", a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));
  ds2 <- dataset.new(name="bla", features=list(c="c", d=5, a="a", b="b"), data= list(7, 2, c(3, 4), list(a="b")));
  ds3 <- dataset.new(name="bla", features=list(e=4), data= list(7, 2, c(3, 4), list(a="b")));

  expect_identical(datasets.feature.names(c(ds1, ds2, ds3)), c("a", "b", "c", "d", "e"));
  expect_identical(datasets.feature.names(c(ds3, ds1, ds2)), c("a", "b", "c", "d", "e"));
  expect_identical(datasets.feature.names(c(ds1, ds3)), c("a", "b", "c", "e"));
  expect_identical(datasets.feature.names(c(ds3, ds1)), c("a", "b", "c", "e"));
  expect_identical(datasets.feature.names(c(ds1)), c("a", "b", "c"));
  expect_identical(datasets.feature.names(c(ds2, ds3)), c("a", "b", "c", "d", "e"));
  expect_identical(datasets.feature.names(c(ds3, ds2)), c("a", "b", "c", "d", "e"));
})
