library("dataManageR")
library("testthat")
library("testthat")
context("datasets.features.unique")


test_that("Test datasets.features.unique", {
  ds1 <- dataset.new(name="alpha/beta",   features=list(c="c", a="a", b="b"), data= list(1, 2));
  ds2 <- dataset.new(name="alpha/gamma",  features=list(c="d", a="a", b="b"), data= list(3, 4));
  ds3 <- dataset.new(name="beta/beta",    features=list(c="c", a="x", b="b"), data= list(5, 6));
  ds4 <- dataset.new(name="beta/gamma",   features=list(c="c", a="a", b="v"), data= list(7, 8, 9));

  dss1 <- list(ds1, ds2, ds3, ds4);

  feats <- datasets.features.unique(dss1, features.keep = "a");
  expect_equal(feats,
                   list(features=list(
                     list(a="a"), list(a="x")
                   ), selection=list(
                     c(1L, 2L, 4L),
                     c(3L)
                   )));

  feats <- datasets.features.unique(dss1, features.ignore = "a");
  expect_equal(feats,
               list(features=list(
                 list(b="b", c="c"),
                 list(b="b", c="d"),
                 list(b="v", c="c")
               ), selection=list(
                 c(1L, 3L),
                 c(2L),
                 c(4L)
               )));

  feats <- datasets.features.unique(dss1, features.keep = c("a", "b"));
  expect_equal(feats,
               list(features=list(
                 list(a="a", b="b"),
                 list(a="x", b="b"),
                 list(a="a", b="v")
               ), selection=list(
                 c(1L, 2L),
                 c(3L),
                 c(4L)
               )));
})
