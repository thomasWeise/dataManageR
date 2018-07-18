library("dataManageR")
context("datasets.feature.values")

test_that("Test datasets.feature.values", {
  ds1 <- dataset.new(name="bla", features=list(c="c", a="a", b="b", f=1.1, g=4.1), data= list(7, 2, c(3, 4), list(a="b")));
  ds2 <- dataset.new(name="bla", features=list(c="f", d=5, a="g", b=4, f=1, g=4), data= list(7, 2, c(3, 4), list(a="b")));
  ds3 <- dataset.new(name="bla", features=list(e=4, b=6L, g=5), data= list(7, 2, c(3, 4), list(a="b")));

  expect_identical(datasets.feature.values(c(ds1, ds2, ds3), "a"), c("a", "g", NA));
  expect_identical(datasets.feature.values(c(ds1, ds3, ds2), "a"), c("a", NA, "g"));
  expect_identical(datasets.feature.values(c(ds2, ds3), "a"), c("g", NA));
  expect_identical(datasets.feature.values(c(ds1), "a"), c("a"));

  expect_identical(datasets.feature.values(c(ds1, ds2, ds3), "b"), c("b", "4", "6"));
  expect_identical(datasets.feature.values(c(ds2, ds3), "b"), c(4L, 6L));

  expect_identical(datasets.feature.values(c(ds1, ds2, ds3), "c"), c("c", "f", NA));
  expect_identical(datasets.feature.values(c(ds3), "c"), c(NA));

  expect_identical(datasets.feature.values(c(ds1, ds2, ds3), "d"), c(NA_integer_, 5L, NA_integer_));
  expect_identical(datasets.feature.values(c(ds2, ds3), "d"), c(5L, NA_integer_));
  expect_identical(datasets.feature.values(c(ds2), "d"), c(5L));

  expect_identical(datasets.feature.values(c(ds1, ds2, ds3), "e"), c(NA_integer_, NA_integer_, 4L));

  expect_identical(datasets.feature.values(c(ds1, ds2, ds3), "f"), c(1.1, 1, NA));
  expect_identical(datasets.feature.values(c(ds1, ds3), "f"), c(1.1, NA));
  expect_identical(datasets.feature.values(c(ds2, ds3), "f"), c(1L, NA_integer_));

  expect_identical(datasets.feature.values(c(ds1, ds2, ds3), "g"), c(4.1, 4, 5));
  expect_identical(datasets.feature.values(c(ds2, ds3), "g"), c(4L, 5L));
  expect_identical(datasets.feature.values(c(ds1, ds3), "g"), c(4.1, 5));
  expect_identical(datasets.feature.values(c(ds1, ds2), "g"), c(4.1, 4));
  expect_identical(datasets.feature.values(c(ds2, ds1), "g"), c(4, 4.1));
})
