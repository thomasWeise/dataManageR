library("dataManageR")
context("datasets.select.by.features")

test_that("datasets.select.by.features", {
  ds1 <- dataset.new(name="bla.1", features=list(c="c", a="a", b="b", f=1.1, g=4.1, y=TRUE, z=TRUE), data= list(7, 2, c(3, 4), list(a="b")));
  ds2 <- dataset.new(name="bla.2", features=list(c="c", d=5, a="g", b=4, f=1, y="FALSE", g=4), data= list(7, 2, c(3, 4), list(a="b")));
  ds3 <- dataset.new(name="bla.3", features=list(e=4, b=6L, g=5, z=FALSE, a="a"), data=list(7, 2, c(3, 4), list(a="b")));

  l <- c(ds1, ds2, ds3);

  expect_identical(datasets.select.by.features(l, exclude=list(a="g"), include=list(g=5), getIndex=FALSE), c(ds3));
  expect_identical(datasets.select.by.features(l, exclude=list(a="g"), include=list(g=5), getIndex=TRUE), c(3L));
  expect_identical(datasets.select.by.features(l, exclude=list(a="g", y=TRUE), getIndex=FALSE), c(ds3));
  expect_identical(datasets.select.by.features(l, exclude=list(a="g", y=TRUE), getIndex=TRUE), c(3L));
  expect_identical(datasets.select.by.features(l, exclude=list(a="g", y=FALSE), getIndex=FALSE), c(ds1, ds3));
  expect_identical(datasets.select.by.features(l, exclude=list(a="g", y=FALSE), getIndex=TRUE), c(1L, 3L));

  expect_identical(datasets.select.by.features(l, exclude=list(a="a"), getIndex=FALSE), c(ds2));
  expect_identical(datasets.select.by.features(l, exclude=list(a="a"), getIndex=TRUE), c(2L));
  expect_identical(datasets.select.by.features(l, exclude=list(a="g"), getIndex=FALSE), c(ds1, ds3));
  expect_identical(datasets.select.by.features(l, exclude=list(a="g"), getIndex=TRUE), c(1L, 3L));
  expect_identical(datasets.select.by.features(l, exclude=list(a="cc"), getIndex=FALSE), l);
  expect_identical(datasets.select.by.features(l, exclude=list(a="cc"), getIndex=TRUE), c(1L, 2L, 3L));
  expect_identical(datasets.select.by.features(l, exclude=list(b="b"), getIndex=FALSE), c(ds2, ds3));
  expect_identical(datasets.select.by.features(l, exclude=list(b="b"), getIndex=TRUE), c(2L, 3L));
  expect_identical(datasets.select.by.features(l, exclude=list(b=4), getIndex=FALSE), c(ds1, ds3));
  expect_identical(datasets.select.by.features(l, exclude=list(b=4), getIndex=TRUE), c(1L, 3L));
  expect_identical(datasets.select.by.features(l, exclude=list(b=6), getIndex=FALSE), c(ds1, ds2));
  expect_identical(datasets.select.by.features(l, exclude=list(b=6), getIndex=TRUE), c(1L, 2L));
  expect_identical(datasets.select.by.features(l, exclude=list(b="cc"), getIndex=FALSE), l);
  expect_identical(datasets.select.by.features(l, exclude=list(b="cc"), getIndex=TRUE), c(1L, 2L, 3L));
  expect_identical(datasets.select.by.features(l, exclude=list(c="c"), getIndex=FALSE), c(ds3));
  expect_identical(datasets.select.by.features(l, exclude=list(c="c"), getIndex=TRUE), c(3L));

  expect_identical(datasets.select.by.features(l, list(a="a"), getIndex=FALSE), c(ds1, ds3));
  expect_identical(datasets.select.by.features(l, list(a="a"), getIndex=TRUE), c(1L, 3L));
  expect_identical(datasets.select.by.features(l, list(a="g"), getIndex=FALSE), c(ds2));
  expect_identical(datasets.select.by.features(l, list(a="g"), getIndex=TRUE), c(2L));
  expect_identical(datasets.select.by.features(l, list(a="cc"), getIndex=FALSE), list());
  expect_identical(datasets.select.by.features(l, list(a="cc"), getIndex=TRUE), integer(0));
  expect_identical(datasets.select.by.features(l, list(b="b"), getIndex=FALSE), c(ds1));
  expect_identical(datasets.select.by.features(l, list(b="b"), getIndex=TRUE), c(1L));
  expect_identical(datasets.select.by.features(l, list(b=4), getIndex=FALSE), c(ds2));
  expect_identical(datasets.select.by.features(l, list(b=4), getIndex=TRUE), c(2L));
  expect_identical(datasets.select.by.features(l, list(b=6), getIndex=FALSE), c(ds3));
  expect_identical(datasets.select.by.features(l, list(b=6), getIndex=TRUE), c(3L));
  expect_identical(datasets.select.by.features(l, list(b="cc"), getIndex=FALSE), list());
  expect_identical(datasets.select.by.features(l, list(b="cc"), getIndex=TRUE), integer(0));
  expect_identical(datasets.select.by.features(l, list(c="c"), getIndex=FALSE), c(ds1, ds2));
  expect_identical(datasets.select.by.features(l, list(c="c"), getIndex=TRUE), c(1L, 2L));

  expect_identical(datasets.select.by.features(l, list(), getIndex=FALSE), l);
  expect_identical(datasets.select.by.features(l, list(), getIndex=TRUE), c(1L, 2L, 3L));
  expect_identical(datasets.select.by.features(l, getIndex=FALSE), l);
  expect_identical(datasets.select.by.features(l, getIndex=TRUE), c(1L, 2L, 3L));
})
