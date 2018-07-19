library("dataManageR")
context("datasets.select.by.name")

test_that("datasets.select.by.name", {
  ds1 <- dataset.new(name="bla.1", features=list(c="c", a="a", b="b", f=1.1, g=4.1, y=TRUE, z=TRUE), data= list(7, 2, c(3, 4), list(a="b")));
  ds2 <- dataset.new(name="bla.2", features=list(c="f", d=5, a="g", b=4, f=1, y="FALSE", g=4), data= list(7, 2, c(3, 4), list(a="b")));
  ds3 <- dataset.new(name="bla.3", features=list(e=4, b=6L, g=5, z=FALSE), data=list(7, 2, c(3, 4), list(a="b")));

  l <- c(ds1, ds2, ds3);

  expect_identical(datasets.select.by.name(l, "bla.1", getIndex=FALSE), ds1);
  expect_identical(datasets.select.by.name(l, "bla.1", getIndex=TRUE), 1L);
  expect_identical(datasets.select.by.name(l, "bla.2", getIndex=FALSE), ds2);
  expect_identical(datasets.select.by.name(l, "bla.2", getIndex=TRUE), 2L);
  expect_identical(datasets.select.by.name(l, "bla.3", getIndex=FALSE), ds3);
  expect_identical(datasets.select.by.name(l, "bla.3", getIndex=TRUE), 3L);

  expect_identical(datasets.select.by.name(l, "bla.x3", getIndex=FALSE), NULL);
  expect_identical(datasets.select.by.name(l, "bla.x3", getIndex=TRUE), NULL);
})
