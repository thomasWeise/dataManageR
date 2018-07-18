library("dataManageR")
context("datasets.merge.by.selection")


test_that("Test datasets.merge.by.selection", {
  ds1 <- dataset.new(name="alpha/beta",   features=list(c="c", a="a", b="b"), data= list(1, 2));
  ds2 <- dataset.new(name="alpha/gamma",  features=list(c="d", a="a", b="b"), data= list(3, 4));
  ds3 <- dataset.new(name="beta/beta",    features=list(c="c", a="x", b="b"), data= list(5, 6));
  ds4 <- dataset.new(name="beta/gamma",   features=list(c="c", a="a", b="v"), data= list(7, 8, 9));

  dss1 <- list(ds1, ds2, ds3, ds4);

  expect_identical(datasets.merge.by.selection(dss1, selection=list(1, 2, 3, 4)), dss1);

  res <- datasets.merge.by.selection(dss1, selection=list(c(1, 2), c(3, 4)));
  expect_length(res, 2L);
  expect_identical(res[[1]]@name, "alpha");
  expect_identical(res[[1]]@features, list(a="a", b="b"));
  expect_identical(res[[1]]@data, list(1, 2, 3, 4));
  expect_identical(res[[2]]@name, "beta");
  expect_identical(res[[2]]@features, list(c="c"));
  expect_identical(res[[2]]@data, list(5, 6, 7, 8, 9));

  res <- datasets.merge.by.selection(dss1, selection=list(c(2), c(1, 3, 4)));
  expect_length(res, 2L);
  expect_identical(res[[1]], ds2);
  expect_identical(res[[2]]@name, "c=c");
  expect_identical(res[[2]]@features, list(c="c"));
  expect_identical(res[[2]]@data, list(1, 2, 5, 6, 7, 8, 9));

  res <- datasets.merge.by.selection(dss1, selection=list(c(1), c(2, 3, 4)));
  expect_length(res, 2L);
  expect_identical(res[[1]], ds1);
  expect_identical(res[[2]]@name, "unnamed");
  expect_length(res[[2]]@features, 0);
  expect_identical(res[[2]]@data, list(3, 4, 5, 6, 7, 8, 9));
})
