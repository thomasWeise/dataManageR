library("dataManageR")
context("datasets.merge.by.features")


test_that("Test datasets.merge.by.features", {
  ds1 <- dataset.new(name="alpha/beta",   features=list(c="c", a="a", b="b"), data= list(1, 2));
  ds2 <- dataset.new(name="alpha/gamma",  features=list(c="d", a="a", b="b"), data= list(3, 4));
  ds3 <- dataset.new(name="beta/beta",    features=list(c="c", a="x", b="b"), data= list(5, 6));
  ds4 <- dataset.new(name="beta/gamma",   features=list(c="c", a="a", b="v"), data= list(7, 8, 9));

  dss1 <- list(ds1, ds2, ds3, ds4);

  merge <- datasets.merge.by.features(dss1, features.keep = "a");
  expect_identical(merge,
                   datasets.merge.by.features(dss1, features.ignore = c("b", "c")));
  res <- merge$datasets;
  sel <- merge$selection;
  expect_length(res, 2L);
  expect_identical(res[[1]]@name, "a=a");
  expect_identical(res[[1]]@features, list(a="a"));
  expect_identical(res[[1]]@data, list(1, 2, 3, 4, 7, 8, 9));
  expect_identical(res[[2]]@name, ds3@name);
  expect_identical(res[[2]]@features, list(a="x"));
  expect_identical(res[[2]]@data, ds3@data);
  expect_identical(sel, list(c(1L, 2L, 4L), c(3L)));

  merge <- datasets.merge.by.features(dss1, features.ignore = "a");
  expect_identical(merge,
                   datasets.merge.by.features(dss1, features.keep = c("b", "c")));
  res <- merge$datasets;
  sel <- merge$selection;
  expect_length(res, 3L);
  expect_identical(res[[1]]@name, "beta");
  expect_identical(res[[1]]@features, list(b="b", c="c"));
  expect_identical(res[[1]]@data, list(1, 2, 5, 6));
  expect_identical(res[[2]]@name, ds2@name);
  expect_identical(res[[2]]@features, list(b="b", c="d"));
  expect_identical(res[[2]]@data, ds2@data);
  expect_identical(res[[3]]@name, ds4@name);
  expect_identical(res[[3]]@features, list(b="v", c="c"));
  expect_identical(res[[3]]@data, ds4@data);
  expect_identical(sel, list(c(1L, 3L), c(2L), c(4L)));

  merge <- datasets.merge.by.features(dss1, features.ignore = c("a", "b"));
  expect_identical(merge,
                   datasets.merge.by.features(dss1, features.keep = "c"));
  res <- merge$datasets;
  sel <- merge$selection;
  expect_length(res, 2L);
  expect_identical(res[[1]]@name, "c=c");
  expect_identical(res[[1]]@features, list(c="c"));
  expect_identical(res[[1]]@data, list(1, 2, 5, 6, 7, 8, 9));
  expect_identical(res[[2]]@name, ds2@name);
  expect_identical(res[[2]]@features, list(c="d"));
  expect_identical(res[[2]]@data, ds2@data);
  expect_identical(sel, list(c(1L, 3L, 4L), c(2L)));


  merge <- datasets.merge.by.features(dss1, features.keep= c("a", "b"));
  expect_identical(merge,
                   datasets.merge.by.features(dss1, features.ignore = "c"));
  res <- merge$datasets;
  sel <- merge$selection;
  expect_length(res, 3L);
  expect_identical(res[[1]]@name, "alpha");
  expect_identical(res[[1]]@features, list(a="a", b="b"));
  expect_identical(res[[1]]@data, list(1, 2, 3, 4));
  expect_identical(res[[2]]@name, ds3@name);
  expect_identical(res[[2]]@features, list(a="x", b="b"));
  expect_identical(res[[2]]@data, ds3@data);
  expect_identical(res[[3]]@name, ds4@name);
  expect_identical(res[[3]]@features, list(a="a", b="v"));
  expect_identical(res[[3]]@data, ds4@data);
  expect_identical(sel, list(c(1L, 2L), c(3L), c(4L)));
})
