library("dataManageR")
library("testthat")
context("datasets.as.data.frame")

test_that("datasets.as.data.frame", {
  ds1 <- dataset.new(name="bla.1", features=list(c="c", a="a", b="b", f=1.1, g=4.1, y=TRUE, z=TRUE), data= list(2,1));
  ds2 <- dataset.new(name="bla.2", features=list(c="f", d=5, a="g", b=4, f=1, y="FALSE", g=4), data= list(6, 7));
  ds3 <- dataset.new(name="bla.3", features=list(e=4, b=6L, g=5, z=FALSE), data=list(5, 3));

  data <- list(ds1, ds2, ds3);
  res <- datasets.as.lists(data, groups=c("a", "a", "b"),
                           transformers = list(a=function(source) {
                             na <- datasets.feature.values(source, "a");
                             na[is.na(na)] <- "xx";
                             return(na)
                           },
                           max=function(source) {
                             vapply(X=source, FUN=function(ds) max(c(unlist(ds@data))),
                                    FUN.VALUE = NaN)
                           }));

  expect_length(res, 2L);
  expect_identical(res[[1L]]$group, "a");
  expect_identical(res[[1L]]$a, c("a", "g"));
  expect_identical(res[[1L]]$max, c(2, 7));

  expect_identical(res[[2L]]$group, "b");
  expect_identical(res[[2L]]$a, c("xx"));
  expect_identical(res[[2L]]$max, 5);
})
