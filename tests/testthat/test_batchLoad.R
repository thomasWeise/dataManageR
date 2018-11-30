library("dataManageR")
library("testthat")
context("datasets.feature.load.text")

test_that("Test datasets.feature.load.text", {
  baseFolder <- tempfile();
  dir.create(baseFolder, showWarnings = FALSE, recursive = TRUE);

  featuresFolder <- tempfile(tmpdir=baseFolder);
  dir.create(featuresFolder, showWarnings = FALSE, recursive = TRUE);

  dataFolder <- tempfile(tmpdir=baseFolder);
  dir.create(dataFolder, showWarnings = FALSE, recursive = TRUE);

  component.aa <- "aa";
  f1 <- file.path(featuresFolder, paste(component.aa, ".txt", sep="", collapse=""));
  f <- file(f1, open="wt");
  writeLines(con=f,
             text=c("a=1", "b=HALLO", "c=FALSE"));
  close(f);

  component.bb <- "bb";
  f2 <- file.path(featuresFolder, paste(component.bb, ".txt", sep="", collapse=""));
  f <- file(f2, open="wt");
  writeLines(con=f,
             text=c("d=4.4", "e=4"));
  close(f);

  component.a <- "a";
  f3 <- file.path(featuresFolder, paste(component.a, ".txt", sep="", collapse=""));
  f <- file(f3, open="wt");
  writeLines(con=f,
             text=c("a=13", "b=XXX", "c=TRUE"));
  close(f);

  dir.create(dir.aa    <- file.path(dataFolder, component.aa), showWarnings = FALSE, recursive = TRUE);
  dir.create(dir.aa.bb <- file.path(dir.aa,     component.bb), showWarnings = FALSE, recursive = TRUE);
  dir.create(dir.a     <- file.path(dataFolder, component.a),  showWarnings = FALSE, recursive = TRUE);
  dir.create(dir.a.bb  <- file.path(dir.a,      component.bb), showWarnings = FALSE, recursive = TRUE);

  file.1 <- file.path(dir.aa.bb, "1.csv");
  f <- file(file.1, open="wt");
  writeLines(con=f,
             text=c("X,Y", "1,2", "2,4"));
  close(f);

  file.2 <- file.path(dir.aa.bb, "2.csv");
  f <- file(file.2, open="wt");
  writeLines(con=f,
             text=c("A,B", "3,4", "5,6", "7,7"));
  close(f);

  file.3 <- file.path(dir.a.bb, "3.csv");
  f <- file(file.3, open="wt");
  writeLines(con=f,
             text=c("A,B", "13,-4", "5.5,16", "8,8"));
  close(f);
  file.4 <- file.path(dir.a.bb, "4.csv");
  f <- file(file.4, open="wt");
  writeLines(con=f,
             text=c("D,E", "0,5", "1,21"));
  close(f);


  datasets <- datasets.batchLoad(path=dataFolder, featuresFolder=featuresFolder, logging=FALSE);
  expect_length(datasets, 2L);

  expect_identical(datasets[[1]]@name, "a/bb");
  expect_identical(datasets[[1]]@features, list(a=13L, b="XXX", c=TRUE, d=4.4, e=4L));
  expect_length(datasets[[1]]@data, 2L);

  expect_identical(nrow(datasets[[1]]@data[[1L]]), 3L);
  expect_identical(colnames(datasets[[1]]@data[[1L]]), c("A", "B"));
  expect_identical(datasets[[1]]@data[[1L]]$A, c(13,5.5,8));
  expect_identical(datasets[[1]]@data[[1L]]$B, c(-4L,16L,8L));
  expect_identical(nrow(datasets[[1]]@data[[2L]]), 2L);
  expect_identical(colnames(datasets[[1]]@data[[2L]]), c("D", "E"));
  expect_identical(datasets[[1]]@data[[2L]]$D, c(0L,1L));
  expect_identical(datasets[[1]]@data[[2L]]$E, c(5L,21L));

  expect_identical(datasets[[2]]@name, "aa/bb");
  expect_identical(datasets[[2]]@features, list(a=1L, b="HALLO", c=FALSE, d=4.4, e=4L));
  expect_length(datasets[[2]]@data, 2L);

  expect_identical(nrow(datasets[[2]]@data[[1L]]), 2L);
  expect_identical(colnames(datasets[[2]]@data[[1L]]), c("X", "Y"));
  expect_identical(datasets[[2]]@data[[1L]]$X, c(1L,2L));
  expect_identical(datasets[[2]]@data[[1L]]$Y, c(2L,4L));
  expect_identical(nrow(datasets[[2]]@data[[2L]]), 3L);
  expect_identical(colnames(datasets[[2]]@data[[2L]]), c("A", "B"));
  expect_identical(datasets[[2]]@data[[2L]]$A, c(3L,5L,7L));
  expect_identical(datasets[[2]]@data[[2L]]$B, c(4L,6L,7L));

  unlink(file.1);
  unlink(file.2);
  unlink(file.3);
  unlink(file.4);
  unlink(dataFolder, recursive = TRUE);
  unlink(f1);
  unlink(f2);
  unlink(f3);
  unlink(featuresFolder, recursive = TRUE);
  unlink(baseFolder, recursive = TRUE);
})
