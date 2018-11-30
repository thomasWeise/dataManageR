library("dataManageR")
library("testthat")
context("datasets.feature.load.text")

test_that("Test datasets.feature.load.text", {
  featuresFolder <- tempfile();
  dir.create(featuresFolder, showWarnings = FALSE, recursive = TRUE);

  component.1 <- "aa";
  f1 <- file.path(featuresFolder, paste(component.1, ".txt", sep="", collapse=""));
  f <- file(f1, open="wt");
  writeLines(con=f,
             text=c("a=1", "b=HALLO", "c=FALSE"));
  close(f);

  component.2 <- "bb";
  f2 <- file.path(featuresFolder, paste(component.2, ".txt", sep="", collapse=""));
  f <- file(f2, open="wt");
  writeLines(con=f,
             text=c("d=4.4", "e=4"));
  close(f);

  component.3 <- "a";
  f3 <- file.path(featuresFolder, paste(component.3, ".txt", sep="", collapse=""));
  f <- file(f3, open="wt");
  writeLines(con=f,
             text=c("a=13", "b=XXX", "c=TRUE"));
  close(f);

  expect_identical(datasets.feature.load.text(featuresFolder,
                                              c(component.1, component.2)),
                   list(a=1L, b="HALLO", c=FALSE, d=4.4, e=4L));
  expect_identical(datasets.feature.load.text(featuresFolder,
                                              c(component.2, component.1)),
                   list(a=1L, b="HALLO", c=FALSE, d=4.4, e=4L));

  expect_identical(datasets.feature.load.text(featuresFolder,
                                              c(component.2, component.3)),
                   list(a=13L, b="XXX", c=TRUE, d=4.4, e=4L));

  expect_error(datasets.feature.load.text(featuresFolder,
                                          c(component.1, component.3)));

  unlink(f1);
  unlink(f2);
  unlink(f3);
  unlink(featuresFolder, recursive = TRUE);
})
