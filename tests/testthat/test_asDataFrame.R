library("dataManageR")
library("testthat")
context("datasets.as.data.frame")

test_that("datasets.as.data.frame", {
  ds1 <- dataset.new(name="bla.1", features=list(c="c", a="a", b="b", f=1.1, g=4.1, y=TRUE, z=TRUE), data= list(7, 2, c(3, 4), list(a="b")));
  ds2 <- dataset.new(name="bla.2", features=list(c="f", d=5, a="g", b=4, f=1, y="FALSE", g=4), data= list(7, 2, c(3, 4), list(a="b")));
  ds3 <- dataset.new(name="bla.3", features=list(e=4, b=6L, g=5, z=FALSE), data=list(7, 2, c(3, 4), list(a="b")));

  df <- datasets.as.data.frame(c(ds1, ds2, ds3), stringsAsFactors = FALSE);

  expect_identical(nrow(df), 3L);
  expect_identical(rownames(df), c("bla.1", "bla.2", "bla.3"));
  expect_identical(colnames(df), c("a", "b", "c", "d", "e", "f", "g", "y", "z"));
  expect_identical(df$a, c("a", "g", NA_character_));
  expect_identical(df$b, c("b", "4", "6"));
  expect_identical(df$c, c("c", "f", NA_character_));
  expect_identical(df$d, c(NA_integer_, 5L, NA_integer_));
  expect_identical(df$e, c(NA_integer_, NA_integer_, 4L));
  expect_identical(df$f, c(1.1, 1, NA_real_));
  expect_identical(df$g, c(4.1, 4, 5));
  expect_identical(df$y, c("TRUE", "FALSE", NA_character_));
  expect_identical(df$z, c(TRUE, NA, FALSE));

  df <- datasets.as.data.frame(c(ds1, ds2, ds3), stringsAsFactors = TRUE);

  expect_identical(nrow(df), 3L);
  expect_identical(rownames(df), c("bla.1", "bla.2", "bla.3"));
  expect_identical(colnames(df), c("a", "b", "c", "d", "e", "f", "g", "y", "z"));
  expect_identical(as.character(df$a), c("a", "g", NA_character_));
  expect_identical(as.integer(df$a), c(1L, 2L, NA_integer_));
  expect_identical(as.character(df$b), c("b", "4", "6"));
  expect_identical(as.integer(df$b), c(3L, 1L, 2L));
  expect_identical(as.character(df$c), c("c", "f", NA_character_));
  expect_identical(as.integer(df$c), c(1L, 2L, NA_integer_));
  expect_identical(df$d, c(NA_integer_, 5L, NA_integer_));
  expect_identical(df$e, c(NA_integer_, NA_integer_, 4L));
  expect_identical(df$f, c(1.1, 1, NA_real_));
  expect_identical(df$g, c(4.1, 4, 5));
  expect_identical(as.character(df$y), c("TRUE", "FALSE", NA_character_));
  expect_identical(as.integer(df$y), c(2L, 1L, NA_integer_));
  expect_identical(df$z, c(TRUE, NA, FALSE));
})
