library("dataManageR")
context("datasets.feature.load.default")

test_that("Test datasets.feature.load.text", {
  expect_identical(datasets.feature.load.default, datasets.feature.load.text);
})
