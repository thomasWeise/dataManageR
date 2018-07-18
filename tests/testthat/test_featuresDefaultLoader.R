library("dataManageR")
context("datasets.feature.load.default")

test_that("Test datasets.feature.load.text", {
  expect_identica(datasets.feature.load.default, datasets.feature.load.text);
})
