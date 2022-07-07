meta <- meta_ae_dummy()

test_that("There are 2 analyses in the output: ae summary & ae specific", {
  expect_equal(names(meta$analysis), c("ae_summary", "ae_specific"))
})

test_that("There are 2 observations: wk12 & wk24", {
  expect_equal(names(meta$observation), c("wk12", "wk24"))
})

test_that("There is 1 population: apat", {
  expect_equal(names(meta$population), "apat")
})

test_that("The output is a list", {
  expect_type(meta, 'list')
})
