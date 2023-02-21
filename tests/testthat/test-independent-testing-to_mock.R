test_that("if df = NULL then the output is NULL", {
  expect_null(to_mock(df = NULL))
})

test_that("if df = NULL then the output is NULL", {
  expect_snapshot(to_mock(df = head(iris)))
})

test_that("if mask = y then the table cells will be masked by 'y'", {
  expect_snapshot(to_mock(df = head(iris), mask = "y"))
})
