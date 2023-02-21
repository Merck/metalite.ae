test_that("Test different type of Numeric Vectors and Digits", {
  expect_equal(fmt_pct(NA), NA)

  expect_equal(fmt_pct(12), "(12.0)")
  expect_equal(fmt_pct(12.3), "(12.3)")
  expect_equal(fmt_pct(12.343, digits = 2), "(12.34)")
  expect_equal(fmt_pct(12.345, digits = 2), "(12.35)")
  expect_equal(fmt_pct(12.345, digits = 0), "(12)")

  expect_equal(fmt_pct(-12.345, digits = 0), "(-12)")
  expect_equal(fmt_pct(-12.345, digits = 2), "(-12.35)")
})

test_that("Test on data frame column", {
  dt <- data.frame(col1 = c(0.01, 1.23, 12.34, 123.456, NA))
  # For df column, length of each string be equal to max length of input column.
  expect_equal(fmt_pct(dt$col1), c("  (0.0)", "  (1.2)", " (12.3)", "(123.5)", NA))
})
