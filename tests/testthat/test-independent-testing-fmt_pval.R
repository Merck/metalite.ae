test_that("Test on data frame column", {
  # Create data frame column with various values.
  # Include `NA` as one of the values.
  # Check if the output column values are as expected.
  expect_equal(fmt_pval(0.1), " 0.100")
  expect_equal(fmt_pval(0.01), " 0.010")
  expect_equal(fmt_pval(0.001), " 0.001")
  expect_equal(fmt_pval(0.0001), "<0.001")
})

test_that("vectorization works", {
  # Test data frame
  df <- data.frame(pval = c(0.1, 0.01, 0.001, 0.0001, 0.00001, NA))
  pvalue <- fmt_pval(df$pval)

  expect_equal(pvalue, c(" 0.100", " 0.010", " 0.001", "<0.001", "<0.001", NA))
})


test_that("vectorization works, expand width to 7", {
  # Test data frame
  df <- data.frame(pval = c(0.1, 0.01, 0.001, 0.0001, 0.00001, NA))
  pvalue <- fmt_pval(df$pval, width = 7)

  expect_equal(pvalue, c("  0.100", "  0.010", "  0.001", "<0.001", "<0.001", NA))
})

test_that("vectorization works, change digits to 2, width to 7", {
  # Test data frame
  df <- data.frame(pval = c(0.1, 0.021, 0.001, 0.0001, 0.00001, NA))
  pvalue <- fmt_pval(df$pval, digits = 2, width = 5)

  expect_equal(pvalue, c(" 0.10", " 0.02", "<0.01", "<0.01", "<0.01", NA))
})
