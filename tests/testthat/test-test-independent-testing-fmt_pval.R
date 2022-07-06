
test_that("Test on data frame column", {
  #Create data frame column with various value.
  #Include NA as one of the value
  #check if the output column values are as expected
  expect_equal(fmt_pval(0.1), " 0.100")
  expect_equal(fmt_pval(0.01), " 0.010")
  expect_equal(fmt_pval(0.001), " 0.001")
  expect_equal(fmt_pval(0.0001), "<0.001")

  pval <- c(0.1, 0.01, 0.001, 0.0001, 0.00001, NA)


  df <- data.frame(pval)

  pvalue <-fmt_pval(df$pval)

  expect_equal(pvalue, c(" 0.100", " 0.010", " 0.001", "<0.001", "<0.001", NA ))
})

