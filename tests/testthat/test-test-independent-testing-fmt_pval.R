test_that("Test different type of Mean, SD and Digits", {
  # Test various numeric inputs, digits and width combination
  #Test when input is NA
  pval_1 <- 0.049
  pval_2 <- 0.01
  pval_3 <- 0.0005

  # test different p values
  expect_match(fmt_pval(pval_1), " 0.049")
  expect_match(fmt_pval(pval_2), " 0.010")
  expect_match(fmt_pval(pval_3), "<0.001")
  expect_error(fmt_pval(NA))

  # test different digits
  expect_match(fmt_pval(pval_1, digits = 2), " 0.05")
  expect_match(fmt_pval(pval_2, digits = 4), " 0.0100")
  expect_match(fmt_pval(pval_3, digits = 2), "<0.01")
  expect_error(fmt_pval(NA, digits = 2))


})

test_that("Test on data frame column", {
  #Create data frame column with various value.
  #Include NA as one of the value
  #check if the output column values are as expected

  # test df
  test_df <- data.frame(p_val = c(0.002, 0.049, 0.01, 0.05, NA))

  expect_setequal(fmt_pval(test_df$p_val), c(" 0.002", " 0.049", " 0.010", " 0.050", NA))

})