test_that("Test different type of Mean, SD and Digits", {
  # Test various numeric inputs, digits and width combination
  #Test when input is NA

  # example data
  x = c(9.0,9.5,9.6,10.2,11.6)
  # p values from different means
  pval_1 <- t.test(x, mu = 9)$p.value
  pval_2 <- t.test(x, mu = 1)$p.value
  pval_3 <- t.test(x, mu = 15)$p.value

  # test different means
  expect_match(fmt_pval(pval_1), " 0.094")
  expect_match(fmt_pval(pval_2), "<0.001")
  expect_match(fmt_pval(pval_3), "<0.001")

  # test different digits
  expect_match(fmt_pval(pval_1, digits = 2), " 0.09")
  expect_match(fmt_pval(pval_2, digits = 4), "<1e-04")
  expect_match(fmt_pval(pval_3, digits = 0), "<1")

})

test_that("Test on data frame column", {
  #Create data frame column with various value.
  #Include NA as one of the value
  #check if the output column values are as expected

})