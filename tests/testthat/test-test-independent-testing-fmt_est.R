test_that("Test different type of Mean, SD and Digits", {
  #Test various numeric inputs, digits combination and bracket type
  #Test when input is NA
  expect_equal(" 11.00 ( 5.666)", fmt_est(10.99999, 5.6656, digits = c(2,3)))

  data(iris)
  x <- iris |>
    summarise(mean = mean(Petal.Width), n = n(), sd = sd(Petal.Width))

  expect_equal("  1.20 ( 0.762)", fmt_est(x$mean, x$sd, digits = c(2,3)))
  expect_equal("1.20 (0.762)", fmt_est(x$mean, x$sd, digits = c(2,3), width = c(1, 2)))
})

test_that("Test different type of Mean, SD and Digits", {
  #Test various numeric inputs, digits combination and bracket type
  #Test when input is NA
  expect_equal(fmt_est(2,  3, digits = c(2,3)), "  2.00 ( 3.000)")
  expect_equal(fmt_est(22,  3, digits = c(2,3)), " 22.00 ( 3.000)")
  expect_equal(fmt_est(22,  333, digits = c(2,3)), " 22.00 (333.000)")
  expect_equal(fmt_est(0, 0, digits = c(2,3)), "  0.00 ( 0.000)")
  expect_equal(fmt_est(999, 999, digits = c(2,3)), "999.00 (999.000)")
  expect_equal(fmt_est(9999, 9999, digits = c(2,3)), "9999.00 (9999.000)")
})



