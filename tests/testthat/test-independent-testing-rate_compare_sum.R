# Test case 1
test_that("compare outputs with SAS (stratified)", {
  treatment <- c(rep("pbo", 119), rep("exp", 116))
  response <- c(rep(0, 70), rep(1, 49), rep(0, 46), rep(1, 70))
  stratum <- c(rep(1:3, 38), 1, 3, 3, 1, 2, 2, rep(1:3, 37), 1, 1, 2, 3)
  n0 <- sapply(split(treatment[treatment == "pbo"], stratum[treatment == "pbo"]), length)
  n1 <- sapply(split(treatment[treatment == "exp"], stratum[treatment == "exp"]), length)
  x0 <- sapply(split(response[treatment == "pbo"], stratum[treatment == "pbo"]), sum)
  x1 <- sapply(split(response[treatment == "exp"], stratum[treatment == "exp"]), sum)
  strata <- c("a", "b", "c")
  result <- rate_compare_sum(
    n0, n1, x0, x1,
    strata,
    delta = 0,
    weight = "ss",
    test = "one.sided",
    eps = 1e-06,
    alpha = 0.05
  )
  # Compare with outcome from sas %rate0compare() macro
  expect_equal(result$est, 19.181238694 / 100, tolerance = 1e-5)
  expect_equal(result$z_score, 2.9215992269, tolerance = 1e-5)
  expect_equal(result$p, 0.0017411966, tolerance = 1e-5)
  expect_equal(result$lower, 6.3398653094 / 100, tolerance = 1e-5)
  expect_equal(result$upper, 31.395972269 / 100, tolerance = 1e-5)
})

test_that("compare outputs with SAS (unstratified)", {
  result <- rate_compare_sum(
    n0 = 119, n1 = 116, x0 = 70, x1 = 46,
    delta = 0,
    weight = "ss",
    test = "one.sided",
    eps = 1e-06,
    alpha = 0.05
  )
  # compare with outcome from sas %rate0compare() macro
  expect_equal(result$est, -19.168357 / 100, tolerance = 1e-5)
  expect_equal(result$z_score, -2.932194791, tolerance = 1e-5)
  expect_equal(result$p, 0.9983171222, tolerance = 1e-5)
  expect_equal(result$lower, -31.33431419 / 100, tolerance = 1e-5)
  expect_equal(result$upper, -6.382126982 / 100, tolerance = 1e-5)
})

# Test case 2
test_that("throw error when the length of n0, n1, x0, x1 doesn't match", {
  expect_error(rate_compare_sum(
    n0 = c(10, 20, 30), n1 = c(20, 10, 40),
    x0 = c(1, 2, 3), x1 = c(2, 4),
    strata = c(1, 2, 3)
  ))
  expect_error(rate_compare_sum(
    n0 = c(10, 20, 30), n1 = c(20, 40),
    x0 = c(1, 2, 3), x1 = c(2, 4, 6),
    strata = c(1, 2, 3)
  ))
  expect_error(rate_compare_sum(
    n0 = c(10, 20, 30), n1 = c(20, 10, 40),
    x0 = c(1, 2, 3), x1 = c(2, 4, 6),
    strata = c("a", "b")
  ))
})

# Test case 3
test_that("match with prop_test_mn()", {
  my_n0 <- 100 # number of subjects in arm 0
  my_n1 <- 105 # number of subjects in arm 1
  my_x0 <- 40 # number of response in arm 0
  my_x1 <- 60 # number of response in arm 1

  xx <- rate_compare_sum(n0 = my_n0, n1 = my_n1, x0 = my_x0, x1 = my_x1)
  yy <- prop_test_mn(n0 = my_n0, n1 = my_n1, x0 = my_x0, x1 = my_x1)

  xx_output <- c(xx$est, xx$lower, xx$upper) * 100
  yy_output <- c(yy$est, yy$lower, yy$upper)

  expect_equal(xx_output, yy_output, tolerance = 1e-4)
})

# Test case 4
test_that("match with rate_compare()", {
  ana <- data.frame(
    treatment = c(rep(0, 100), rep(1, 100)),
    response  = c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60)),
    stratum   = c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
  )

  x0 <- sum(ana$response[1:100])
  x1 <- sum(ana$response[101:200])
  n0 <- 100
  n1 <- 100

  xx <- rate_compare(response ~ treatment, data = ana)
  yy <- rate_compare_sum(n0 = n0, n1 = n1, x0 = x0, x1 = x1)

  xx_output <- c(xx$est, xx$lower, xx$upper, xx$p)
  yy_output <- c(yy$est, yy$lower, yy$upper, yy$p)
  expect_equal(xx_output, yy_output, tolerance = 1e-4)
  expect_equal(colnames(xx), colnames(yy))
})

test_that("return MN confidence interval when both event counts are zero", {
  result <- rate_compare_sum(n0 = 105, n1 = 101, x0 = 0, x1 = 0)

  expect_equal(result$est, 0)
  expect_equal(result$z_score, 0)
  expect_equal(result$p, 1)
  expect_true(all(is.finite(c(result$lower, result$upper))))
  expect_lt(result$lower, 0)
  expect_gt(result$upper, 0)
  expect_equal(result$lower, -0.03547636, tolerance = 1e-3)
  expect_equal(result$upper, 0.03683015, tolerance = 1e-3)
})

test_that("zero-event confidence interval gets tighter with larger samples", {
  small <- rate_compare_sum(n0 = 10, n1 = 20, x0 = 0, x1 = 0)
  large <- rate_compare_sum(n0 = 105, n1 = 101, x0 = 0, x1 = 0)

  expect_true(all(is.finite(c(small$lower, small$upper))))
  expect_gt(small$upper - small$lower, large$upper - large$lower)
})

test_that("single-arm zero events still return finite results", {
  result <- rate_compare_sum(n0 = 105, n1 = 101, x0 = 0, x1 = 1)

  expect_true(all(is.finite(unlist(result))))
})

test_that("NA inputs still return an NA row", {
  result <- rate_compare_sum(n0 = NA, n1 = 101, x0 = 0, x1 = 0)

  expect_true(all(is.na(unlist(result))))
})

test_that("zero-event strata contribute to a stratified analysis", {
  result <- rate_compare_sum(
    n0 = c(50, 55), n1 = c(50, 51),
    x0 = c(0, 4), x1 = c(0, 8),
    strata = c("a", "b")
  )

  expect_true(all(is.finite(unlist(result))))
})

test_that("stratified equal weights normalize to sum to 1", {
  # With identical stratum sample sizes, equal, ss, and cmh weights are identical.
  n0 <- c(50, 50, 50)
  n1 <- c(50, 50, 50)
  x0 <- c(10, 15, 20)
  x1 <- c(20, 25, 30)
  strata <- c("a", "b", "c")

  res_ss <- rate_compare_sum(n0, n1, x0, x1, strata, weight = "ss")
  res_eq <- rate_compare_sum(n0, n1, x0, x1, strata, weight = "equal")
  expect_equal(res_eq, res_ss, tolerance = 1e-10)

  res_ss_d <- rate_compare_sum(n0, n1, x0, x1, strata, delta = 0.1, weight = "ss")
  res_eq_d <- rate_compare_sum(n0, n1, x0, x1, strata, delta = 0.1, weight = "equal")
  expect_equal(res_eq_d, res_ss_d, tolerance = 1e-10)

  # Unbalanced strata from the vignette example should produce a valid RD and CI.
  ana <- data.frame(
    treatment = c(rep(0, 100), rep(1, 100)),
    response = c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60)),
    stratum = c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
  )
  res_vig <- rate_compare(
    response ~ treatment,
    strata = stratum, data = ana, weight = "equal"
  )
  n0_v <- tapply(ana$treatment == 0, ana$stratum, sum)
  n1_v <- tapply(ana$treatment == 1, ana$stratum, sum)
  x0_v <- tapply(ana$response[ana$treatment == 0], ana$stratum[ana$treatment == 0], sum)
  x1_v <- tapply(ana$response[ana$treatment == 1], ana$stratum[ana$treatment == 1], sum)
  expect_equal(res_vig$est, mean(x1_v / n1_v - x0_v / n0_v), tolerance = 1e-10)
  expect_true(all(is.finite(unlist(res_vig))))
  expect_lt(res_vig$lower, res_vig$est)
  expect_gt(res_vig$upper, res_vig$est)
})

test_that("unstratified rate_compare_sum() ignores weight when strata is NULL", {
  res_ss <- rate_compare_sum(n0 = 119, n1 = 116, x0 = 70, x1 = 46, weight = "ss")
  res_eq <- rate_compare_sum(n0 = 119, n1 = 116, x0 = 70, x1 = 46, weight = "equal")
  expect_equal(res_eq, res_ss, tolerance = 1e-10)
})
