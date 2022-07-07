test_that("compare outputs with SAS (stratified)", {
  treatment <- c(rep("pbo", 119), rep("exp", 116))
  response <- c(rep(0, 70), rep(1, 49), rep(0, 46), rep(1, 70))
  stratum <- c(rep(1:3, 38), 1, 3, 3, 1, 2, 2, rep(1:3, 37), 1,1, 2, 3)

  result <- rate_compare(
     formula = response ~ factor(treatment, levels = c("pbo", "exp")),
     strata = stratum,
     delta = 0,
     weight = "cmh",
     test = "one.sided",
     eps = 1e-06,
     alpha = 0.05
  )
  # compare with outcome from sas %rate0compare() macro
  expect_equal(result$est, 19.181363786 /100, tolerance = 1e-5)
  expect_equal(result$z_score, 2.9216181843, tolerance = 1e-5)
  expect_equal(result$p, 0.0017410907, tolerance = 1e-5)
  expect_equal(result$lower, 6.3399769229 / 100, tolerance = 1e-5)
  expect_equal(result$upper, 31.396078455 / 100, tolerance = 1e-5)
})

test_that("compare outputs with SAS (unstratified)", {

  treatment <- c(rep("pbo", 119), rep("exp", 116))
  response <- c(rep(0, 70), rep(1, 49), rep(0, 46), rep(1, 70))

  result <- rate_compare(
    formula = response ~ factor(treatment, levels = c("pbo", "exp")),
    delta = 0,
    weight = "equal",
    test = "two.sided",
    eps = 1e-06,
    alpha = 0.05
  )
  # compare with outcome from sas %rate0compare() macro
  expect_equal(result$est, 19.168356998 /100, tolerance = 1e-5)
  expect_equal(result$z_score, 2.9321947913, tolerance = 1e-5)
  expect_equal(result$p, 0.0033657557, tolerance = 1e-5)
  expect_equal(result$lower, 6.3821269823 / 100, tolerance = 1e-5)
  expect_equal(result$upper, 31.334314192 / 100, tolerance = 1e-5)
})

test_that("rate_compare() matches prop_test_mn for unstratified analysis", {
  ana <- data.frame(
    treatment = c(rep(0,100),rep(1,100)),
    response  = c(rep(0,80),rep(1,20),rep(0,40),rep(1,60)),
    stratum   = c(rep(1:4,12),1,3,3,1,rep(1:4,12),rep(1:4,25))
  )

  x0 <- sum(ana$response[1:100])
  x1 <- sum(ana$response[101:200])
  n0 <- 100
  n1 <- 100

  compare1 <- rate_compare(response~treatment, data = ana)
  compare2 <- prop_test_mn(x0, n0, x1, n1)

  o1 <- c(compare1$est, compare1$lower, compare1$upper, compare1$p)*100

  o2 <- c(compare2$est, compare2$lower, compare2$upper, compare2$p)
  expect_equal(o1, o2, tolerance = 1e-3)
})
