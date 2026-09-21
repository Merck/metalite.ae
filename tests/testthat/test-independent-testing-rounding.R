test_that("decimal ties round half away from zero via fmt_pct", {
  # Positive ties at digits = 1: 0.05 -> 0.1, ..., 0.95 -> 1.0
  x <- seq(0.05, 0.95, by = 0.1)
  expected <- c(
    "(0.1)", "(0.2)", "(0.3)", "(0.4)", "(0.5)",
    "(0.6)", "(0.7)", "(0.8)", "(0.9)", "(1.0)"
  )
  expect_equal(fmt_pct(x, digits = 1), expected)

  # Negative ties at digits = 1: mirror image away from zero
  xn <- -x
  expected_neg <- c(
    "(-0.1)", "(-0.2)", "(-0.3)", "(-0.4)", "(-0.5)",
    "(-0.6)", "(-0.7)", "(-0.8)", "(-0.9)", "(-1.0)"
  )
  expect_equal(fmt_pct(xn, digits = 1), expected_neg)
})

test_that("formatted output never shows negative zero", {
  small_neg <- c(-0.001, -0.004, -0.04, -0.049, -0.0)
  neg_zero <- function(x) grepl("-0\\.0*([^0-9]|$)|-0([^0-9.]|$)", x)

  pct <- fmt_pct(small_neg, digits = 1)
  expect_true(all(!neg_zero(pct)))
  expect_equal(pct, rep("(0.0)", length(small_neg)))

  est <- fmt_est(small_neg)
  expect_true(all(!neg_zero(est)))

  ci <- fmt_ci(small_neg, small_neg + 0.01)
  expect_true(all(!neg_zero(ci)))
})

test_that("round_half_away_from_zero matches roundSAS reference", {
  roundSAS_ref <- function(x, digits = 0) {
    posneg <- sign(x)
    z <- abs(x) * 10^digits
    z <- z + 0.5 + sqrt(.Machine$double.eps)
    z <- trunc(z)
    z <- z / 10^digits
    z <- ifelse(!is.na(z) & z > 0, z * posneg, z)
    z
  }

  grid <- c(
    NA_real_, NaN, 0, -0, 0.04, -0.04, 0.05, -0.05, 0.15, -0.15,
    0.95, -0.95, 1.25, -1.25, 2.25, -2.25, 2.35, -2.35,
    12.345, -12.345, 123.456, -123.456, 0.125, -0.125, 2.675, -2.675
  )

  for (d in c(0, 1, 2, 3)) {
    expect_identical(
      round_half_away_from_zero(grid, digits = d),
      roundSAS_ref(grid, digits = d),
      info = paste0("digits = ", d)
    )
  }
})

test_that("boundary 2.25 rounds to 2.3 at digits = 1", {
  expect_equal(round_half_away_from_zero(2.25, digits = 1), 2.3)
  expect_equal(fmt_pct(2.25, digits = 1), "(2.3)")
  expect_equal(round_half_away_from_zero(-2.25, digits = 1), -2.3)
  expect_equal(fmt_pct(-2.25, digits = 1), "(-2.3)")
})
