# PR #257 vectorized two things in the unstratified case: the per-CI bisection
# grid scan inside rate_compare_sum(), and the across-terms path in the new
# rate_compare_sum_unstratified(). To prove neither drifted from the original,
# these tests compare BOTH current implementations against rate_compare_sum_old()
# -- a frozen verbatim copy of the pre-PR function (see helper-rate_compare_sum_old.R).

# Reference: loop the frozen OLD scalar function one term at a time.
# `weight` is intentionally left at its default: for a single unstratified term
# the stratum weight normalizes to 1, so est/z_score/p and the CI do not depend
# on it (weight only matters when combining multiple strata).
old_by_term <- function(n0, n1, x0, x1, delta = 0,
                        test = "one.sided", bisection = 100,
                        eps = 1e-06, alpha = 0.05) {
  rows <- lapply(seq_along(n0), function(i) {
    rate_compare_sum_old(
      n0 = n0[i], n1 = n1[i], x0 = x0[i], x1 = x1[i],
      delta = delta, test = test, bisection = bisection, eps = eps, alpha = alpha
    )
  })
  do.call(rbind, rows)
}

# Reference: loop the CURRENT scalar rate_compare_sum() one term at a time.
scalar_by_term <- function(n0, n1, x0, x1, delta = 0,
                           test = "one.sided", bisection = 100,
                           eps = 1e-06, alpha = 0.05) {
  rows <- lapply(seq_along(n0), function(i) {
    rate_compare_sum(
      n0 = n0[i], n1 = n1[i], x0 = x0[i], x1 = x1[i],
      delta = delta, test = test, bisection = bisection, eps = eps, alpha = alpha
    )
  })
  do.call(rbind, rows)
}

expect_match_old <- function(got, old) {
  expect_equal(got$est, old$est, tolerance = 1e-10)
  expect_equal(got$z_score, old$z_score, tolerance = 1e-10)
  expect_equal(got$p, old$p, tolerance = 1e-10)
  expect_equal(got$lower, old$lower, tolerance = 1e-8)
  expect_equal(got$upper, old$upper, tolerance = 1e-8)
}

# Assert both the current scalar path and the batch path reproduce the OLD
# function, across every alpha the function is used with in practice.
check_both_vs_old <- function(n0, n1, x0, x1, delta = 0, test = "one.sided") {
  for (alpha in c(0.025, 0.05)) {
    old <- old_by_term(n0, n1, x0, x1, delta = delta, test = test, alpha = alpha)
    # current scalar rate_compare_sum(), term by term
    expect_match_old(
      scalar_by_term(n0, n1, x0, x1, delta = delta, test = test, alpha = alpha),
      old
    )
    # new vectorized across-terms path
    expect_match_old(
      rate_compare_sum_unstratified(n0, n1, x0, x1, delta = delta, test = test, alpha = alpha),
      old
    )
  }
}

test_that("current scalar and batch match the OLD function (random inputs)", {
  set.seed(42)
  nt <- 300
  n0 <- sample(20:500, nt, replace = TRUE)
  n1 <- sample(20:500, nt, replace = TRUE)
  x0 <- vapply(n0, function(n) sample(0:n, 1), integer(1))
  x1 <- vapply(n1, function(n) sample(0:n, 1), integer(1))
  check_both_vs_old(n0, n1, x0, x1)
})

test_that("current scalar and batch match the OLD function (edge cases)", {
  # zero events, all events, single subject, equal rates, extreme split
  n0 <- c(100, 100, 1, 50, 200)
  n1 <- c(100, 100, 1, 50, 10)
  x0 <- c(0, 100, 0, 25, 200)
  x1 <- c(0, 100, 1, 25, 0)
  check_both_vs_old(n0, n1, x0, x1)
})

test_that("current scalar and batch match the OLD function (nonzero delta, two.sided)", {
  set.seed(7)
  nt <- 100
  n0 <- sample(30:300, nt, replace = TRUE)
  n1 <- sample(30:300, nt, replace = TRUE)
  x0 <- vapply(n0, function(n) sample(0:n, 1), integer(1))
  x1 <- vapply(n1, function(n) sample(0:n, 1), integer(1))
  check_both_vs_old(n0, n1, x0, x1, delta = 0.1, test = "two.sided")
})

test_that("batch returns an NA row for any NA input term", {
  # The old scalar function early-returns a single NA row for ANY NA input, so
  # it cannot be looped per term here; assert the batch's per-term NA handling.
  n0 <- c(100, NA, 80)
  n1 <- c(100, 90, NA)
  x0 <- c(10, 5, 8)
  x1 <- c(20, 7, 9)
  out <- rate_compare_sum_unstratified(n0, n1, x0, x1)
  expect_equal(nrow(out), 3)
  expect_false(is.na(out$est[1]))
  expect_true(all(is.na(c(out$est[2], out$est[3]))))
})
