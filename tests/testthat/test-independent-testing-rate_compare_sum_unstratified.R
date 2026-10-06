# The vectorized unstratified path (rate_compare_sum_unstratified(), PR #257)
# must return exactly what the per-term scalar rate_compare_sum() returns. These
# tests pin that equivalence so future edits to either path cannot drift apart.

# Reference: loop the scalar rate_compare_sum() one term at a time.
scalar_by_term <- function(n0, n1, x0, x1, delta = 0,
                           test = "one.sided", bisection = 100,
                           eps = 1e-06, alpha = 0.05) {
  rows <- lapply(seq_along(n0), function(i) {
    rate_compare_sum(
      n0 = n0[i], n1 = n1[i], x0 = x0[i], x1 = x1[i],
      delta = delta, weight = "ss", test = test,
      bisection = bisection, eps = eps, alpha = alpha
    )
  })
  do.call(rbind, rows)
}

expect_match_scalar <- function(batch, scal) {
  expect_equal(batch$est, scal$est, tolerance = 1e-10)
  expect_equal(batch$z_score, scal$z_score, tolerance = 1e-10)
  expect_equal(batch$p, scal$p, tolerance = 1e-10)
  expect_equal(batch$lower, scal$lower, tolerance = 1e-8)
  expect_equal(batch$upper, scal$upper, tolerance = 1e-8)
}

test_that("batch unstratified CIs match per-term rate_compare_sum() (random)", {
  set.seed(42)
  nt <- 300
  n0 <- sample(20:500, nt, replace = TRUE)
  n1 <- sample(20:500, nt, replace = TRUE)
  x0 <- vapply(n0, function(n) sample(0:n, 1), integer(1))
  x1 <- vapply(n1, function(n) sample(0:n, 1), integer(1))

  expect_match_scalar(
    rate_compare_sum_unstratified(n0, n1, x0, x1),
    scalar_by_term(n0, n1, x0, x1)
  )
})

test_that("batch unstratified CIs match scalar on edge cases", {
  # zero events, all events, single subject, equal rates, extreme split
  n0 <- c(100, 100, 1, 50, 200)
  n1 <- c(100, 100, 1, 50, 10)
  x0 <- c(0, 100, 0, 25, 200)
  x1 <- c(0, 100, 1, 25, 0)

  expect_match_scalar(
    rate_compare_sum_unstratified(n0, n1, x0, x1),
    scalar_by_term(n0, n1, x0, x1)
  )
})

test_that("batch unstratified matches scalar for nonzero delta and two.sided", {
  set.seed(7)
  nt <- 100
  n0 <- sample(30:300, nt, replace = TRUE)
  n1 <- sample(30:300, nt, replace = TRUE)
  x0 <- vapply(n0, function(n) sample(0:n, 1), integer(1))
  x1 <- vapply(n1, function(n) sample(0:n, 1), integer(1))

  expect_match_scalar(
    rate_compare_sum_unstratified(n0, n1, x0, x1, delta = 0.1, test = "two.sided"),
    scalar_by_term(n0, n1, x0, x1, delta = 0.1, test = "two.sided")
  )
})

test_that("batch unstratified returns an NA row for any NA input term", {
  n0 <- c(100, NA, 80)
  n1 <- c(100, 90, NA)
  x0 <- c(10, 5, 8)
  x1 <- c(20, 7, 9)
  out <- rate_compare_sum_unstratified(n0, n1, x0, x1)
  expect_equal(nrow(out), 3)
  # the two NA-bearing terms carry no estimate; the clean term does
  expect_false(is.na(out$est[1]))
  expect_true(all(is.na(c(out$est[2], out$est[3]))))
})
