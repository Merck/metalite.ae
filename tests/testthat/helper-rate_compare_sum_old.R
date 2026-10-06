# Frozen pre-PR #257 copy of rate_compare_sum(), taken verbatim from commit
# 26ba273^ (f6e088e), before the bisection grid was vectorized and the
# across-terms rate_compare_sum_unstratified() path was added. Renamed to
# rate_compare_sum_old() so the equivalence tests can compare the current
# implementations against the genuine original, not against each other.
# Do not edit: it is a reference oracle, intentionally identical to the old code.

rate_compare_sum_old <- function(
  n0, n1,
  x0, x1,
  strata = NULL,
  delta = 0,
  weight = c("ss", "equal", "cmh"),
  test = c("one.sided", "two.sided"),
  bisection = 100,
  eps = 1e-06,
  alpha = 0.05
) {
  if (any(is.na(c(n0, n1, x0, x1)))) {
    z <- data.frame(
      est = NA, z_score = NA,
      p = NA, lower = NA, upper = NA
    )
    return(z)
  }

  test <- match.arg(test)
  weight <- match.arg(weight)

  len <- c(length(n0), length(n1), length(x0), length(x1))
  if (!is.null(strata)) len <- c(len, length(strata))
  if (max(len) != min(len)) {
    stop(
      "The length of input arguments ",
      "`n0`, `n1`, `x0`, `x1`, and `strata` are different.",
      call. = FALSE
    )
  }

  # Count the event
  n <- n0 + n1
  c <- x0 + x1
  r1 <- x1 / n1
  r0 <- x0 / n0

  # start the analysis
  l3 <- n
  l2 <- (n1 + 2 * n0) * delta - n - c
  l1 <- (n0 * delta - n - 2 * x0) * delta + c
  l0 <- x0 * delta * (1 - delta)

  q <- (l2 / (3 * l3))^3 - l1 * l2 / (6 * l3^2) + l0 / (2 * l3)
  sign <- ifelse(q > 0, 1, -1)
  p <- sqrt((l2 / (3 * l3))^2 - l1 / (3 * l3)) * sign

  # Calculate R tilter
  temp <- q / (p^3)
  # To limit this temp within (-1, 1)
  temp <- pmax(pmin(temp, 1), -1)
  a <- (pi + acos(temp)) / 3

  # Start to calculate R tilter
  r0t <- 2 * p * cos(a) - l2 / (3 * l3)
  r0t <- pmax(pmin(r0t, pmin(1, 1 - delta)), pmax(0, -delta))
  r1t <- r0t + delta
  vart <- (r1t * (1 - r1t) / n1 + r0t * (1 - r0t) / n0) * (n / (n - 1))

  if (is.null(strata) || length(unique(strata)) == 1) {
    r_diff <- (r1 - r0)
    z_score <- if (isTRUE(r_diff == delta) && isTRUE(vart == 0)) {
      0
    } else {
      (r_diff - delta) / sqrt(vart)
    }
  }
  if (!length(unique(strata)) == 1) {
    # Start to calculate the Chi-square
    w <- switch(weight,
      equal = rep(1, length(strata)),
      ss = n / sum(n),
      cmh = (n0 * n1 / n) / sum(n0 * n1 / n)
    )
    r1_w <- r1 * w
    r0_w <- r0 * w
    var_w <- w^2 * vart
    r_diff <- (sum(r1_w) - sum(r0_w))
    z_score <- if (isTRUE(r_diff == delta) && isTRUE(sum(var_w) == 0)) {
      0
    } else {
      (r_diff - delta) / sqrt(sum(var_w))
    }
  }

  pval <- if (isTRUE(z_score == 0) && all(c(x0, x1) == 0)) {
    1
  } else {
    switch(test,
      one.sided = ifelse(delta <= 0, 1 - pnorm(z_score), pnorm(z_score)),
      two.sided = 1 - pchisq(z_score^2, 1)
    )
  }

  # Bisection function to find the roots:
  # `f` is the function for which the root is sought,
  # `a` and `b` are minimum and maximum of the interval,
  # which contains the root from the bisection method.
  #
  # The scan walks a grid of `bisection` intervals looking for sign changes.
  # Adjacent intervals share an endpoint, so the right-edge value of one
  # interval is the left-edge value of the next: we carry `fb` forward into
  # `fa` instead of re-evaluating `f` there, roughly halving the number of
  # `f` calls during the scan.
  biroot <- function(f, a, b) {
    h <- abs(b - a) / bisection
    j <- 0
    roots <- c()

    # The right endpoint of interval i is the left endpoint of interval i + 1,
    # so we carry its function value forward instead of recomputing it.
    a1 <- a
    fa <- f(a1)

    i <- 0
    while (i <= bisection) {
      b1 <- a1 + h

      # Evaluate function safely
      fb <- f(b1)

      # Skip intervals where fa or fb are NA/NaN/Inf
      if (is.finite(fa) && is.finite(fb) && (fa * fb < 0)) {
        # Refine within a private copy of the bracket so the carried-forward
        # scan endpoints (`a1`, `fa`) are not clobbered.
        lo <- a1
        hi <- b1
        flo <- fa
        repeat {
          if (abs(hi - lo) < eps) {
            break
          }

          x <- (lo + hi) / 2
          fx <- f(x)

          # If fx is NA/NaN/Inf, break and skip this interval
          if (!is.finite(fx)) break

          if (flo * fx < 0) {
            hi <- x
          } else {
            lo <- x
            flo <- fx
          }
        }

        j <- j + 1
        roots[j] <- (lo + hi) / 2
      }

      # Advance the grid, reusing the right endpoint as the next left endpoint.
      a1 <- b1
      fa <- fb

      i <- i + 1
    }

    if (j == 0) {
      message(
        "After ", bisection,
        " intervals, no lower or upper CI limit was found. ",
        "Try increasing `bisection` or adjusting the search interval (a, b) for lower or uppder limit."
      )
      return(NA)
    } else {
      return(roots)
    }
  }

  # Loop-invariant quantities pulled out of `func_d`, which is called once per
  # bisection grid point (hundreds of times per CI). The chi-square critical
  # value and the stratified/unstratified branch do not depend on `d`.
  chisq_crit <- qchisq(1 - alpha, 1)
  unstratified <- is.null(strata) || length(unique(strata)) == 1

  # Start to calculate the confidence interval
  func_d <- function(d) {
    l3 <- n
    l2 <- (n1 + 2 * n0) * d - n - c
    l1 <- (n0 * d - n - 2 * x0) * d + c
    l0 <- x0 * d * (1 - d)

    q <- (l2 / (3 * l3))^3 - l1 * l2 / (6 * l3^2) + l0 / (2 * l3)
    sign <- ifelse(q > 0, 1, -1)
    p <- sqrt((l2 / (3 * l3))^2 - l1 / (3 * l3)) * sign
    # Adust p
    p <- ifelse(p > (-1e-20) & p < 0,
      p - 1e-16,
      ifelse(
        p >= 0 & p < (1e-20),
        p + 1e-16,
        p
      )
    )
    # Calculate R tilter
    temp <- q / (p^3)
    # To limit this temp within (-1, 1)
    temp <- pmax(pmin(temp, 1), -1)
    a <- (pi + acos(temp)) / 3
    # Start to calculate R tilter
    r0t <- 2 * p * cos(a) - l2 / (3 * l3)
    r0t <- pmax(pmin(r0t, pmin(1, 1 - d)), pmax(0, -d))
    r1t <- r0t + d
    vart <- (r1t * (1 - r1t) / n1 + r0t * (1 - r0t) / n0) * (n / (n - 1))

    if (unstratified) {
      r_diff <- (x1 / n1 - x0 / n0)
      chisq_obs <- if (isTRUE(r_diff == d) && isTRUE(vart == 0)) {
        0
      } else {
        (r_diff - d)^2 / vart
      }
    } else {
      # Start to calculate the Chi-square
      r1_w <- r1 * w
      r0_w <- r0 * w
      var_w <- w^2 * vart
      vs <- sum(var_w)

      r_diff <- sum(r1_w) - sum(r0_w)
      chisq_obs <- if (isTRUE(r_diff == d) && isTRUE(vs == 0)) {
        0
      } else {
        (r_diff - d)^2 / vs
      }
    }
    return(chisq_obs - chisq_crit)
  }

  ci <- biroot(f = func_d, a = -0.999, b = 0.999)
  ci <- ci[(abs(ci) < 1)]

  z <- data.frame(
    est = r_diff, z_score = z_score,
    p = pval, lower = ci[1], upper = ci[2]
  )
  z
}
