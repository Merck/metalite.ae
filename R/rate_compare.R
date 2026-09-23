# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the metalite.ae program.
#
# metalite.ae is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Unstratified and stratified Miettinen and Nurminen test
#'
#' Unstratified and stratified Miettinen and Nurminen test details can be found
#' in `vignette("rate-compare")`.
#'
#' @param formula A symbolic description of the model to be fitted,
#'   which has the form `y ~ x`. Here, `y` is the numeric vector
#'   with values of 0 or 1. `x` is the group information.
#' @param strata An optional vector of weights to be used in the analysis.
#'   If not specified, unstratified MN analysis is used.
#'   If specified, stratified MN analysis is conducted.
#' @param data An optional data frame, list, or environment containing
#'   the variables in the model.
#'   If not found in data, the variables are taken from `environment (formula)`,
#'   typically the environment from which `rate_compare` is called.
#' @param delta A numeric value to set the difference of two group
#'   under the null.
#' @param weight Weighting schema used in stratified MN method.
#'   Default is `"ss"`:
#'   - `"equal"` for equal weighting.
#'   - `"ss"` for sample size weighting.
#'   - `"cmh"` for Cochran–Mantel–Haenszel's weights.
#' @param test A character string specifying the side of p-value,
#'   must be one of `"one.sided"`, or `"two.sided"`.
#' @param bisection The number of sections in the interval used in
#'   bisection method. Default is 100.
#' @param eps The level of precision. Default is 1e-06.
#' @param alpha Pre-defined alpha level for two-sided confidence interval.
#'
#' @return A data frame with the test results.
#'
#' @references
#' Miettinen, O. and Nurminen, M, Comparative Analysis of Two Rates.
#' _Statistics in Medicine_, 4(2):213--226, 1985.
#'
#' @export
#'
#' @examples
#' # Conduct the stratified MN analysis with sample size weights
#' treatment <- c(rep("pbo", 100), rep("exp", 100))
#' response <- c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60))
#' stratum <- c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
#' rate_compare(
#'   response ~ factor(treatment, levels = c("pbo", "exp")),
#'   strata = stratum,
#'   delta = 0,
#'   weight = "ss",
#'   test = "one.sided",
#'   alpha = 0.05
#' )
rate_compare <- function(
  formula,
  strata,
  data,
  delta = 0,
  weight = c("ss", "equal", "cmh"),
  test = c("one.sided", "two.sided"),
  bisection = 100,
  eps = 1e-06,
  alpha = 0.05
) {
  test <- match.arg(test)
  weight <- match.arg(weight)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "strata"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  response <- stats::model.response(mf, "numeric")
  treatment <- mf[, 2L]

  # Count the event
  if (missing(strata)) {
    strata2 <- NULL
    strt <- sapply(split(response, treatment), sum)
    ntrt <- sapply(split(response, treatment), length)
    rtrt <- strt / ntrt
    trt <- names(ntrt)
    strata_re <- 1
  }
  if (!missing(strata)) {
    strata2 <- mf[, 3L]
    strt <- sapply(split(response, paste(t(strata2), treatment, sep = "_")), sum)
    ntrt <- sapply(split(response, paste(t(strata2), treatment, sep = "_")), length)
    str <- sapply(strsplit(names(ntrt), "_"), "[", 1)
    trt <- sapply(strsplit(names(ntrt), "_"), "[", 2)
    strata_re <- unique(strata2)
  }
  n0 <- ntrt[trt == names(table(treatment))[1]]
  n1 <- ntrt[trt == names(table(treatment))[2]]
  x0 <- strt[trt == names(table(treatment))[1]]
  x1 <- strt[trt == names(table(treatment))[2]]
  n <- n0 + n1
  c <- x0 + x1
  r1 <- x1 / n1
  r0 <- x0 / n0

  rate_compare_sum(
    n0, n1, x0, x1,
    strata_re,
    delta = delta,
    weight = weight,
    test = test,
    bisection = bisection,
    eps = eps,
    alpha = alpha
  )
}

#' Unstratified and stratified Miettinen and Nurminen test in
#' aggregate data level
#'
#' @param n0,n1 The sample size in the control group and experimental group,
#'   separately. The length should be the same as the length for
#'   `x0/x1` and `strata`.
#' @param x0,x1 The number of events in the control group and
#'   experimental group, separately. The length should be the same
#'   as the length for `n0/n1` and `strata`.
#' @param strata A vector of stratum indication to be used in the analysis.
#'   If `NULL` or the length of unique values of `strata` equals to 1,
#'   it is unstratified MN analysis. Otherwise, it is stratified MN analysis.
#'   The length of `strata` should be the same as the length for
#'   `x0/x1` and `n0/n1`.
#' @param delta A numeric value to set the difference of two groups
#'   under the null.
#' @param weight Weighting schema used in stratified MN method.
#'   Default is `"ss"`:
#'   - `"equal"` for equal weighting.
#'   - `"ss"` for sample size weighting.
#'   - `"cmh"` for Cochran-Mantel-Haenszel's weights.
#' @param test A character string specifying the side of p-value,
#'   must be one of `"one.sided"`, or `"two.sided"`.
#' @param bisection The number of sections in the interval used in
#'   bisection method. Default is 100.
#' @param eps The level of precision. Default is 1e-06.
#' @param alpha Pre-defined alpha level for two-sided confidence interval.
#'
#' @return A data frame with the test results.
#'
#' @references
#' Miettinen, O. and Nurminen, M, Comparative Analysis of Two Rates.
#' _Statistics in Medicine_, 4(2):213--226, 1985.
#'
#' @importFrom stats pnorm pchisq qchisq
#'
#' @export
#'
#' @examples
#' # Conduct the stratified MN analysis with sample size weights
#' treatment <- c(rep("pbo", 100), rep("exp", 100))
#' response <- c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60))
#' stratum <- c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
#' n0 <- sapply(split(treatment[treatment == "pbo"], stratum[treatment == "pbo"]), length)
#' n1 <- sapply(split(treatment[treatment == "exp"], stratum[treatment == "exp"]), length)
#' x0 <- sapply(split(response[treatment == "pbo"], stratum[treatment == "pbo"]), sum)
#' x1 <- sapply(split(response[treatment == "exp"], stratum[treatment == "exp"]), sum)
#' strata <- c("a", "b", "c", "d")
#' rate_compare_sum(
#'   n0, n1, x0, x1,
#'   strata,
#'   delta = 0,
#'   weight = "ss",
#'   test = "one.sided",
#'   alpha = 0.05
#' )
rate_compare_sum <- function(
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
