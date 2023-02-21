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

#' Unstratified and stratified  Miettinen and Nurminen test
#'
#' Unstratified and stratified  Miettinen and Nurminen test.
#' Details can be found in `vignette("rate-compare")`.
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
rate_compare <- function(formula,
                         strata,
                         data,
                         delta = 0,
                         weight = c("ss", "equal", "cmh"),
                         test = c("one.sided", "two.sided"),
                         bisection = 100,
                         eps = 1e-06,
                         alpha = 0.05) {
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
    delta = 0,
    weight = weight,
    test = test,
    bisection = bisection,
    eps = eps,
    alpha = alpha
  )
}
