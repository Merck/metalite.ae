# Unstratified and stratified Miettinen and Nurminen test

Unstratified and stratified Miettinen and Nurminen test details can be
found in
[`vignette("rate-compare")`](https://merck.github.io/metalite.ae/articles/rate-compare.md).

## Usage

``` r
rate_compare(
  formula,
  strata,
  data,
  delta = 0,
  weight = c("ss", "equal", "cmh"),
  test = c("one.sided", "two.sided"),
  bisection = 100,
  eps = 1e-06,
  alpha = 0.05
)
```

## Arguments

- formula:

  A symbolic description of the model to be fitted, which has the form
  `y ~ x`. Here, `y` is the numeric vector with values of 0 or 1. `x` is
  the group information.

- strata:

  An optional vector of weights to be used in the analysis. If not
  specified, unstratified MN analysis is used. If specified, stratified
  MN analysis is conducted.

- data:

  An optional data frame, list, or environment containing the variables
  in the model. If not found in data, the variables are taken from
  `environment (formula)`, typically the environment from which
  `rate_compare` is called.

- delta:

  A numeric value to set the difference of two group under the null.

- weight:

  Weighting schema used in stratified MN method. Default is `"ss"`:

  - `"equal"` for equal weighting.

  - `"ss"` for sample size weighting.

  - `"cmh"` for Cochran–Mantel–Haenszel's weights.

- test:

  A character string specifying the side of p-value, must be one of
  `"one.sided"`, or `"two.sided"`.

- bisection:

  The number of sections in the interval used in bisection method.
  Default is 100.

- eps:

  The level of precision. Default is 1e-06.

- alpha:

  Pre-defined alpha level for two-sided confidence interval.

## Value

A data frame with the test results.

## References

Miettinen, O. and Nurminen, M, Comparative Analysis of Two Rates.
*Statistics in Medicine*, 4(2):213–226, 1985.

## Examples

``` r
# Conduct the stratified MN analysis with sample size weights
treatment <- c(rep("pbo", 100), rep("exp", 100))
response <- c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60))
stratum <- c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
rate_compare(
  response ~ factor(treatment, levels = c("pbo", "exp")),
  strata = stratum,
  delta = 0,
  weight = "ss",
  test = "one.sided",
  alpha = 0.05
)
#>         est  z_score            p     lower     upper
#> 1 0.3998397 5.712797 5.556727e-09 0.2684383 0.5172779
```
