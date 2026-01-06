# Format model estimator

Formats mean sd/se to a format as x.x or x.x (x.xx) if both mean and
sd/sd are defined.

## Usage

``` r
fmt_est(
  mean,
  sd = rep(NA, length(mean)),
  digits = c(1, 1),
  width = c(4, 3) + digits
)
```

## Arguments

- mean:

  A numeric vector of mean value.

- sd:

  A numeric vector of standard deviation value.

- digits:

  Digits of each column, i.e., format as x.x (x.xx).

- width:

  Width of each column.

## Value

The same data frame with additional attributes for page features.

## Details

The function assumes 1 column or 2 columns:

- If there is only 1 column, only represent mean.

- If there are 2 columns, represent mean (sd) or mean(se). Decimals will
  understand the number will be formatted as x.x (x.xx).

## Specification

The contents of this section are shown in PDF user manual only.

## Examples

``` r
fmt_est(mean(iris$Petal.Length), sd(iris$Petal.Length))
#> [1] "  3.8 ( 1.8)"
fmt_est(mean(iris$Petal.Length), sd(iris$Petal.Length), digits = c(2, 3))
#> [1] "  3.76 ( 1.765)"
```
