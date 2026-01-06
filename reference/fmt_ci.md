# Format confidence interval

Format confidence interval

## Usage

``` r
fmt_ci(lower, upper, digits = 2, width = 3 + digits)
```

## Arguments

- lower:

  A numeric value of lower value of CI.

- upper:

  A numeric value of upper value of CI.

- digits:

  Digits of each column, i.e., format as (x.x, x.x).

- width:

  Width of each column.

## Value

A numeric vector with the expected format.

## Examples

``` r
fmt_ci(0.2356, 0.3871)
#> [1] "( 0.24,  0.39)"
```
