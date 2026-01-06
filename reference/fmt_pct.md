# Format percentage

Format percentage

## Usage

``` r
fmt_pct(x, digits = 1, pre = "(", post = ")")
```

## Arguments

- x:

  A numeric vector.

- digits:

  Number of digits.

- pre:

  Text before the number.

- post:

  Text after the number.

## Value

A numeric vector with the expected format.

## Examples

``` r
fmt_pct(c(1, 1.52, 0.3, 100))
#> [1] "  (1.0)" "  (1.5)" "  (0.3)" "(100.0)"
```
