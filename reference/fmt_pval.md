# Format p-value

Format p-value

## Usage

``` r
fmt_pval(p, digits = 3, width = 3 + digits)
```

## Arguments

- p:

  A numeric vector of p-values.

- digits:

  Digits of each column, i.e., format as x.xxx.

- width:

  Width of each column.

## Value

A numeric vector with the expected format.

## Examples

``` r
fmt_pval(c(0.1234, 0.00002))
#> [1] " 0.123" "<0.001"
```
