# Format exposure-adjusted AE summary

Format exposure-adjusted AE summary

## Usage

``` r
format_ae_exp_adj(
  outdata,
  display = c("n", "total_exp", "events", "eaer", "total"),
  digits_total_exp = 2,
  digits_eaer = 2,
  mock = FALSE
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- display:

  A character vector of measurement to be displayed:

  - `n`: Number of subjects exposed.

  - `total_exp`: Total exposure in person-time.

  - `events`: Number of AE.

  - `eaer`: Exposure adjusted event rate.

  - `total`: Total columns.

- digits_total_exp:

  A numeric value of number of digits for total exposure value.

- digits_eaer:

  A numeric value of number of digits for exposure-adjusted event rate.

- mock:

  A boolean value to display mock table.

## Value

A list of analysis raw datasets.

## Examples

``` r
meta <- meta_ae_example()

outdata <- meta |>
  prepare_ae_summary(
    population = "apat",
    observation = "wk12",
    parameter = "any;ser;rel"
  ) |>
  extend_ae_summary_eaer(adj_unit = "month")
#> any
#> ser
#> rel
#> any
#> ser
#> rel
tbl <- outdata |>
  format_ae_exp_adj()
head(tbl$tbl)
#>                              name     Placebo     Low.Dose    High.Dose
#> 1  Number of Participants exposed          86           84           84
#> 2  Total exposure in person-month      421.20       273.29       274.31
#> 3                  adverse events 301 (71.46) 435 (159.17) 455 (165.87)
#> 4          serious adverse events    0 (0.00)     1 (0.37)     2 (0.73)
#> 5 drug-related{^b} adverse events 133 (31.58) 292 (106.85) 279 (101.71)
#>           Total           row_label
#> 1           254               -----
#> 2        968.80               -----
#> 3 1191 (122.94) Total events (rate)
#> 4      3 (0.31) Total events (rate)
#> 5   704 (72.67) Total events (rate)
```
