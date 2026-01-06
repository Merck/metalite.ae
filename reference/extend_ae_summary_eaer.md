# Add exposure-adjusted rate information for AE summary analysis

Add exposure-adjusted rate information for AE summary analysis

## Usage

``` r
extend_ae_summary_eaer(
  outdata,
  duration_var = "TRTDUR",
  adj_unit = c("year", "month", "week", "day")
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_summary()`](https://merck.github.io/metalite.ae/reference/prepare_ae_summary.md).

- duration_var:

  A character value of duration variable name. By default, `"TRTDUR"` is
  used.

- adj_unit:

  A character value of exposure adjusted unit. It could be select from
  `"year"`, `"month"`, `"week"`, and `"day"`.

## Value

A list of analysis raw datasets.

## Examples

``` r
meta <- meta_ae_example()
prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
) |>
  extend_ae_summary_eaer()
#> any
#> rel
#> ser
#> any
#> rel
#> ser
#> List of 17
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "any;rel;ser"
#>  $ n              :'data.frame': 5 obs. of  4 variables:
#>  $ order          : num [1:5] 1 100 200 300 400
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 5 obs. of  4 variables:
#>  $ diff           :'data.frame': 5 obs. of  2 variables:
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>  $ name           : chr [1:5] "Participants in population" "with one or more adverse events" "with no adverse events" "with drug-related{^a} adverse events" ...
#>  $ prepare_call   : language prepare_ae_summary(meta = meta, population = "apat", observation = "wk12",      parameter = "any;rel;ser")
#>  $ total_exp      :'data.frame': 1 obs. of  4 variables:
#>  $ event_num      :'data.frame': 3 obs. of  4 variables:
#>  $ eaer           :'data.frame': 3 obs. of  4 variables:
#>  $ adj_unit       : chr "year"
```
