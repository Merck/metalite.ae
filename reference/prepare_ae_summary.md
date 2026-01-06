# Prepare datasets for AE summary

Prepare datasets for AE summary

## Usage

``` r
prepare_ae_summary(meta, population, observation, parameter, ...)
```

## Arguments

- meta:

  A metadata object created by metalite.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- ...:

  Additional arguments passed to
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

## Value

A list of analysis datasets needed for AE summary.

## Examples

``` r
meta <- meta_ae_example()
prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
)
#> any
#> rel
#> ser
#> List of 13
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
```
