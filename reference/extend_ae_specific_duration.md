# Add average duration information for AE specific analysis

Add average duration information for AE specific analysis

## Usage

``` r
extend_ae_specific_duration(outdata, duration_var, duration_unit = "Day")
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- duration_var:

  A character value of variable name for adverse event duration.

- duration_unit:

  A character value of adverse event duration unit.

## Value

A list of analysis raw datasets.

## Examples

``` r
meta <- meta_ae_example()
tbl <- prepare_ae_specific(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  extend_ae_specific_duration(duration_var = "ADURN") |>
  format_ae_specific(display = c("n", "prop", "dur"))
head(tbl$tbl)
#>                                             name n_1 prop_1        dur_1 n_2
#> 1                     Participants in population  86   <NA>         <NA>  84
#> 2   with one or more drug-related adverse events  44 (51.2)  29.0 ( 3.5)  73
#> 3            with no drug-related adverse events  42 (48.8)         <NA>  11
#> 4                                                 NA   <NA>         <NA>  NA
#> 122                            Cardiac disorders   6  (7.0)  27.1 ( 5.9)   7
#> 25                           Atrial fibrillation   1  (1.2)          6.0   0
#>     prop_2        dur_2 n_3 prop_3        dur_3
#> 1     <NA>         <NA>  84   <NA>         <NA>
#> 2   (86.9)  27.2 ( 3.2)  70 (83.3)  30.6 ( 2.2)
#> 3   (13.1)         <NA>  14 (16.7)         <NA>
#> 4     <NA>         <NA>  NA   <NA>         <NA>
#> 122  (8.3)  16.1 ( 3.5)   4  (4.8)   1.5 ( 0.4)
#> 25   (0.0)         <NA>   2  (2.4)   1.7 ( 0.7)
```
