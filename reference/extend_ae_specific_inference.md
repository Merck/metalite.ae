# Add inference information for AE specific analysis

Add inference information for AE specific analysis

## Usage

``` r
extend_ae_specific_inference(outdata, ..., ci = 0.95)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- ...:

  Other options passed on to
  [`rate_compare_sum()`](https://merck.github.io/metalite.ae/reference/rate_compare_sum.md)

- ci:

  A numeric value for the percentile of confidence interval.

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
  extend_ae_specific_inference(eps = 1e-6, bisection = 200) |>
  format_ae_specific(display = c("n", "prop", "diff", "diff_ci"))
head(tbl$tbl)
#>                                             name n_1 prop_1 n_2 prop_2 n_3
#> 1                     Participants in population  86   <NA>  84   <NA>  84
#> 2   with one or more drug-related adverse events  44 (51.2)  73 (86.9)  70
#> 3            with no drug-related adverse events  42 (48.8)  11 (13.1)  14
#> 4                                                 NA   <NA>  NA   <NA>  NA
#> 122                            Cardiac disorders   6  (7.0)   7  (8.3)   4
#> 25                           Atrial fibrillation   1  (1.2)   0  (0.0)   2
#>     prop_3 diff_2           ci_2 diff_3           ci_3
#> 1     <NA>   <NA>   (-4.4, -0.0)   <NA>   (-4.4, -0.0)
#> 2   (83.3)   35.7   (22.4, 48.0)   32.2   (18.4, 44.8)
#> 3   (16.7)  -35.7 (-48.0, -22.4)  -32.2 (-44.8, -18.4)
#> 4     <NA>   <NA>           <NA>   <NA>           <NA>
#> 122  (4.8)    1.4   (-7.3, 10.2)   -2.2  (-10.3,  5.6)
#> 25   (2.4)   -1.2   (-6.3,  3.3)    1.2   (-4.2,  7.3)
```
