# Add average number of events information for AE specific analysis

Add average number of events information for AE specific analysis

## Usage

``` r
extend_ae_specific_events(outdata)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

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
  extend_ae_specific_events() |>
  format_ae_specific(display = c("n", "prop", "events_avg"))
head(tbl$tbl)
#>                                             name n_1 prop_1  eventsavg_1 n_2
#> 1                     Participants in population  86   <NA>         <NA>  84
#> 2   with one or more drug-related adverse events  44 (51.2)   0.7 ( 0.1)  73
#> 3            with no drug-related adverse events  42 (48.8)         <NA>  11
#> 4                                                 NA   <NA>         <NA>  NA
#> 122                            Cardiac disorders   6  (7.0)   2.3 ( 0.6)   7
#> 25                           Atrial fibrillation   1  (1.2)          1.0   0
#>     prop_2  eventsavg_2 n_3 prop_3  eventsavg_3
#> 1     <NA>         <NA>  84   <NA>         <NA>
#> 2   (86.9)   1.6 ( 0.2)  70 (83.3)   1.5 ( 0.2)
#> 3   (13.1)         <NA>  14 (16.7)         <NA>
#> 4     <NA>         <NA>  NA   <NA>         <NA>
#> 122  (8.3)   1.9 ( 0.4)   4  (4.8)   1.2 ( 0.2)
#> 25   (0.0)         <NA>   2  (2.4)   1.5 ( 0.5)
```
