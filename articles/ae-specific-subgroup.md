# Subgroup Analysis for Specific AE

``` r
library(metalite.ae)
```

## Overview

The AE specific subgroup analysis aims to provide tables to summarize
details of adverse events by subgroup. The development of AE specific
subgroup analysis involves exported functions:

- [`prepare_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific_subgroup.md):
  prepare analysis raw datasets.
- [`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md):
  prepare analysis (mock) outdata with proper format.
- [`tlf_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific_subgroup.md):
  transfer (mock) output dataset to RTF table.

### Analysis preparation

The
[`prepare_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific_subgroup.md)
function is designed to be used for multiple purposes. The input of the
function is a `meta` object created by the metalite package.

``` r
meta <- meta_ae_example()
```

The output of the function is an `outdata` object containing a list of
analysis raw datasets. Key arguments are `subgroup_var`,
`subgroup_header`, and `display_subgroup_total`.

``` r
outdata <- prepare_ae_specific_subgroup(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  subgroup_var = "SEX",
  subgroup_header = c("TRTA", "SEX"),
  display_subgroup_total = TRUE
)
```

``` r
outdata
#> $components
#> [1] "soc" "par"
#> 
#> $group
#> [1] "Placebo"   "Low Dose"  "High Dose"
#> 
#> $subgroup
#> [1] "f" "m"
#> 
#> $display_subgroup_total
#> [1] TRUE
#> 
#> $meta
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 20 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var          subset           label
#> 1 'wk12' 'USUBJID' 'TRTA'        SAFFL == 'Y' 'Weeks 0 to 12'
#> 2 'wk24' 'USUBJID' 'TRTA'     AOCC01FL == 'Y' 'Weeks 0 to 24'
#> 
#> 
#>   Analysis parameter type:
#>        name                                         label
#> 1     'rel'                 'drug-related adverse events'
#> 2   'aeosi'          'adverse events of special interest'
#> 3 'dtc0rel' 'drug-related adverse events result in death'
#> 4     'any'                          'any adverse events'
#> 5     'ser'                      'serious adverse events'
#>                                 subset
#> 1 AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 2                         AEOSI == 'Y'
#> 3         AESDTH == 'Y' & AEREL == 'Y'
#> 4                                     
#> 5                         AESER == 'Y'
#> 
#> 
#>   Analysis function:
#>            name                             label
#> 1  'ae_summary'    'Table: adverse event summary'
#> 2  'ae_listing'          'Listing: adverse event'
#> 3  'ae_exp_adj' 'Exposure Adjusted Incident Rate'
#> 4 'ae_specific'   'Table: specific adverse event'
#> 
#> 
#> $population
#> [1] "apat"
#> 
#> $observation
#> [1] "wk12"
#> 
#> $parameter
#> [1] "rel"
#> 
#> $out_all
#> $out_all$F
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 138 obs. of  4 variables:
#>  $ order          : num [1:138] 1 100 200 900 1000 ...
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 138 obs. of  4 variables:
#>  $ diff           :'data.frame': 138 obs. of  2 variables:
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>  $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:138] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
#> 
#> $out_all$M
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 138 obs. of  4 variables:
#>  $ order          : num [1:138] 1 100 200 900 1000 ...
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 138 obs. of  4 variables:
#>  $ diff           :'data.frame': 138 obs. of  2 variables:
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>  $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:138] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
#> 
#> $out_all$Total
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 138 obs. of  4 variables:
#>  $ order          : num [1:138] 1 100 200 900 10000 ...
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 138 obs. of  4 variables:
#>  $ diff           :'data.frame': 138 obs. of  2 variables:
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>  $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:138] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language prepare_ae_specific(meta = meta, population = population, observation = observation,      parameter = parameter, | __truncated__
```

The output dataset contains commonly used statistics within each
`subgroup_var`.

``` r
outdata$out_all$F
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 138 obs. of  4 variables:
#>  $ order          : num [1:138] 1 100 200 900 1000 ...
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 138 obs. of  4 variables:
#>  $ diff           :'data.frame': 138 obs. of  2 variables:
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>  $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:138] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
outdata$out_all$M
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 138 obs. of  4 variables:
#>  $ order          : num [1:138] 1 100 200 900 1000 ...
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 138 obs. of  4 variables:
#>  $ diff           :'data.frame': 138 obs. of  2 variables:
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>  $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:138] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
outdata$out_all$Total
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 138 obs. of  4 variables:
#>  $ order          : num [1:138] 1 100 200 900 10000 ...
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 138 obs. of  4 variables:
#>  $ diff           :'data.frame': 138 obs. of  2 variables:
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>  $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:138] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language prepare_ae_specific(meta = meta, population = population, observation = observation,      parameter = parameter, | __truncated__
```

The variable is indexed by the order of `outdata$group` and
`outdata$subgroup` within each `subgroup_var`.

``` r
outdata$group
#> [1] "Placebo"   "Low Dose"  "High Dose"
outdata$subgroup
#> [1] "f" "m"
```

The row is indexed by the order of `name` within each `subgroup_var`
analysis output.

``` r
head(data.frame(outdata$out_all$Total$order, outdata$out_all$Total$name))
#>   outdata.out_all.Total.order                   outdata.out_all.Total.name
#> 1                           1                   Participants in population
#> 2                         100 with one or more drug-related adverse events
#> 3                         200          with no drug-related adverse events
#> 4                         900                                             
#> 5                       10000                            Cardiac disorders
#> 6                       10021                          Atrial fibrillation
```

- `n_pop`: participants in population within each `subgroup_var`.

``` r
outdata$out_all$F$n_pop
#>   n_1 n_2 n_3 n_4
#> 1  53  50  40 143
outdata$out_all$M$n_pop
#>   n_1 n_2 n_3 n_4
#> 1  33  34  44 111
outdata$out_all$Total$n_pop
#>   n_1 n_2 n_3 n_4
#> 1  86  84  84 254
```

- `n`: number of subjects with AE within each `subgroup_var`.

``` r
head(outdata$out_all$F$n)
#>     n_1 n_2 n_3 n_4
#> 1    53  50  40 143
#> 2    28  41  32 101
#> 3    25   9   8  42
#> 4    NA  NA  NA  NA
#> 122   4   4   4  12
#> 25    1   0   2   3
head(outdata$out_all$M$n)
#>     n_1 n_2 n_3 n_4
#> 1    33  34  44 111
#> 2    16  32  38  86
#> 3    17   2   6  25
#> 4    NA  NA  NA  NA
#> 122   2   3   0   5
#> 26    0   1   0   1
head(outdata$out_all$Total$n)
#>     n_1 n_2 n_3 n_4
#> 1    86  84  84 254
#> 2    44  73  70 187
#> 3    42  11  14  67
#> 4    NA  NA  NA  NA
#> 122   6   7   4  17
#> 25    1   0   2   3
```

- `prop`: proportion of subjects with AE within each `subgroup_var`.

``` r
head(outdata$out_all$F$prop)
#>        prop_1 prop_2 prop_3    prop_4
#> 1          NA     NA     NA        NA
#> 2   52.830189     82     80 70.629371
#> 3   47.169811     18     20 29.370629
#> 4          NA     NA     NA        NA
#> 122  7.547170      8     10  8.391608
#> 25   1.886792      0      5  2.097902
head(outdata$out_all$M$prop)
#>        prop_1    prop_2   prop_3     prop_4
#> 1          NA        NA       NA         NA
#> 2   48.484848 94.117647 86.36364 77.4774775
#> 3   51.515152  5.882353 13.63636 22.5225225
#> 4          NA        NA       NA         NA
#> 122  6.060606  8.823529  0.00000  4.5045045
#> 26   0.000000  2.941176  0.00000  0.9009009
head(outdata$out_all$Total$prop)
#>        prop_1    prop_2    prop_3    prop_4
#> 1          NA        NA        NA        NA
#> 2   51.162791 86.904762 83.333333 73.622047
#> 3   48.837209 13.095238 16.666667 26.377953
#> 4          NA        NA        NA        NA
#> 122  6.976744  8.333333  4.761905  6.692913
#> 25   1.162791  0.000000  2.380952  1.181102
```

- `diff`: risk difference compared with the `reference_group` within
  each `subgroup_var`.

``` r
head(outdata$out_all$Total$diff)
#>         diff_2     diff_3
#> 1           NA         NA
#> 2    35.741971  32.170543
#> 3   -35.741971 -32.170543
#> 4           NA         NA
#> 122   1.356589  -2.214839
#> 25   -1.162791   1.218162
```

### Format output

After we have the raw analysis results, we can use
[`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md)
to prepare the outdata to create RTF tables.

``` r
tbl <- outdata |> format_ae_specific_subgroup()
head(tbl$tbl)
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2 Fn_3
#> 96                    Participants in population   53    <NA>   50    <NA>   40
#> 135 with one or more drug-related adverse events   28  (52.8)   41  (82.0)   32
#> 134          with no drug-related adverse events   25  (47.2)    9  (18.0)    8
#> 1                                                  NA    <NA>   NA    <NA>   NA
#> 33                             Cardiac disorders    4   (7.5)    4   (8.0)    4
#> 22                           Atrial fibrillation    1   (1.9)    0   (0.0)    2
#>     Fprop_3 Mn_1 Mprop_1 Mn_2 Mprop_2 Mn_3 Mprop_3 Totaln_1 Totalprop_1
#> 96     <NA>   33    <NA>   34    <NA>   44    <NA>       86        <NA>
#> 135  (80.0)   16  (48.5)   32  (94.1)   38  (86.4)       44      (51.2)
#> 134  (20.0)   17  (51.5)    2   (5.9)    6  (13.6)       42      (48.8)
#> 1      <NA>   NA    <NA>   NA    <NA>   NA    <NA>       NA        <NA>
#> 33   (10.0)    2   (6.1)    3   (8.8)    0   (0.0)        6       (7.0)
#> 22    (5.0)    0   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)
#>     Totaln_2 Totalprop_2 Totaln_3 Totalprop_3 order
#> 96        84        <NA>       84        <NA>     1
#> 135       73      (86.9)       70      (83.3)   100
#> 134       11      (13.1)       14      (16.7)   200
#> 1         NA        <NA>       NA        <NA>   900
#> 33         7       (8.3)        4       (4.8) 10000
#> 22         0       (0.0)        2       (2.4) 10021
```

We can hide the total column:

``` r
tbl <- outdata |> format_ae_specific_subgroup(display = c("n", "prop"))
head(tbl$tbl)
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2 Fn_3
#> 96                    Participants in population   53    <NA>   50    <NA>   40
#> 135 with one or more drug-related adverse events   28  (52.8)   41  (82.0)   32
#> 134          with no drug-related adverse events   25  (47.2)    9  (18.0)    8
#> 1                                                  NA    <NA>   NA    <NA>   NA
#> 33                             Cardiac disorders    4   (7.5)    4   (8.0)    4
#> 22                           Atrial fibrillation    1   (1.9)    0   (0.0)    2
#>     Fprop_3 Mn_1 Mprop_1 Mn_2 Mprop_2 Mn_3 Mprop_3 Totaln_1 Totalprop_1
#> 96     <NA>   33    <NA>   34    <NA>   44    <NA>       86        <NA>
#> 135  (80.0)   16  (48.5)   32  (94.1)   38  (86.4)       44      (51.2)
#> 134  (20.0)   17  (51.5)    2   (5.9)    6  (13.6)       42      (48.8)
#> 1      <NA>   NA    <NA>   NA    <NA>   NA    <NA>       NA        <NA>
#> 33   (10.0)    2   (6.1)    3   (8.8)    0   (0.0)        6       (7.0)
#> 22    (5.0)    0   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)
#>     Totaln_2 Totalprop_2 Totaln_3 Totalprop_3 order
#> 96        84        <NA>       84        <NA>     1
#> 135       73      (86.9)       70      (83.3)   100
#> 134       11      (13.1)       14      (16.7)   200
#> 1         NA        <NA>       NA        <NA>   900
#> 33         7       (8.3)        4       (4.8) 10000
#> 22         0       (0.0)        2       (2.4) 10021
```

Adding risk difference:

``` r
tbl <- outdata |> format_ae_specific_subgroup(display = c("n", "prop", "diff"))
head(tbl$tbl)
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2 Fn_3
#> 96                    Participants in population   53    <NA>   50    <NA>   40
#> 135 with one or more drug-related adverse events   28  (52.8)   41  (82.0)   32
#> 134          with no drug-related adverse events   25  (47.2)    9  (18.0)    8
#> 1                                                  NA    <NA>   NA    <NA>   NA
#> 33                             Cardiac disorders    4   (7.5)    4   (8.0)    4
#> 22                           Atrial fibrillation    1   (1.9)    0   (0.0)    2
#>     Fprop_3 Fdiff_2 Fdiff_3 Mn_1 Mprop_1 Mn_2 Mprop_2 Mn_3 Mprop_3 Mdiff_2
#> 96     <NA>    <NA>    <NA>   33    <NA>   34    <NA>   44    <NA>    <NA>
#> 135  (80.0)    29.2    27.2   16  (48.5)   32  (94.1)   38  (86.4)    45.6
#> 134  (20.0)   -29.2   -27.2   17  (51.5)    2   (5.9)    6  (13.6)   -45.6
#> 1      <NA>    <NA>    <NA>   NA    <NA>   NA    <NA>   NA    <NA>    <NA>
#> 33   (10.0)     0.5     2.5    2   (6.1)    3   (8.8)    0   (0.0)     2.8
#> 22    (5.0)    -1.9     3.1    0   (0.0)    0   (0.0)    0   (0.0)     0.0
#>     Mdiff_3 Totaln_1 Totalprop_1 Totaln_2 Totalprop_2 Totaln_3 Totalprop_3
#> 96     <NA>       86        <NA>       84        <NA>       84        <NA>
#> 135    37.9       44      (51.2)       73      (86.9)       70      (83.3)
#> 134   -37.9       42      (48.8)       11      (13.1)       14      (16.7)
#> 1      <NA>       NA        <NA>       NA        <NA>       NA        <NA>
#> 33     -6.1        6       (7.0)        7       (8.3)        4       (4.8)
#> 22      0.0        1       (1.2)        0       (0.0)        2       (2.4)
#>     Totaldiff_2 Totaldiff_3 order
#> 96         <NA>        <NA>     1
#> 135        35.7        32.2   100
#> 134       -35.7       -32.2   200
#> 1          <NA>        <NA>   900
#> 33          1.4        -2.2 10000
#> 22         -1.2         1.2 10021
```

### Mock data preparation

We can also use
[`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md)
to create mock output data.

The purpose of the `mock` argument is not to create a comprehensive mock
table template, but a handy way to help users create a mock table that
mimics the exact output layout.

Additional work is required to develop a flexible mock table generation
tool (for example, a dedicated mock table generation package).

``` r
tbl <- outdata |> format_ae_specific_subgroup(mock = FALSE)
head(tbl$tbl)
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2 Fn_3
#> 96                    Participants in population   53    <NA>   50    <NA>   40
#> 135 with one or more drug-related adverse events   28  (52.8)   41  (82.0)   32
#> 134          with no drug-related adverse events   25  (47.2)    9  (18.0)    8
#> 1                                                  NA    <NA>   NA    <NA>   NA
#> 33                             Cardiac disorders    4   (7.5)    4   (8.0)    4
#> 22                           Atrial fibrillation    1   (1.9)    0   (0.0)    2
#>     Fprop_3 Mn_1 Mprop_1 Mn_2 Mprop_2 Mn_3 Mprop_3 Totaln_1 Totalprop_1
#> 96     <NA>   33    <NA>   34    <NA>   44    <NA>       86        <NA>
#> 135  (80.0)   16  (48.5)   32  (94.1)   38  (86.4)       44      (51.2)
#> 134  (20.0)   17  (51.5)    2   (5.9)    6  (13.6)       42      (48.8)
#> 1      <NA>   NA    <NA>   NA    <NA>   NA    <NA>       NA        <NA>
#> 33   (10.0)    2   (6.1)    3   (8.8)    0   (0.0)        6       (7.0)
#> 22    (5.0)    0   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)
#>     Totaln_2 Totalprop_2 Totaln_3 Totalprop_3 order
#> 96        84        <NA>       84        <NA>     1
#> 135       73      (86.9)       70      (83.3)   100
#> 134       11      (13.1)       14      (16.7)   200
#> 1         NA        <NA>       NA        <NA>   900
#> 33         7       (8.3)        4       (4.8) 10000
#> 22         0       (0.0)        2       (2.4) 10021
```

### RTF tables

By using
[`tlf_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific_subgroup.md),
we can transfer the output from
[`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md)
to an RTF or PDF table.

``` r
outdata |>
  format_ae_specific_subgroup() |>
  tlf_ae_specific_subgroup(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    path_outtable = "rtf/ae0specific0sub0gender1.rtf"
  )
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0specific0sub0gender1.rtf
```
