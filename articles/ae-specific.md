# AE Specification

``` r
library(metalite.ae)
```

## Overview

The objective of this tutorial is to generate a production-ready AE
specification analyses. It extends examples shown in the [specific AE
chapter](https://r4csr.org/tlf-ae-specific.html) of the *R for Clinical
Study Reports and Submission* book.

The AE specification analysis entails the creation of tables that
summarize details of different types of adverse events. To accomplish
this using metalite.ae, three essential functions are required:

- [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md):
  prepare analysis raw datasets.
- [`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md):
  prepare analysis (mock) outdata with proper format.
- [`tlf_ae_specific()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific.md):
  transfer (mock) output dataset to RTF table.

There are three optional functions to extend AE specification analysis.

- [`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md):
  add risk difference inference results based on M&N method.
- [`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md):
  add average duration of AE.
- [`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md):
  add average number of AE events.

An example output:

## Example data

Within metalite.ae, we utilized the ADSL and ADAE datasets from the
metalite package to create an illustrative dataset. The metadata
structure remains consistent across all analysis examples within
metalite.ae. Additional information can be accessed on the [metalite
package
website](https://merck.github.io/metalite/articles/metalite.html).

``` r
meta <- meta_ae_example()
```

Click to show the output

``` r
meta
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
```

### Analysis preparation

The function
[`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
is used to create a dataset for AE summary analysis by utilizing
predefined keywords specified in the example data `meta`.

The resulting output of the function is an `outdata` object, which
comprises a collection of raw datasets for analysis and reporting.

``` r
outdata <- prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)
```

``` r
outdata
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
#>  $ prepare_call   : language prepare_ae_specific(meta = meta, population = "apat", observation = "wk12",      parameter = "rel")
```

The resulting dataset contains frequently used statistics, with
variables indexed according to the order specified in `outdata$group`.

``` r
outdata$group
#> [1] "Placebo"   "Low Dose"  "High Dose" "Total"
```

The row is indexed according to the order of `outdata$name`.

``` r
head(data.frame(outdata$order, outdata$name))
#>   outdata.order                                 outdata.name
#> 1             1                   Participants in population
#> 2           100 with one or more drug-related adverse events
#> 3           200          with no drug-related adverse events
#> 4           900                                             
#> 5         10000                            Cardiac disorders
#> 6         10021                          Atrial fibrillation
```

- `n_pop`: number of participants in population.

``` r
outdata$n_pop
#>   n_1 n_2 n_3 n_4
#> 1  86  84  84 254
```

- `n`: number of subjects with AE.

``` r
head(outdata$n)
#>     n_1 n_2 n_3 n_4
#> 1    86  84  84 254
#> 2    44  73  70 187
#> 3    42  11  14  67
#> 4    NA  NA  NA  NA
#> 122   6   7   4  17
#> 25    1   0   2   3
```

- `prop`: proportion of subjects with AE.

``` r
head(outdata$prop)
#>        prop_1    prop_2    prop_3    prop_4
#> 1          NA        NA        NA        NA
#> 2   51.162791 86.904762 83.333333 73.622047
#> 3   48.837209 13.095238 16.666667 26.377953
#> 4          NA        NA        NA        NA
#> 122  6.976744  8.333333  4.761905  6.692913
#> 25   1.162791  0.000000  2.380952  1.181102
```

- `diff`: risk difference compared with the `reference_group`.

``` r
head(outdata$diff)
#>         diff_2     diff_3
#> 1           NA         NA
#> 2    35.741971  32.170543
#> 3   -35.741971 -32.170543
#> 4           NA         NA
#> 122   1.356589  -2.214839
#> 25   -1.162791   1.218162
```

## Format output

Once the raw analysis results are obtained, the
[`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
function can be employed to prepare the outdata, ensuring its
compatibility with production-ready RTF tables.

``` r
tbl <- outdata |> format_ae_specific()
head(tbl$tbl)
#>                                             name n_1 prop_1 n_2 prop_2 n_3
#> 1                     Participants in population  86   <NA>  84   <NA>  84
#> 2   with one or more drug-related adverse events  44 (51.2)  73 (86.9)  70
#> 3            with no drug-related adverse events  42 (48.8)  11 (13.1)  14
#> 4                                                 NA   <NA>  NA   <NA>  NA
#> 122                            Cardiac disorders   6  (7.0)   7  (8.3)   4
#> 25                           Atrial fibrillation   1  (1.2)   0  (0.0)   2
#>     prop_3 n_4 prop_4
#> 1     <NA> 254   <NA>
#> 2   (83.3) 187 (73.6)
#> 3   (16.7)  67 (26.4)
#> 4     <NA>  NA   <NA>
#> 122  (4.8)  17  (6.7)
#> 25   (2.4)   3  (1.2)
```

### Additional statistics

By using the `display` argument, we can choose specific statistics to
include. For instance, we have the option to incorporate the risk
difference.

``` r
tbl <- outdata |> format_ae_specific(display = c("n", "prop", "diff"))
head(tbl$tbl)
#>                                             name n_1 prop_1 n_2 prop_2 n_3
#> 1                     Participants in population  86   <NA>  84   <NA>  84
#> 2   with one or more drug-related adverse events  44 (51.2)  73 (86.9)  70
#> 3            with no drug-related adverse events  42 (48.8)  11 (13.1)  14
#> 4                                                 NA   <NA>  NA   <NA>  NA
#> 122                            Cardiac disorders   6  (7.0)   7  (8.3)   4
#> 25                           Atrial fibrillation   1  (1.2)   0  (0.0)   2
#>     prop_3 diff_2 diff_3
#> 1     <NA>   <NA>   <NA>
#> 2   (83.3)   35.7   32.2
#> 3   (16.7)  -35.7  -32.2
#> 4     <NA>   <NA>   <NA>
#> 122  (4.8)    1.4   -2.2
#> 25   (2.4)   -1.2    1.2
```

To perform advanced analysis, the
[`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md)
function is utilized. For instance, we can incorporate a 95% confidence
interval based on the Miettinen and Nurminen (M&N) method. Further
information regarding the M&N method can be found in the [rate compare
vignette](https://merck.github.io/metalite.ae/articles/rate-compare.html).

``` r
tbl <- outdata |>
  extend_ae_specific_inference() |>
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
#> 1     <NA>   <NA>   (-4.4,  0.0)   <NA>   (-4.4,  0.0)
#> 2   (83.3)   35.7   (22.4, 48.0)   32.2   (18.4, 44.8)
#> 3   (16.7)  -35.7 (-48.0, -22.4)  -32.2 (-44.8, -18.4)
#> 4     <NA>   <NA>           <NA>   <NA>           <NA>
#> 122  (4.8)    1.4   (-7.3, 10.2)   -2.2  (-10.3,  5.6)
#> 25   (2.4)   -1.2   (-6.3,  3.3)    1.2   (-4.2,  7.3)
```

We can use
[`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md)
to add average duration of AE.

``` r
tbl <- outdata |>
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

We can use
[`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md)
to add number of AE and/or average of it per subject.

``` r
tbl <- outdata |>
  extend_ae_specific_events() |>
  format_ae_specific(display = c("n", "prop", "events_count", "events_avg"))

head(tbl$tbl)
#>                                             name n_1 prop_1  eventsavg_1
#> 1                     Participants in population  86   <NA>         <NA>
#> 2   with one or more drug-related adverse events  44 (51.2)   0.7 ( 0.1)
#> 3            with no drug-related adverse events  42 (48.8)         <NA>
#> 4                                                 NA   <NA>         <NA>
#> 122                            Cardiac disorders   6  (7.0)   2.3 ( 0.6)
#> 25                           Atrial fibrillation   1  (1.2)          1.0
#>     eventscount_1 n_2 prop_2  eventsavg_2 eventscount_2 n_3 prop_3  eventsavg_3
#> 1              NA  84   <NA>         <NA>            NA  84   <NA>         <NA>
#> 2             133  73 (86.9)   1.6 ( 0.2)           292  70 (83.3)   1.5 ( 0.2)
#> 3              NA  11 (13.1)         <NA>            NA  14 (16.7)         <NA>
#> 4              NA  NA   <NA>         <NA>            NA  NA   <NA>         <NA>
#> 122            14   7  (8.3)   1.9 ( 0.4)            13   4  (4.8)   1.2 ( 0.2)
#> 25              1   0  (0.0)         <NA>             0   2  (2.4)   1.5 ( 0.5)
#>     eventscount_3
#> 1              NA
#> 2             279
#> 3              NA
#> 4              NA
#> 122             5
#> 25              3
```

We can use `filter_method` and `filter_criteria` parameters to filter
information based on the specified criteria:

- `filter_method`: A character value to specify how to filter rows (by
  `count` or `percent`).
  - `count`: Filter based on participant count.
  - `percent`: Filter based on percent incidence.
- `filter_criteria`: A numeric value to display rows where at least one
  therapy group has:
  - a percent incidence or participant count greater than or equal to
    the specified value.
  - If `filter_method` is `percent`, the value should be between 0 and
    100.
  - If `filter_method` is `count`, the value should be greater than 0.

``` r
tbl <- outdata |>
  extend_ae_specific_events() |>
  format_ae_specific(
    display = c("n", "prop", "events_count", "events_avg"),
    filter_method = "percent",
    filter_criteria = 6
  )

head(tbl$tbl)
#>                                             name n_1 prop_1  eventsavg_1
#> 1                     Participants in population  86   <NA>         <NA>
#> 2   with one or more drug-related adverse events  44 (51.2)   0.7 ( 0.1)
#> 3            with no drug-related adverse events  42 (48.8)         <NA>
#> 4                                                 NA   <NA>         <NA>
#> 122                            Cardiac disorders   6  (7.0)   2.3 ( 0.6)
#> 126                   Gastrointestinal disorders   4  (4.7)   1.8 ( 0.5)
#>     eventscount_1 n_2 prop_2  eventsavg_2 eventscount_2 n_3 prop_3  eventsavg_3
#> 1              NA  84   <NA>         <NA>            NA  84   <NA>         <NA>
#> 2             133  73 (86.9)   1.6 ( 0.2)           292  70 (83.3)   1.5 ( 0.2)
#> 3              NA  11 (13.1)         <NA>            NA  14 (16.7)         <NA>
#> 4              NA  NA   <NA>         <NA>            NA  NA   <NA>         <NA>
#> 122            14   7  (8.3)   1.9 ( 0.4)            13   4  (4.8)   1.2 ( 0.2)
#> 126             7   8  (9.5)   1.9 ( 0.4)            15  10 (11.9)   2.1 ( 0.5)
#>     eventscount_3
#> 1              NA
#> 2             279
#> 3              NA
#> 4              NA
#> 122             5
#> 126            21
```

In results above, rows having any one of “prop_x” values are greater
than 6 get kept in the output.

We can use `sort_order` and `sort_column` parameters to sort results
based on the specified criteria:

- `sort_order` A character value to specify sorting order:
  - `alphabetical`: Sort by alphabetical order.
  - `count_des`: Sort by count in descending order.
  - `count_asc`: Sort by count in ascending order.
- `sort_column A` character value of `group` in `outdata` used to sort a
  table with.

``` r
tbl <- outdata |>
  extend_ae_specific_events() |>
  format_ae_specific(
    display = c("n", "prop", "events_count", "events_avg"),
    sort_order = c("count_des"),
    sort_column = c("Placebo")
  )

head(tbl$tbl)
#>                                             name n_1 prop_1  eventsavg_1
#> 1                     Participants in population  86   <NA>         <NA>
#> 2   with one or more drug-related adverse events  44 (51.2)   0.7 ( 0.1)
#> 3            with no drug-related adverse events  42 (48.8)         <NA>
#> 4                                                 NA   <NA>         <NA>
#> 122                            Cardiac disorders   6  (7.0)   2.3 ( 0.6)
#> 79                         Myocardial infarction   2  (2.3)   1.0 ( 0.0)
#>     eventscount_1 n_2 prop_2  eventsavg_2 eventscount_2 n_3 prop_3  eventsavg_3
#> 1              NA  84   <NA>         <NA>            NA  84   <NA>         <NA>
#> 2             133  73 (86.9)   1.6 ( 0.2)           292  70 (83.3)   1.5 ( 0.2)
#> 3              NA  11 (13.1)         <NA>            NA  14 (16.7)         <NA>
#> 4              NA  NA   <NA>         <NA>            NA  NA   <NA>         <NA>
#> 122            14   7  (8.3)   1.9 ( 0.4)            13   4  (4.8)   1.2 ( 0.2)
#> 79              2   1  (1.2)          2.0             2   1  (1.2)          1.0
#>     eventscount_3
#> 1              NA
#> 2             279
#> 3              NA
#> 4              NA
#> 122             5
#> 79              1
```

### Mock data preparation

The `mock` argument facilitates the creation of a mock table with ease.

Please note that the intention of the `mock` argument is not to provide
an all-encompassing mock table template. Instead, it serves as a
convenient method to assist users in generating a mock table that
closely resembles the desired output layout. To develop a more versatile
mock table generation tool, further efforts are necessary. This could
potentially involve the creation of a dedicated mock table generation
package or similar solutions.

``` r
tbl <- outdata |> format_ae_specific(mock = TRUE)
head(tbl$tbl)
#>                                           name  n_1 prop_1  n_2 prop_2  n_3
#> 1                   Participants in population   xx   <NA>   xx   <NA>   xx
#> 2 with one or more drug-related adverse events   xx (xx.x)   xx (xx.x)   xx
#> 3          with no drug-related adverse events   xx (xx.x)   xx (xx.x)   xx
#> 4                                              <NA>   <NA> <NA>   <NA> <NA>
#> 5                            Cardiac disorders    x  (x.x)    x  (x.x)    x
#> 6                          Atrial fibrillation    x  (x.x)    x  (x.x)    x
#>   prop_3  n_4 prop_4
#> 1   <NA>  xxx   <NA>
#> 2 (xx.x)  xxx (xx.x)
#> 3 (xx.x)   xx (xx.x)
#> 4   <NA> <NA>   <NA>
#> 5  (x.x)   xx  (x.x)
#> 6  (x.x)    x  (x.x)
```

## RTF tables

The last step is to prepare the RTF table using
[`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md).

``` r
outdata |>
  format_ae_specific() |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    path_outtable = "rtf/ae0specific1.rtf"
  )
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0specific1.rtf
```

The
[`tlf_ae_specific()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific.md)
function also provides some commonly used arguments to customize the
table.

``` r
outdata |>
  format_ae_specific() |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    col_rel_width = c(6, rep(1, 8)),
    text_font_size = 8,
    orientation = "landscape",
    path_outtable = "rtf/ae0specific2.rtf"
  )
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0specific2.rtf
```

The mock table can also be generated.

``` r
outdata |>
  format_ae_specific(mock = TRUE) |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    path_outtable = "rtf/mock_ae0specific1.rtf"
  )
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/mock_ae0specific1.rtf
```
