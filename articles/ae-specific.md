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

# Define metadata
adsl <- forestly::forestly_adsl
adae <- forestly::forestly_adae

adsl$TRT01A <- factor(
  adsl$TRT01A,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)
adae$TRTA <- factor(
  adae$TRTA,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)

analysis_plan <- metalite::plan(
  analysis = "ae_specific",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

meta <- metalite::meta_adam(observation = adae, population = adsl) |>
  metalite::define_plan(analysis_plan) |>
  metalite::define_population(
    name = "apat",
    var = c(
      "USUBJID", "SAFFL", "TRT01A", "TRTDUR",
      "SITEID", "SEX", "RACE", "AGE"
    ),
    group = "TRT01A",
    subset = SAFFL == "Y",
    label = "All Participants as Treated"
  ) |>
  metalite::define_observation(
    name = "wk12",
    var = c(
      "USUBJID", "SAFFL", "TRTA", "AEDECOD", "AEBODSYS", "AEREL",
      "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
    ),
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
  ) |>
  metalite::define_parameter(
    name = "rel",
    term1 = "Drug-Related",
    term2 = "",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Drug-related AEs"
  ) |>
  metalite::define_analysis(
    name = "ae_specific",
    title = "Patients with Drug-Related Adverse Events"
  ) |>
  metalite::meta_build()
```

Click to show the output

``` r

meta
#> ADaM metadata: 
#>    .$data_population     Population data with 170 subjects 
#>    .$data_observation    Observation data with 736 records 
#>    .$plan    Analysis plan with 1 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id    group
#> 1 'apat' 'USUBJID' 'TRT01A'
#>                                                      var       subset
#> 1 USUBJID, SAFFL, TRT01A, TRTDUR, SITEID, SEX, RACE, AGE SAFFL == 'Y'
#>                           label
#> 1 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group
#> 1 'wk12' 'USUBJID' 'TRTA'
#>                                                                                         var
#> 1 USUBJID, SAFFL, TRTA, AEDECOD, AEBODSYS, AEREL, AESER, AEOUT, AEACN, AESDTH, ASTDT, AENDT
#>         subset           label
#> 1 SAFFL == 'Y' 'Weeks 0 to 12'
#> 
#> 
#>   Analysis parameter type:
#>    name              label                               subset
#> 1 'rel' 'Drug-related AEs' AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 
#> 
#>   Analysis function:
#>            name                           label
#> 1 'ae_specific' 'Table: specific adverse event'
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
#>  $ n              :'data.frame': 114 obs. of  3 variables:
#>  $ order          : num [1:114] 1 100 200 900 1000 ...
#>  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group: num 2
#>  $ prop           :'data.frame': 114 obs. of  3 variables:
#>  $ diff           :'data.frame': 114 obs. of  1 variable:
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:114] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:114] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language prepare_ae_specific(meta = meta, population = "apat", observation = "wk12",      parameter = "rel")
```

The resulting dataset contains frequently used statistics, with
variables indexed according to the order specified in `outdata$group`.

``` r

outdata$group
#> [1] "Low Dose" "Placebo"  "Total"
```

The row is indexed according to the order of `outdata$name`.

``` r

head(data.frame(outdata$order, outdata$name))
#>   outdata.order                                 outdata.name
#> 1             1                   Participants in population
#> 2           100 with one or more drug-related adverse events
#> 3           200          with no drug-related adverse events
#> 4           900                                             
#> 5          1000                            Cardiac disorders
#> 6          1018                          Atrial fibrillation
```

- `n_pop`: number of participants in population.

``` r

outdata$n_pop
#>   n_1 n_2 n_3
#> 1  84  86 170
```

- `n`: number of subjects with AE.

``` r

head(outdata$n)
#>    n_1 n_2 n_3
#> 1   84  86 170
#> 2   73  44 117
#> 3   11  42  53
#> 4   NA  NA  NA
#> 98   7   6  13
#> 22   0   1   1
```

- `prop`: proportion of subjects with AE.

``` r

head(outdata$prop)
#>       prop_1    prop_2     prop_3
#> 1         NA        NA         NA
#> 2  86.904762 51.162791 68.8235294
#> 3  13.095238 48.837209 31.1764706
#> 4         NA        NA         NA
#> 98  8.333333  6.976744  7.6470588
#> 22  0.000000  1.162791  0.5882353
```

- `diff`: risk difference compared with the `reference_group`.

``` r

head(outdata$diff)
#>        diff_1
#> 1          NA
#> 2   35.741971
#> 3  -35.741971
#> 4          NA
#> 98   1.356589
#> 22  -1.162791
```

## Format output

Once the raw analysis results are obtained, the
[`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
function can be employed to prepare the outdata, ensuring its
compatibility with production-ready RTF tables.

``` r

tbl <- outdata |> format_ae_specific()
head(tbl$tbl)
#>                                            name n_1 prop_1 n_2 prop_2 n_3
#> 1                    Participants in population  84   <NA>  86   <NA> 170
#> 2  with one or more drug-related adverse events  73 (86.9)  44 (51.2) 117
#> 3           with no drug-related adverse events  11 (13.1)  42 (48.8)  53
#> 4                                                NA   <NA>  NA   <NA>  NA
#> 98                            Cardiac disorders   7  (8.3)   6  (7.0)  13
#> 22                          Atrial fibrillation   0  (0.0)   1  (1.2)   1
#>    prop_3
#> 1    <NA>
#> 2  (68.8)
#> 3  (31.2)
#> 4    <NA>
#> 98  (7.6)
#> 22  (0.6)
```

### Additional statistics

By using the `display` argument, we can choose specific statistics to
include. For instance, we have the option to incorporate the risk
difference.

``` r

tbl <- outdata |> format_ae_specific(display = c("n", "prop", "diff"))
head(tbl$tbl)
#>                                            name n_1 prop_1 n_2 prop_2
#> 1                    Participants in population  84   <NA>  86   <NA>
#> 2  with one or more drug-related adverse events  73 (86.9)  44 (51.2)
#> 3           with no drug-related adverse events  11 (13.1)  42 (48.8)
#> 4                                                NA   <NA>  NA   <NA>
#> 98                            Cardiac disorders   7  (8.3)   6  (7.0)
#> 22                          Atrial fibrillation   0  (0.0)   1  (1.2)
#>    between_tbl
#> 1         <NA>
#> 2         35.7
#> 3        -35.7
#> 4         <NA>
#> 98         1.4
#> 22        -1.2
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
#>                                            name n_1 prop_1 n_2 prop_2 diff_1
#> 1                    Participants in population  84   <NA>  86   <NA>   <NA>
#> 2  with one or more drug-related adverse events  73 (86.9)  44 (51.2)   35.7
#> 3           with no drug-related adverse events  11 (13.1)  42 (48.8)  -35.7
#> 4                                                NA   <NA>  NA   <NA>   <NA>
#> 98                            Cardiac disorders   7  (8.3)   6  (7.0)    1.4
#> 22                          Atrial fibrillation   0  (0.0)   1  (1.2)   -1.2
#>              ci_1
#> 1    (-4.4,  4.3)
#> 2    (22.4, 48.0)
#> 3  (-48.0, -22.4)
#> 4            <NA>
#> 98   (-7.3, 10.2)
#> 22   (-6.3,  3.3)
```

We can use
[`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md)
to add average duration of AE.

``` r

tbl <- outdata |>
  extend_ae_specific_duration(duration_var = "ADURN") |>
  format_ae_specific(display = c("n", "prop", "dur"))

head(tbl$tbl)
#>                                            name n_1 prop_1        dur_1 n_2
#> 1                    Participants in population  84   <NA>         <NA>  86
#> 2  with one or more drug-related adverse events  73 (86.9)  27.2 ( 3.2)  44
#> 3           with no drug-related adverse events  11 (13.1)         <NA>  42
#> 4                                                NA   <NA>         <NA>  NA
#> 98                            Cardiac disorders   7  (8.3)  16.1 ( 3.5)   6
#> 22                          Atrial fibrillation   0  (0.0)         <NA>   1
#>    prop_2        dur_2
#> 1    <NA>         <NA>
#> 2  (51.2)  29.0 ( 3.5)
#> 3  (48.8)         <NA>
#> 4    <NA>         <NA>
#> 98  (7.0)  27.1 ( 5.9)
#> 22  (1.2)          6.0
```

We can use
[`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md)
to add number of AE and/or average of it per subject.

``` r

tbl <- outdata |>
  extend_ae_specific_events() |>
  format_ae_specific(display = c("n", "prop", "events_count", "events_avg"))

head(tbl$tbl)
#>                                            name n_1 prop_1  eventsavg_1
#> 1                    Participants in population  84   <NA>         <NA>
#> 2  with one or more drug-related adverse events  73 (86.9)   2.5 ( 0.3)
#> 3           with no drug-related adverse events  11 (13.1)         <NA>
#> 4                                                NA   <NA>         <NA>
#> 98                            Cardiac disorders   7  (8.3)   1.9 ( 0.4)
#> 22                          Atrial fibrillation   0  (0.0)         <NA>
#>    eventscount_1 n_2 prop_2  eventsavg_2 eventscount_2
#> 1             NA  86   <NA>         <NA>            NA
#> 2            292  44 (51.2)   1.1 ( 0.2)           133
#> 3             NA  42 (48.8)         <NA>            NA
#> 4             NA  NA   <NA>         <NA>            NA
#> 98            13   6  (7.0)   2.3 ( 0.6)            14
#> 22             0   1  (1.2)          1.0             1
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
#> 1                     Participants in population  84   <NA>         <NA>
#> 2   with one or more drug-related adverse events  73 (86.9)   2.5 ( 0.3)
#> 3            with no drug-related adverse events  11 (13.1)         <NA>
#> 4                                                 NA   <NA>         <NA>
#> 98                             Cardiac disorders   7  (8.3)   1.9 ( 0.4)
#> 102                   Gastrointestinal disorders   8  (9.5)   1.9 ( 0.4)
#>     eventscount_1 n_2 prop_2  eventsavg_2 eventscount_2
#> 1              NA  86   <NA>         <NA>            NA
#> 2             292  44 (51.2)   1.1 ( 0.2)           133
#> 3              NA  42 (48.8)         <NA>            NA
#> 4              NA  NA   <NA>         <NA>            NA
#> 98             13   6  (7.0)   2.3 ( 0.6)            14
#> 102            15   4  (4.7)   1.8 ( 0.5)             7
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
#>                                            name n_1 prop_1  eventsavg_1
#> 1                    Participants in population  84   <NA>         <NA>
#> 2  with one or more drug-related adverse events  73 (86.9)   2.5 ( 0.3)
#> 3           with no drug-related adverse events  11 (13.1)         <NA>
#> 4                                                NA   <NA>         <NA>
#> 98                            Cardiac disorders   7  (8.3)   1.9 ( 0.4)
#> 63                        Myocardial infarction   1  (1.2)          2.0
#>    eventscount_1 n_2 prop_2  eventsavg_2 eventscount_2
#> 1             NA  86   <NA>         <NA>            NA
#> 2            292  44 (51.2)   1.1 ( 0.2)           133
#> 3             NA  42 (48.8)         <NA>            NA
#> 4             NA  NA   <NA>         <NA>            NA
#> 98            13   6  (7.0)   2.3 ( 0.6)            14
#> 63             2   2  (2.3)   1.0 ( 0.0)             2
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
#> 1                   Participants in population   xx   <NA>   xx   <NA>  xxx
#> 2 with one or more drug-related adverse events   xx (xx.x)   xx (xx.x)  xxx
#> 3          with no drug-related adverse events   xx (xx.x)   xx (xx.x)   xx
#> 4                                              <NA>   <NA> <NA>   <NA> <NA>
#> 5                            Cardiac disorders    x  (x.x)    x  (x.x)   xx
#> 6                          Atrial fibrillation    x  (x.x)    x  (x.x)    x
#>   prop_3
#> 1   <NA>
#> 2 (xx.x)
#> 3 (xx.x)
#> 4   <NA>
#> 5  (x.x)
#> 6  (x.x)
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
    col_rel_width = c(6, rep(1, 6)),
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
