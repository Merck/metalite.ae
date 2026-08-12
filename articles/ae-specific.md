# Generate a Static AE-Specific Table

``` r

library(metalite.ae)
```

## Overview

This vignette demonstrates how to generate a static AE-specific table
reporting patients with **drug-related adverse events** by treatment
group.

The workflow uses three functions from
[metalite.ae](https://merck.github.io/metalite.ae/):

- [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
  prepares the analysis datasets.
- [`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
  formats the results for reporting.
- [`tlf_ae_specific()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific.md)
  creates the RTF table.

Related vignettes explain how to [customize displayed
columns](https://merck.github.io/metalite.ae/articles/ae-specific-custom-columns.md)
and [filter or sort
rows](https://merck.github.io/metalite.ae/articles/ae-specific-filter-sort.md).
This guide also covers basic RTF customization and mock output.

An example output is shown below.

## Generate an AE-specific table

The example uses ADSL and ADAE data from the
[forestly](https://merck.github.io/forestly/) package.

### Step 1: Define metadata

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
    title = "Participants with Drug-Related Adverse Events"
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

[`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
uses the population, observation, and parameter definitions in `meta` to
calculate the AE-specific analysis results. It returns an `outdata`
object for formatting and reporting.

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

The statistic columns follow the treatment-group order in
`outdata$group`.

``` r

outdata$group
#> [1] "Low Dose" "Placebo"  "Total"
```

Rows follow the order defined by `outdata$order` and `outdata$name`.

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

- `n_pop`: number of participants in the analysis population.

``` r

outdata$n_pop
#>   n_1 n_2 n_3
#> 1  84  86 170
```

- `n`: number of participants with an AE.

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

- `prop`: proportion of participants with an AE.

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

[`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
converts the analysis results into a production-ready table dataset.

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

Use `display` to select and order statistics. For example, include
`"diff"` to show the risk difference.

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

Use
[`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md)
to add confidence intervals and p-values for the risk difference based
on the Miettinen and Nurminen (M&N) method. For details, see the [rate
compare
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

Use
[`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md)
to add the mean AE duration.

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

Use
[`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md)
to add the AE count and the mean number of events per participant.

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

Use `filter_method` and `filter_criteria` to retain rows that meet a
minimum incidence threshold in at least one treatment group:

- `filter_method = "count"` applies the threshold to participant counts.
- `filter_method = "percent"` applies the threshold to incidence
  percentages from 0 to 100.
- `filter_criteria` sets the minimum count or percentage to retain.

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

The example retains rows with an incidence of at least 6% in any
treatment group.

Use `sort_order` and `sort_column` to control row order:

- `sort_order = "alphabetical"` sorts rows by label.
- `sort_order = "count_des"` sorts counts in descending order.
- `sort_order = "count_asc"` sorts counts in ascending order.
- `sort_column` selects the treatment group whose counts determine the
  order.

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

Set `mock = TRUE` to create placeholder values while preserving the
planned table structure. The result is a starting point and may require
customization for study-specific requirements.

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

Pass the formatted output to
[`tlf_ae_specific()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific.md)
to create the RTF table.

``` r

outdata |>
  format_ae_specific() |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/RtmpEJLHDu/file1ddc39ca6016.rtf
```

Use arguments such as `col_rel_width`, `text_font_size`, and
`orientation` to customize the table layout.

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
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/RtmpEJLHDu/file1ddc5ef35f52.rtf
```

Mock output can be written to RTF in the same way.

``` r

outdata |>
  format_ae_specific(mock = TRUE) |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/RtmpEJLHDu/file1ddc76c5cca.rtf
```
