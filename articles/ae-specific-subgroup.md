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

# Define metadata
adsl <- forestly::forestly_adsl
adae <- forestly::forestly_adae

adsl$TRTA <- factor(
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
      "USUBJID", "SAFFL", "TRTA", "TRTDUR",
      "SITEID", "SEX", "RACE", "AGE"
    ),
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "All Participants as Treated"
  ) |>
  metalite::define_observation(
    name = "wk12",
    var = c(
      "USUBJID", "SAFFL", "TRTA", "SEX", "AEDECOD", "AEBODSYS", "AEREL",
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
#> [1] "Low Dose" "Placebo" 
#> 
#> $subgroup
#> [1] "f" "m"
#> 
#> $display_subgroup_total
#> [1] TRUE
#> 
#> $meta
#> ADaM metadata: 
#>    .$data_population     Population data with 170 subjects 
#>    .$data_observation    Observation data with 736 records 
#>    .$plan    Analysis plan with 1 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group                                                  var
#> 1 'apat' 'USUBJID' 'TRTA' USUBJID, SAFFL, TRTA, TRTDUR, SITEID, SEX, RACE, AGE
#>         subset                         label
#> 1 SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group
#> 1 'wk12' 'USUBJID' 'TRTA'
#>                                                                                              var
#> 1 USUBJID, SAFFL, TRTA, SEX, AEDECOD, AEBODSYS, AEREL, AESER, AEOUT, AEACN, AESDTH, ASTDT, AENDT
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
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
#> 
#> $out_all$M
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
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
#> 
#> $out_all$Total
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
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
outdata$out_all$M
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
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
outdata$out_all$Total
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
#>  $ prepare_call   : language prepare_ae_specific(meta = meta, population = population, observation = observation,      parameter = parameter, | __truncated__
```

The variable is indexed by the order of `outdata$group` and
`outdata$subgroup` within each `subgroup_var`.

``` r

outdata$group
#> [1] "Low Dose" "Placebo"
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
#> 5                        1000                            Cardiac disorders
#> 6                        1018                          Atrial fibrillation
```

- `n_pop`: participants in population within each `subgroup_var`.

``` r

outdata$out_all$F$n_pop
#>   n_1 n_2 n_3
#> 1  50  53 103
outdata$out_all$M$n_pop
#>   n_1 n_2 n_3
#> 1  34  33  67
outdata$out_all$Total$n_pop
#>   n_1 n_2 n_3
#> 1  84  86 170
```

- `n`: number of subjects with AE within each `subgroup_var`.

``` r

head(outdata$out_all$F$n)
#>    n_1 n_2 n_3
#> 1   50  53 103
#> 2   41  28  69
#> 3    9  25  34
#> 4   NA  NA  NA
#> 98   4   4   8
#> 22   0   1   1
head(outdata$out_all$M$n)
#>    n_1 n_2 n_3
#> 1   34  33  67
#> 2   32  16  48
#> 3    2  17  19
#> 4   NA  NA  NA
#> 98   3   2   5
#> 23   1   0   1
head(outdata$out_all$Total$n)
#>    n_1 n_2 n_3
#> 1   84  86 170
#> 2   73  44 117
#> 3   11  42  53
#> 4   NA  NA  NA
#> 98   7   6  13
#> 22   0   1   1
```

- `prop`: proportion of subjects with AE within each `subgroup_var`.

``` r

head(outdata$out_all$F$prop)
#>    prop_1    prop_2     prop_3
#> 1      NA        NA         NA
#> 2      82 52.830189 66.9902913
#> 3      18 47.169811 33.0097087
#> 4      NA        NA         NA
#> 98      8  7.547170  7.7669903
#> 22      0  1.886792  0.9708738
head(outdata$out_all$M$prop)
#>       prop_1    prop_2    prop_3
#> 1         NA        NA        NA
#> 2  94.117647 48.484848 71.641791
#> 3   5.882353 51.515152 28.358209
#> 4         NA        NA        NA
#> 98  8.823529  6.060606  7.462687
#> 23  2.941176  0.000000  1.492537
head(outdata$out_all$Total$prop)
#>       prop_1    prop_2     prop_3
#> 1         NA        NA         NA
#> 2  86.904762 51.162791 68.8235294
#> 3  13.095238 48.837209 31.1764706
#> 4         NA        NA         NA
#> 98  8.333333  6.976744  7.6470588
#> 22  0.000000  1.162791  0.5882353
```

- `diff`: risk difference compared with the `reference_group` within
  each `subgroup_var`.

``` r

head(outdata$out_all$Total$diff)
#>        diff_1
#> 1          NA
#> 2   35.741971
#> 3  -35.741971
#> 4          NA
#> 98   1.356589
#> 22  -1.162791
```

### Format output

After we have the raw analysis results, we can use
[`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md)
to prepare the outdata to create RTF tables.

``` r

tbl <- outdata |> format_ae_specific_subgroup()
head(tbl$tbl)
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2 Mn_1
#> 78                    Participants in population   50    <NA>   53    <NA>   34
#> 112 with one or more drug-related adverse events   41  (82.0)   28  (52.8)   32
#> 111          with no drug-related adverse events    9  (18.0)   25  (47.2)    2
#> 1                                                  NA    <NA>   NA    <NA>   NA
#> 29                             Cardiac disorders    4   (8.0)    4   (7.5)    3
#> 19                           Atrial fibrillation    0   (0.0)    1   (1.9)    0
#>     Mprop_1 Mn_2 Mprop_2 Totaln_1 Totalprop_1 Totaln_2 Totalprop_2 order
#> 78     <NA>   33    <NA>       84        <NA>       86        <NA>     1
#> 112  (94.1)   16  (48.5)       73      (86.9)       44      (51.2)   100
#> 111   (5.9)   17  (51.5)       11      (13.1)       42      (48.8)   200
#> 1      <NA>   NA    <NA>       NA        <NA>       NA        <NA>   900
#> 29    (8.8)    2   (6.1)        7       (8.3)        6       (7.0)  1000
#> 19    (0.0)    0   (0.0)        0       (0.0)        1       (1.2)  1018
```

We can hide the total column:

``` r

tbl <- outdata |> format_ae_specific_subgroup(display = c("n", "prop"))
head(tbl$tbl)
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2 Mn_1
#> 78                    Participants in population   50    <NA>   53    <NA>   34
#> 112 with one or more drug-related adverse events   41  (82.0)   28  (52.8)   32
#> 111          with no drug-related adverse events    9  (18.0)   25  (47.2)    2
#> 1                                                  NA    <NA>   NA    <NA>   NA
#> 29                             Cardiac disorders    4   (8.0)    4   (7.5)    3
#> 19                           Atrial fibrillation    0   (0.0)    1   (1.9)    0
#>     Mprop_1 Mn_2 Mprop_2 Totaln_1 Totalprop_1 Totaln_2 Totalprop_2 order
#> 78     <NA>   33    <NA>       84        <NA>       86        <NA>     1
#> 112  (94.1)   16  (48.5)       73      (86.9)       44      (51.2)   100
#> 111   (5.9)   17  (51.5)       11      (13.1)       42      (48.8)   200
#> 1      <NA>   NA    <NA>       NA        <NA>       NA        <NA>   900
#> 29    (8.8)    2   (6.1)        7       (8.3)        6       (7.0)  1000
#> 19    (0.0)    0   (0.0)        0       (0.0)        1       (1.2)  1018
```

Adding risk difference:

``` r

tbl <- outdata |> format_ae_specific_subgroup(display = c("n", "prop", "diff"))
head(tbl$tbl)
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2
#> 78                    Participants in population   50    <NA>   53    <NA>
#> 112 with one or more drug-related adverse events   41  (82.0)   28  (52.8)
#> 111          with no drug-related adverse events    9  (18.0)   25  (47.2)
#> 1                                                  NA    <NA>   NA    <NA>
#> 29                             Cardiac disorders    4   (8.0)    4   (7.5)
#> 19                           Atrial fibrillation    0   (0.0)    1   (1.9)
#>     Fbetween_tbl Mn_1 Mprop_1 Mn_2 Mprop_2 Mbetween_tbl Totaln_1 Totalprop_1
#> 78          <NA>   34    <NA>   33    <NA>         <NA>       84        <NA>
#> 112         29.2   32  (94.1)   16  (48.5)         45.6       73      (86.9)
#> 111        -29.2    2   (5.9)   17  (51.5)        -45.6       11      (13.1)
#> 1           <NA>   NA    <NA>   NA    <NA>         <NA>       NA        <NA>
#> 29           0.5    3   (8.8)    2   (6.1)          2.8        7       (8.3)
#> 19          -1.9    0   (0.0)    0   (0.0)          0.0        0       (0.0)
#>     Totaln_2 Totalprop_2 Totalbetween_tbl order
#> 78        86        <NA>             <NA>     1
#> 112       44      (51.2)             35.7   100
#> 111       42      (48.8)            -35.7   200
#> 1         NA        <NA>             <NA>   900
#> 29         6       (7.0)              1.4  1000
#> 19         1       (1.2)             -1.2  1018
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
#>                                             name Fn_1 Fprop_1 Fn_2 Fprop_2 Mn_1
#> 78                    Participants in population   50    <NA>   53    <NA>   34
#> 112 with one or more drug-related adverse events   41  (82.0)   28  (52.8)   32
#> 111          with no drug-related adverse events    9  (18.0)   25  (47.2)    2
#> 1                                                  NA    <NA>   NA    <NA>   NA
#> 29                             Cardiac disorders    4   (8.0)    4   (7.5)    3
#> 19                           Atrial fibrillation    0   (0.0)    1   (1.9)    0
#>     Mprop_1 Mn_2 Mprop_2 Totaln_1 Totalprop_1 Totaln_2 Totalprop_2 order
#> 78     <NA>   33    <NA>       84        <NA>       86        <NA>     1
#> 112  (94.1)   16  (48.5)       73      (86.9)       44      (51.2)   100
#> 111   (5.9)   17  (51.5)       11      (13.1)       42      (48.8)   200
#> 1      <NA>   NA    <NA>       NA        <NA>       NA        <NA>   900
#> 29    (8.8)    2   (6.1)        7       (8.3)        6       (7.0)  1000
#> 19    (0.0)    0   (0.0)        0       (0.0)        1       (1.2)  1018
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
