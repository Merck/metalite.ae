# AE Summary

``` r

library(metalite.ae)
```

## Overview

The objective of this tutorial is to generate a production-ready AE
summary. It extends examples shown in the [AE summary
chapter](https://r4csr.org/tlf-ae-summary.html) of the *R for Clinical
Study Reports and Submission* book.

The AE summary analysis entails the creation of tables that summarize
adverse events information. To accomplish this using metalite.ae, three
essential functions are required:

- [`prepare_ae_summary()`](https://merck.github.io/metalite.ae/reference/prepare_ae_summary.md):
  prepare analysis raw datasets.
- [`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md):
  prepare analysis (mock) outdata with proper format.
- [`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md):
  transfer (mock) output dataset to RTF files.

There is one optional function to extend AE summary analysis:

- [`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md):
  add risk difference inference results based on M&N method.

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
  analysis = "ae_summary",
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
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
    name = "any",
    term1 = "",
    term2 = "",
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "All AEs"
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
  metalite::define_parameter(
    name = "ser",
    term1 = "Serious",
    term2 = "",
    subset = AESER == "Y",
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Serious AEs"
  ) |>
  metalite::define_analysis(
    name = "ae_summary",
    title = "Adverse Event Summary"
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
#> 1 'any'          'All AEs'                                     
#> 2 'rel' 'Drug-related AEs' AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 3 'ser'      'Serious AEs'                         AESER == 'Y'
#> 
#> 
#>   Analysis function:
#>           name                          label
#> 1 'ae_summary' 'Table: adverse event summary'
```

## Analysis preparation

The function
[`prepare_ae_summary()`](https://merck.github.io/metalite.ae/reference/prepare_ae_summary.md)
is used to create a dataset for AE summary analysis by utilizing
predefined keywords specified in the example data `meta`.

The resulting output of the function is an outdata object, which
comprises a collection of raw datasets for analysis and reporting.

``` r

outdata <- prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
)
```

``` r

outdata
#> List of 13
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "any;rel;ser"
#>  $ n              :'data.frame': 5 obs. of  3 variables:
#>  $ order          : num [1:5] 1 100 200 300 400
#>  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group: num 2
#>  $ prop           :'data.frame': 5 obs. of  3 variables:
#>  $ diff           : num [1:5, 1] NA 11.43 35.74 1.19 -11.43
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:5] "Participants in population" "with one or more adverse events" "with no adverse events" "with drug-related{^a} adverse events" ...
#>  $ prepare_call   : language prepare_ae_summary(meta = meta, population = "apat", observation = "wk12",      parameter = "any;rel;ser")
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
#>   outdata.order                         outdata.name
#> 1             1           Participants in population
#> 2           100      with one or more adverse events
#> 3           200               with no adverse events
#> 4           300 with drug-related{^a} adverse events
#> 5           400          with serious adverse events
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
#> 2   77  69 146
#> 3    7  17  24
#> 21  73  44 117
#> 22   1   0   1
```

- `prop`: proportion of subjects with AE.

``` r

head(outdata$prop)
#>       prop_1   prop_2     prop_3
#> 1         NA       NA         NA
#> 2  91.666667 80.23256 85.8823529
#> 3   8.333333 19.76744 14.1176471
#> 21 86.904762 51.16279 68.8235294
#> 22  1.190476  0.00000  0.5882353
```

- `diff`: risk difference compared with the `reference_group`.

``` r

head(outdata$diff)
#>                   [,1]
#> pop_diff            NA
#>              11.434109
#>              35.741971
#>               1.190476
#> noevnt_diff -11.434109
```

## Format output

Once the raw analysis results are obtained, the
[`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
function can be employed to prepare the outdata, ensuring its
compatibility with production-ready RTF tables.

``` r

tbl <- outdata |> format_ae_summary()
tbl$tbl
#>                                    name n_1 prop_1 n_2 prop_2 n_3 prop_3
#> 1            Participants in population  84   <NA>  86   <NA> 170   <NA>
#> 2       with one or more adverse events  77 (91.7)  69 (80.2) 146 (85.9)
#> 3                with no adverse events   7  (8.3)  17 (19.8)  24 (14.1)
#> 21 with drug-related{^a} adverse events  73 (86.9)  44 (51.2) 117 (68.8)
#> 22          with serious adverse events   1  (1.2)   0  (0.0)   1  (0.6)
```

### Additional statistics

By using the `display` argument, we can choose specific statistics to
include. For instance, we have the option to incorporate the risk
difference.

``` r

tbl <- outdata |> format_ae_summary(display = c("n", "prop", "diff"))
tbl$tbl
#>                                    name n_1 prop_1 n_2 prop_2 between_tbl
#> 1            Participants in population  84   <NA>  86   <NA>        <NA>
#> 2       with one or more adverse events  77 (91.7)  69 (80.2)        11.4
#> 3                with no adverse events   7  (8.3)  17 (19.8)        35.7
#> 21 with drug-related{^a} adverse events  73 (86.9)  44 (51.2)         1.2
#> 22          with serious adverse events   1  (1.2)   0  (0.0)       -11.4
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
  format_ae_summary(display = c("n", "prop", "diff", "diff_ci"))

tbl$tbl
#>                                    name n_1 prop_1 n_2 prop_2
#> 1            Participants in population  84   <NA>  86   <NA>
#> 2       with one or more adverse events  77 (91.7)  69 (80.2)
#> 3                with no adverse events   7  (8.3)  17 (19.8)
#> 21 with drug-related{^a} adverse events  73 (86.9)  44 (51.2)
#> 22          with serious adverse events   1  (1.2)   0  (0.0)
#>    structure.c.NA....11.4.....35.7......1.2.....11.4....dim...c.5L..
#> 1                                                               <NA>
#> 2                                                               11.4
#> 3                                                               35.7
#> 21                                                               1.2
#> 22                                                             -11.4
#>             ci_1
#> 1   (-4.4,  4.3)
#> 2   ( 1.0, 22.2)
#> 3  (-22.2, -1.0)
#> 21  (22.4, 48.0)
#> 22  (-3.1,  6.5)
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

tbl <- outdata |> format_ae_summary(mock = TRUE)
tbl$tbl
#>                                   name n_1 prop_1 n_2 prop_2 n_3 prop_3
#> 1           Participants in population  xx   <NA>  xx   <NA> xxx   <NA>
#> 2      with one or more adverse events  xx (xx.x)  xx (xx.x) xxx (xx.x)
#> 3               with no adverse events   x  (x.x)  xx (xx.x)  xx (xx.x)
#> 4 with drug-related{^a} adverse events  xx (xx.x)  xx (xx.x) xxx (xx.x)
#> 5          with serious adverse events   x  (x.x)   x  (x.x)   x  (x.x)
```

## RTF tables

The last step is to prepare the RTF table using
[`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md).

``` r

outdata |>
  format_ae_summary() |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary", # Provide analysis type defined in meta$analysis
    path_outtable = "rtf/ae0summary1.rtf"
  )
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0summary1.rtf
```

The
[`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md)
function also provides some commonly used argument to customize the
table.

``` r

outdata |>
  format_ae_summary() |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary", # Provide analysis type defined in meta$analysis
    col_rel_width = c(6, rep(1, 6)),
    text_font_size = 8,
    orientation = "landscape",
    path_outtable = "rtf/ae0summary2.rtf"
  )
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0summary2.rtf
```

The empty table can be generated if there is not result to display.

The mock table can also be generated.

``` r

outdata |>
  format_ae_summary(mock = TRUE) |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary", # Provide analysis type defined in meta$analysis
    path_outtable = "rtf/mock_ae0summary1.rtf"
  )
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/mock_ae0summary1.rtf
```
