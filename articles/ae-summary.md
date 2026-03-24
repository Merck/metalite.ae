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

The resulting dataset contains frequently used statistics, with
variables indexed according to the order specified in `outdata$group`.

``` r
outdata$group
#> [1] "Placebo"   "Low Dose"  "High Dose" "Total"
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
#>   n_1 n_2 n_3 n_4
#> 1  86  84  84 254
```

- `n`: number of subjects with AE.

``` r
head(outdata$n)
#>    n_1 n_2 n_3 n_4
#> 1   86  84  84 254
#> 2   69  77  79 225
#> 3   17   7   5  29
#> 21  44  73  70 187
#> 22   0   1   2   3
```

- `prop`: proportion of subjects with AE.

``` r
head(outdata$prop)
#>      prop_1    prop_2    prop_3    prop_4
#> 1        NA        NA        NA        NA
#> 2  80.23256 91.666667 94.047619 88.582677
#> 3  19.76744  8.333333  5.952381 11.417323
#> 21 51.16279 86.904762 83.333333 73.622047
#> 22  0.00000  1.190476  2.380952  1.181102
```

- `diff`: risk difference compared with the `reference_group`.

``` r
head(outdata$diff)
#>        diff_2     diff_3
#> 1          NA         NA
#> 2   11.434109  13.815061
#> 21  35.741971  32.170543
#> 22   1.190476   2.380952
#> 3  -11.434109 -13.815061
```

## Format output

Once the raw analysis results are obtained, the
[`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
function can be employed to prepare the outdata, ensuring its
compatibility with production-ready RTF tables.

``` r
tbl <- outdata |> format_ae_summary()
tbl$tbl
#>                                    name n_1 prop_1 n_2 prop_2 n_3 prop_3 n_4
#> 1            Participants in population  86   <NA>  84   <NA>  84   <NA> 254
#> 2       with one or more adverse events  69 (80.2)  77 (91.7)  79 (94.0) 225
#> 3                with no adverse events  17 (19.8)   7  (8.3)   5  (6.0)  29
#> 21 with drug-related{^a} adverse events  44 (51.2)  73 (86.9)  70 (83.3) 187
#> 22          with serious adverse events   0  (0.0)   1  (1.2)   2  (2.4)   3
#>    prop_4
#> 1    <NA>
#> 2  (88.6)
#> 3  (11.4)
#> 21 (73.6)
#> 22  (1.2)
```

### Additional statistics

By using the `display` argument, we can choose specific statistics to
include. For instance, we have the option to incorporate the risk
difference.

``` r
tbl <- outdata |> format_ae_summary(display = c("n", "prop", "diff"))
tbl$tbl
#>                                    name n_1 prop_1 n_2 prop_2 n_3 prop_3 diff_2
#> 1            Participants in population  86   <NA>  84   <NA>  84   <NA>   <NA>
#> 2       with one or more adverse events  69 (80.2)  77 (91.7)  79 (94.0)   11.4
#> 3                with no adverse events  17 (19.8)   7  (8.3)   5  (6.0)   35.7
#> 21 with drug-related{^a} adverse events  44 (51.2)  73 (86.9)  70 (83.3)    1.2
#> 22          with serious adverse events   0  (0.0)   1  (1.2)   2  (2.4)  -11.4
#>    diff_3
#> 1    <NA>
#> 2    13.8
#> 3    32.2
#> 21    2.4
#> 22  -13.8
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
#>                                    name n_1 prop_1 n_2 prop_2 n_3 prop_3 diff_2
#> 1            Participants in population  86   <NA>  84   <NA>  84   <NA>   <NA>
#> 2       with one or more adverse events  69 (80.2)  77 (91.7)  79 (94.0)   11.4
#> 3                with no adverse events  17 (19.8)   7  (8.3)   5  (6.0)   35.7
#> 21 with drug-related{^a} adverse events  44 (51.2)  73 (86.9)  70 (83.3)    1.2
#> 22          with serious adverse events   0  (0.0)   1  (1.2)   2  (2.4)  -11.4
#>             ci_2 diff_3          ci_3
#> 1   (-4.4,  4.3)   <NA>  (-4.4,  4.3)
#> 2   ( 1.0, 22.2)   13.8  ( 4.0, 24.3)
#> 3  (-22.2, -1.0)   32.2 (-24.3, -4.0)
#> 21  (22.4, 48.0)    2.4  (18.4, 44.8)
#> 22  (-3.1,  6.5)  -13.8  (-2.0,  8.3)
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
#>                                   name n_1 prop_1 n_2 prop_2 n_3 prop_3 n_4
#> 1           Participants in population  xx   <NA>  xx   <NA>  xx   <NA> xxx
#> 2      with one or more adverse events  xx (xx.x)  xx (xx.x)  xx (xx.x) xxx
#> 3               with no adverse events  xx (xx.x)   x  (x.x)   x  (x.x)  xx
#> 4 with drug-related{^a} adverse events  xx (xx.x)  xx (xx.x)  xx (xx.x) xxx
#> 5          with serious adverse events   x  (x.x)   x  (x.x)   x  (x.x)   x
#>   prop_4
#> 1   <NA>
#> 2 (xx.x)
#> 3 (xx.x)
#> 4 (xx.x)
#> 5  (x.x)
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
    col_rel_width = c(6, rep(1, 8)),
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
