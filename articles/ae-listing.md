# AE Listing

``` r

library(metalite.ae)
```

## Overview

The objective of this tutorial is to generate a production-ready adverse
events (AE) listing.

The AE listing offers comprehensive information on the desired adverse
events. There are two essential functions for constructing AE listing
tables with metalite.ae:

- [`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md):
  prepare AE listing datasets.
- [`tlf_ae_listing()`](https://merck.github.io/metalite.ae/reference/tlf_ae_listing.md):
  transfer output datasets to RTF files.

An example output:

## Example data

Within metalite.ae, we utilized the ADSL and ADAE datasets from the
metalite package to create an illustrative dataset. The metadata
structure remains consistent across all analysis examples within
metalite.ae. Additional information can be accessed on the [metalite
package
website](https://merck.github.io/metalite/articles/metalite.html).

``` r

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
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "ser"
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
    name = "ae_listing",
    var_name = c(
      "USUBJID", "ASTDY", "AEDECOD", "ADURN",
      "AESEV", "AESER", "AEREL", "AEOUT"
    ),
    group_by = c("USUBJID", "ASTDY"),
    page_by = "TRTA"
  ) |>
  metalite::meta_build()
#> Warning in
#> metalite::define_parameter(metalite::define_observation(metalite::define_population(metalite::define_plan(metalite::meta_adam(observation
#> = adae, : any is not in .$plan
#> Warning in
#> metalite::define_parameter(metalite::define_parameter(metalite::define_observation(metalite::define_population(metalite::define_plan(metalite::meta_adam(observation
#> = adae, : rel is not in .$plan
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
#>           name                    label
#> 1 'ae_listing' 'Listing: adverse event'
```

## Analysis preparation

The function
[`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md)
is used to create a dataset for AE listing by utilizing predefined
keywords specified in the example data `meta`.

The resulting output of the function is an `outdata` object, which
comprises a collection of raw datasets for analysis and reporting.

``` r

tbl <- prepare_ae_listing(
  meta,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "ser"
)
```

``` r

head(tbl$tbl)
#>          USUBJID ASTDY AEDECOD ADURN  AESEV AESER    AEREL              AEOUT
#> 1131 01-718-1170    27 SYNCOPE     2 SEVERE     Y PROBABLE RECOVERED/RESOLVED
#>          TRTA
#> 1131 Low Dose
```

``` r

head(tbl$col_name)
#>                       USUBJID                         ASTDY 
#>   "Unique Subject Identifier" "Analysis Start Relative Day" 
#>                       AEDECOD                         ADURN 
#>     "Dictionary-Derived Term"             "AE Duration (N)" 
#>                         AESEV                         AESER 
#>          "Severity/Intensity"               "Serious Event"
```

## RTF tables

The last step is to prepare the RTF table using
[`tlf_ae_listing()`](https://merck.github.io/metalite.ae/reference/tlf_ae_listing.md).

``` r

footnote <- c(
  "Related: Investigator-assessed relationship of the adverse event to study medication. Y = RELATED, N = NOT RELATED",
  "Action Taken: Discontinued = DRUG WITHDRAWN, Interrupted = DRUG INTERRUPTED, Reduced = DOSE REDUCED, Increased = DOSE INCREASED, None = DOSE NOT CHANGED, N/A = NOT APPLICABLE.",
  "Outcome: Resolved = RECOVERED/RESOLVED, Resolving = RECOVERING/RESOLVING, Sequelae = RECOVERED/RESOLVED WITH SEQUELAE, Not resolved = NOT RECOVERED/NOT RESOLVED.",
  "Adverse event terms are from MedDRA Version 25.0."
)
```

``` r

tbl |> tlf_ae_listing(
  footnotes = footnote,
  orientation = "portrait",
  source = "Source:  [CDISCpilot: adam-adsl; adae]",
  analysis = "ae_listing", # Provide analysis type defined in meta$analysis
  path_outtable = "rtf/ae0listing0ser0wk12.rtf",
  path_outdata = NULL
)
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0listing0ser0wk12.rtf
```
