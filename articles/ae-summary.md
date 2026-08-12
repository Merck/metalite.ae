# Generate a Static AE Summary Table

``` r

library(metalite.ae)
```

## Overview

This vignette demonstrates how to generate a static AE summary table
reporting - The number and percentage of participants with **any AEs**
by treatment group; - The number and percentage of participants with
**drug-related AEs** by treatment group; - The number and percentage of
participants with **serious AEs** by treatment group.

The workflow uses three functions from
[metalite.ae](https://merck.github.io/metalite.ae/):

- [`prepare_ae_summary()`](https://merck.github.io/metalite.ae/reference/prepare_ae_summary.md)
  prepares the analysis datasets.
- [`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
  formats the results or creates mock output.
- [`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md)
  creates the RTF table.

An optional extension adds risk-difference inference:

- [`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md)
  adds confidence intervals and p-values based on the Miettinen and
  Nurminen method.

## Generate an AE summary table

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

### Step 2: Generate the RTF table

[`prepare_ae_summary()`](https://merck.github.io/metalite.ae/reference/prepare_ae_summary.md)
uses the definitions in `meta` to calculate the summary results.
[`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
formats those results, and
[`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md)
creates the RTF table.

``` r

prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
) |>
  format_ae_summary() |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary", # Provide analysis type defined in meta$analysis
    path_outtable = tempfile(fileext = ".rtf")
  )
```
