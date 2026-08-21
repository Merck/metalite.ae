# Generate a Static AE-Specific Table in RTF format

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

### Step 2: Generate the static AE specific table

[`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
uses the population, observation, and parameter definitions in `meta` to
calculate the AE-specific analysis results. It returns an `outdata`
object for formatting and reporting.

[`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
converts the analysis results into a production-ready table dataset.

Pass the formatted output to
[`tlf_ae_specific()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific.md)
to create the RTF table.

``` r

rtf_dir <- if (dir.exists("vignettes/rtf")) "vignettes/rtf" else "rtf"
rtf_file <- file.path(rtf_dir, "ae0specific1.rtf")

outdata <- prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  format_ae_specific() |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    path_outtable = rtf_file
  )
```

Generated RTF file: ae0specific1.rtf
