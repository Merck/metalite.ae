# AE Listing

``` r

library(metalite.ae)
```

## Overview

This vignette demonstrates how to generate a static adverse event (AE)
listing focused on serious AEs.

The listing presents participant-level details for adverse events of
interest. Two functions support the workflow:

- [`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md)
  prepares the listing dataset.
- [`tlf_ae_listing()`](https://merck.github.io/metalite.ae/reference/tlf_ae_listing.md)
  creates the RTF table.

## Step 1: Define metadata

The example uses ADSL and ADAE data from the
[forestly](https://merck.github.io/forestly/) package.

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
```

## Step 2: Generate the AE listing table

[`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md)
uses the population, observation, parameter, and analysis definitions in
`meta` to prepare the listing dataset. The result is passed directly to
[`tlf_ae_listing()`](https://merck.github.io/metalite.ae/reference/tlf_ae_listing.md)
to create the RTF table.

``` r

footnote <- c(
  "Related: Investigator-assessed relationship of the adverse event to study medication. Y = RELATED, N = NOT RELATED",
  "Action Taken: Discontinued = DRUG WITHDRAWN, Interrupted = DRUG INTERRUPTED, Reduced = DOSE REDUCED, Increased = DOSE INCREASED, None = DOSE NOT CHANGED, N/A = NOT APPLICABLE.",
  "Outcome: Resolved = RECOVERED/RESOLVED, Resolving = RECOVERING/RESOLVING, Sequelae = RECOVERED/RESOLVED WITH SEQUELAE, Not resolved = NOT RECOVERED/NOT RESOLVED.",
  "Adverse event terms are from MedDRA Version 25.0."
)

prepare_ae_listing(
  meta,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "ser"
) |>
  tlf_ae_listing(
    footnotes = footnote,
    orientation = "portrait",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_listing", # Provide analysis type defined in meta$analysis
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/RtmpBIby9K/file1cfa1ba48962.rtf
```
