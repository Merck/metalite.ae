# Generate an Interactive AE Listing Table with reactable

``` r

library(metalite.ae)
```

## Overview

This vignette demonstrates how to generate an interactive adverse event
(AE) listing focused on drug-related AEs using `metalite.ae`.

The listing presents participant-level details for drug-related AEs.
Three functions support the workflow:

- [`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md)
  prepares the listing dataset.
- [`format_ae_listing()`](https://merck.github.io/metalite.ae/reference/format_ae_listing.md)
  organizes the listing output.
- [`react_ae_listing()`](https://merck.github.io/metalite.ae/reference/react_ae_listing.md)
  creates an interactive listing table.

In the interactive table, each column has its own filter box so users
can quickly search for participants or events of interest.

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
    term1 = "Related",
    term2 = "",
    subset = AREL == "RELATED",
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Related AEs"
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

## Step 2: Generate an interactive AE listing table

[`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md)
uses the population, observation, parameter, and analysis definitions in
`meta` to prepare the listing dataset.
[`format_ae_listing()`](https://merck.github.io/metalite.ae/reference/format_ae_listing.md)
organizes the table, and
[`react_ae_listing()`](https://merck.github.io/metalite.ae/reference/react_ae_listing.md)
creates an interactive view.

``` r

prepare_ae_listing(
  meta,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  format_ae_listing() |>
  react_ae_listing(
    default_page_size = 15,
    patient_folding = FALSE
  )
```

Use `patient_folding = TRUE` when reviewers should only see records for
a specific participant after entering the exact full ID in the
first-column search box. This mode is useful for privacy-conscious
review workflows, focused medical review, or meetings where you want to
avoid showing all participants by default. In this setting, the table
starts empty and displays records only when a full, valid patient ID is
provided.

``` r

prepare_ae_listing(
  meta,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  format_ae_listing() |>
  react_ae_listing(
    default_page_size = 15,
    patient_folding = TRUE
  )
```
