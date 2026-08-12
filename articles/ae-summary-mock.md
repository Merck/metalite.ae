# Create a AE Summary Mock-up Table

``` r

library(metalite.ae)
```

This vignette demonstrates how to generate a static AE summary table
reporting - The number and percentage of participants with **any AEs**
by treatment group; - The number and percentage of participants with
**drug-related AEs** by treatment group; - The number and percentage of
participants with **serious AEs** by treatment group.

## Overview

Mock tables help reviewers evaluate a proposed table structure before
final results are available. The `mock` argument of
[`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
replaces the analysis values with placeholder values while preserving
the AE summary layout.

The mock output is intended as a convenient starting point that
resembles the planned table. It is not an all-encompassing mock table
template, so additional customization may be needed for study-specific
requirements.

## Define metadata

This example uses ADSL and ADAE data from the
[forestly](https://merck.github.io/forestly/) package. The metadata
follows the same approach used in the [AE
Summary](https://merck.github.io/metalite.ae/articles/ae-summary.md)
vignette.

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
  analysis = "ae_summary",
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
)

meta <- metalite::meta_adam(observation = adae, population = adsl) |>
  metalite::define_plan(analysis_plan) |>
  metalite::define_population(
    name = "apat",
    var = c("USUBJID", "SAFFL", "TRT01A"),
    group = "TRT01A",
    subset = SAFFL == "Y",
    label = "All Participants as Treated"
  ) |>
  metalite::define_observation(
    name = "wk12",
    var = c(
      "USUBJID", "SAFFL", "TRTA", "AEDECOD", "AEBODSYS", "AEREL",
      "AESER"
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

## Prepare a mock table

First, prepare the AE summary analysis. Passing `mock = TRUE` to
[`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
then creates placeholder values for the formatted table.

The mock table retains the row labels and treatment-group structure
derived from the metadata. This allows the layout to be reviewed without
presenting the calculated analysis values as final results.

``` r

prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
) |>
  format_ae_summary(mock = TRUE) |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary", # Provide analysis type defined in meta$analysis
    path_outtable = tempfile(fileext = ".rtf")
  )
#> any
#> rel
#> ser
#> The output is saved in/tmp/Rtmp2OUxyy/file1e4f3a591a8e.rtf
```
