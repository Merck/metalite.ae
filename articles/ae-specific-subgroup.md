# Subgroup Analysis for Specific AE

``` r

library(metalite.ae)
```

## Overview

This vignette demonstrates how to summarize patients with drug-related
AEs by subgroup. The workflow uses three functions:

- [`prepare_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific_subgroup.md)
  prepares the subgroup analysis datasets.
- [`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md)
  formats the results or creates mock output.
- [`tlf_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific_subgroup.md)
  creates the RTF table.

## Build metadata

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
    title = "Participants with Drug-Related Adverse Events"
  ) |>
  metalite::meta_build()
```

## Generate an AE specific subgroup table

[`prepare_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific_subgroup.md)
uses the definitions in `meta` to calculate results for each subgroup.
[`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md)
formats those results, and
[`tlf_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific_subgroup.md)
creates the RTF table.

``` r

rtf_dir <- if (dir.exists("vignettes/rtf")) "vignettes/rtf" else "rtf"
rtf_file <- file.path(rtf_dir, "ae0specific0sub0gender1.rtf")

prepare_ae_specific_subgroup(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  subgroup_var = "SEX",
  subgroup_header = c("TRTA", "SEX"),
  display_subgroup_total = TRUE
) |>
  format_ae_specific_subgroup(display = c("n", "prop", "diff")) |>
  tlf_ae_specific_subgroup(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific",
    path_outtable = rtf_file
  )
#> Warning in foo(text_justification): The input is not a single value, with
#> length equal to number of columns or a matrix with same dimension of the table.
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length
#> [13] is not a sub-multiple or multiple of the number of rows [114]
#> Warning in foo(border_left): The input is not a single value, with length equal
#> to number of columns or a matrix with same dimension of the table.
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length
#> [13] is not a sub-multiple or multiple of the number of rows [114]
#> Warning in matrix(width, nrow = nrow(tbl), ncol = ncol(tbl), byrow = TRUE):
#> data length [13] is not a sub-multiple or multiple of the number of rows [114]
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length
#> [13] is not a sub-multiple or multiple of the number of rows [114]
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0specific0sub0gender1.rtf
```

[Download the generated RTF
table](https://merck.github.io/metalite.ae/articles/rtf/ae0specific0sub0gender1.rtf)
