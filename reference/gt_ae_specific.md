# Specific adverse events table in gt format

Specific adverse events table in gt format

## Usage

``` r
gt_ae_specific(
  outdata,
  meddra_version = NULL,
  source = NULL,
  analysis,
  footnotes = NULL,
  title = c("analysis", "observation", "population")
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- meddra_version:

  A character value of the MedDRA version for this dataset.

- source:

  A character value of the data source.

- analysis:

  One of analysis name existing at `outdata$meta$analysis`

- footnotes:

  A character vector of table footnotes.

- title:

  Term "analysis", "observation"and "population") for collecting title
  from metadata or a character vector of table titles.

## Value

A `gt_tbl` object for an AE specific table.

## Examples

``` r
library(gt)

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
    var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
    group = "TRT01A",
    subset = SAFFL == "Y",
    label = "All Participants as Treated"
  ) |>
  metalite::define_observation(
    name = "wk12",
    var = c(
      "USUBJID", "SAFFL", "TRTA", "SEX", "AEDECOD", "AEBODSYS",
      "AEREL", "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
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
    title = "Participants With Drug-Related Adverse Events"
  ) |>
  metalite::meta_build()

meta |>
  prepare_ae_specific(
    population = "apat",
    observation = "wk12",
    parameter = "rel"
  ) |>
  format_ae_specific() |>
  gt_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific"
  )


  


Participants With Drug-Related Adverse Events

(Incidence > 0 % in One or More Treatment Groups)

Weeks 0 to 12

All Participants as Treated
```
