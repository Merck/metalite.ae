# Prepare datasets for AE specific subgroup analysis

Prepare datasets for AE specific subgroup analysis

## Usage

``` r
prepare_ae_specific_subgroup(
  meta,
  population,
  observation,
  parameter,
  subgroup_var,
  subgroup_header = c(meta$population[[population]]$group, subgroup_var),
  components = c("soc", "par"),
  display_subgroup_total = TRUE
)
```

## Arguments

- meta:

  A metadata object created by metalite.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- subgroup_var:

  A character value of subgroup variable name in observation data saved
  in `meta$data_observation`.

- subgroup_header:

  A character vector for column header hierarchy. The first element will
  be the first level header and the second element will be second level
  header.

- components:

  A character vector of components name.

- display_subgroup_total:

  Logical. Display total column for subgroup analysis or not.

## Value

A list of analysis datasets needed for AE specific subgroup analysis.

## Examples

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
    var = c("USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE"),
    group = "TRTA",
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

outdata <- prepare_ae_specific_subgroup(
  meta, "apat", "wk12", "rel",
  subgroup_var = "SEX"
)
names(outdata$out_all)
#> [1] "F"     "M"     "Total"
```
