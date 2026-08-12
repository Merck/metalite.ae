# Prepare datasets for AE summary

Prepare datasets for AE summary

## Usage

``` r
prepare_ae_summary(meta, population, observation, parameter, ...)
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

- ...:

  Additional arguments passed to
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

## Value

An `outdata` object containing analysis datasets needed for AE summary.
Key values include:

- `group`: Treatment groups used to index the statistic columns.

- `order`: Numeric values defining the row order.

- `name`: Descriptive row labels corresponding to `order`.

- `n_pop`: Number of participants in the population.

- `n`: Number of participants with an adverse event.

- `prop`: Proportion of participants with an adverse event.

- `diff`: Risk difference compared with the `reference_group`.

## Examples

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

prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
)
#> any
#> rel
#> ser
#> List of 13
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "any;rel;ser"
#>  $ n              :'data.frame': 5 obs. of  3 variables:
#>  $ order          : num [1:5] 1 100 200 300 400
#>  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group: num 2
#>  $ prop           :'data.frame': 5 obs. of  3 variables:
#>  $ diff           : num [1:5, 1] NA 11.43 35.74 1.19 -11.43
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:5] "Participants in population" "with one or more adverse events" "with no adverse events" "with drug-related{^a} adverse events" ...
#>  $ prepare_call   : language prepare_ae_summary(meta = meta, population = "apat", observation = "wk12",      parameter = "any;rel;ser")
```
