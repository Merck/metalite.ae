# Add exposure-adjusted rate information for AE summary analysis

Add exposure-adjusted rate information for AE summary analysis

## Usage

``` r
extend_ae_summary_eaer(
  outdata,
  duration_var = "TRTDUR",
  adj_unit = c("year", "month", "week", "day")
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_summary()`](https://merck.github.io/metalite.ae/reference/prepare_ae_summary.md).

- duration_var:

  A character value of duration variable name. By default, `"TRTDUR"` is
  used.

- adj_unit:

  A character value of exposure adjusted unit. It could be select from
  `"year"`, `"month"`, `"week"`, and `"day"`.

## Value

A list of analysis raw datasets.

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

# Calculate exposure-adjusted event rates
prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
) |>
  extend_ae_summary_eaer()
#> any
#> rel
#> ser
#> any
#> rel
#> ser
#> List of 17
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
#>  $ total_exp      :'data.frame': 1 obs. of  3 variables:
#>  $ event_num      :'data.frame': 3 obs. of  3 variables:
#>  $ eaer           :'data.frame': 3 obs. of  3 variables:
#>  $ adj_unit       : chr "year"
```
