# Format exposure-adjusted AE summary

Format exposure-adjusted AE summary

## Usage

``` r
format_ae_exp_adj(
  outdata,
  display = c("n", "total_exp", "events", "eaer", "total"),
  digits_total_exp = 2,
  digits_eaer = 2,
  mock = FALSE
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- display:

  A character vector of measurement to be displayed:

  - `n`: Number of subjects exposed.

  - `total_exp`: Total exposure in person-time.

  - `events`: Number of AE.

  - `eaer`: Exposure adjusted event rate.

  - `total`: Total columns.

- digits_total_exp:

  A numeric value of number of digits for total exposure value.

- digits_eaer:

  A numeric value of number of digits for exposure-adjusted event rate.

- mock:

  A boolean value to display mock table.

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

# Calculate and format exposure-adjusted event rates
outdata <- meta |>
  prepare_ae_summary(
    population = "apat",
    observation = "wk12",
    parameter = "any;rel;ser"
  ) |>
  extend_ae_summary_eaer(adj_unit = "month")
#> any
#> rel
#> ser
#> any
#> rel
#> ser

tbl <- outdata |>
  format_ae_exp_adj()
head(tbl$tbl)
#>                              name     Low.Dose     Placebo        Total
#> 1  Number of Participants exposed           84          86          170
#> 2  Total exposure in person-month       273.29      421.20       694.49
#> 3                  adverse events 435 (159.17) 301 (71.46) 736 (105.98)
#> 4 drug-related{^b} adverse events 292 (106.85) 133 (31.58)  425 (61.20)
#> 5          serious adverse events     1 (0.37)    0 (0.00)     1 (0.14)
#>             row_label
#> 1               -----
#> 2               -----
#> 3 Total events (rate)
#> 4 Total events (rate)
#> 5 Total events (rate)
```
