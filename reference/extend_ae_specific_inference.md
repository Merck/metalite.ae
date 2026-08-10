# Add inference information for AE specific analysis

Add inference information for AE specific analysis

## Usage

``` r
extend_ae_specific_inference(outdata, ..., ci = 0.95)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- ...:

  Other options passed on to
  [`rate_compare_sum()`](https://merck.github.io/metalite.ae/reference/rate_compare_sum.md)

- ci:

  A numeric value for the percentile of confidence interval.

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
    subset = AEREL == "RELATED",
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Drug-related AEs"
  ) |>
  metalite::define_analysis(
    name = "ae_specific",
    title = "Participants With {term1} Adverse Events {term2}"
  ) |>
  metalite::meta_build()

# Calculate AE specific analysis and format it
tbl <- prepare_ae_specific(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  extend_ae_specific_inference(eps = 1e-6, bisection = 200) |>
  format_ae_specific(display = c("n", "prop", "diff", "diff_ci"))
head(tbl$tbl)
#>                                           name n_1  prop_1 n_2  prop_2 diff_1
#> 1                   Participants in population  84    <NA>  86    <NA>   <NA>
#> 2 with one or more drug-related adverse events   0   (0.0)   0   (0.0)    0.0
#> 3          with no drug-related adverse events  84 (100.0)  86 (100.0)    0.0
#>           ci_1
#> 1 (-4.4,  4.3)
#> 2         <NA>
#> 3 (-4.4,  4.3)
```
