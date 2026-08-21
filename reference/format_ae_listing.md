# Format AE listing analysis

Format AE listing analysis

## Usage

``` r
format_ae_listing(outdata, mock = FALSE)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md).

- mock:

  A boolean value to display mock table.

## Value

An `outdata` object (a structured list) for AE listing. Key elements
include:

- `meta`: metadata used for analysis.

- `population`, `observation`, `parameter`: selected analysis terms.

- `col_name`: named vector of display labels for listing columns.

- `tbl`: formatted listing data frame used by
  [`tlf_ae_listing()`](https://merck.github.io/metalite.ae/reference/tlf_ae_listing.md).

- `prepare_call` and `extend_call`: recorded function calls for
  reproducibility.

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

outdata <- prepare_ae_listing(
  meta,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "ser"
)
tbl <- outdata |>
  format_ae_listing()
head(tbl$tbl)
#>          USUBJID ASTDY AEDECOD ADURN  AESEV AESER    AEREL              AEOUT
#> 1131 01-718-1170    27 SYNCOPE     2 SEVERE     Y PROBABLE RECOVERED/RESOLVED
#>          TRTA
#> 1131 Low Dose
```
