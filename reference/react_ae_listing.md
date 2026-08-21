# Interactive AE listing table

Interactive AE listing table

## Usage

``` r
react_ae_listing(
  outdata,
  default_page_size = 15,
  searchable = TRUE,
  striped = TRUE,
  highlight = TRUE,
  patient_folding = FALSE
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`format_ae_listing()`](https://merck.github.io/metalite.ae/reference/format_ae_listing.md).

- default_page_size:

  Number of rows to display per page.

- searchable:

  A boolean value to enable global search. The default is TRUE.

- striped:

  A boolean value to display striped rows. The default is TRUE.

- highlight:

  A boolean value to highlight row on hover. The default is TRUE.

- patient_folding:

  A boolean value to control patient-level folding. The default is
  FALSE. If `TRUE`, all rows are hidden by default and only the first
  column (patient ID column) is filterable; other column filters are
  disabled. In this mode, records are shown only when the entered value
  exactly matches a full patient ID in the first column. If `FALSE`, all
  rows are displayed and filters are available for all columns.

## Value

A `reactable` htmlwidget object.

## Examples

``` r
if (requireNamespace("reactable", quietly = TRUE) &&
  requireNamespace("forestly", quietly = TRUE)) {
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
      subset = AEREL == "RELATED",
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

  meta |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing() |>
    react_ae_listing()
}

{"x":{"tag":{"name":"Reactable","attribs":{"data":{"Unique Subject Identifier":[],"Analysis Start Relative Day":[],"Dictionary-Derived Term":[],"AE Duration (N)":[],"Severity/Intensity":[],"Serious Event":[],"Causality":[],"Outcome of Adverse Event":[],"TRTA":[]},"columns":[{"id":"Unique Subject Identifier","name":"Unique Subject Identifier","type":"character","filterable":true,"minWidth":120},{"id":"Analysis Start Relative Day","name":"Analysis Start Relative Day","type":"numeric","filterable":true,"minWidth":120},{"id":"Dictionary-Derived Term","name":"Dictionary-Derived Term","type":"character","filterable":true,"minWidth":120},{"id":"AE Duration (N)","name":"AE Duration (N)","type":"numeric","filterable":true,"minWidth":120},{"id":"Severity/Intensity","name":"Severity/Intensity","type":"character","filterable":true,"minWidth":120},{"id":"Serious Event","name":"Serious Event","type":"character","filterable":true,"minWidth":120},{"id":"Causality","name":"Causality","type":"character","filterable":true,"minWidth":120},{"id":"Outcome of Adverse Event","name":"Outcome of Adverse Event","type":"character","filterable":true,"minWidth":120},{"id":"TRTA","name":"TRTA","type":"factor","filterable":true,"minWidth":120}],"filterable":true,"searchable":true,"defaultPageSize":15,"showPageSizeOptions":true,"pageSizeOptions":[10,15,25,50],"highlight":true,"bordered":true,"striped":true,"compact":true,"nowrap":true,"dataKey":"3a43ff7184fc278747fa3bc8086833fb"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}
```
