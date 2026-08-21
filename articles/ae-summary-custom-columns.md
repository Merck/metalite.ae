# Customize Columns in an AE Summary Table

``` r

library(metalite.ae)
```

## Overview

The `display` argument of
[`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
selects and orders the statistics in an AE summary table. This vignette
demonstrates how to include risk-difference estimates and inference
results.

## Define metadata

The example uses ADSL and ADAE data from the
[forestly](https://merck.github.io/forestly/) package.

The metadata follows the same approach used in the [AE Summary in RTF
format](https://merck.github.io/metalite.ae/articles/ae-summary-rtf.md)
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

## Select columns

Use
[`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md)
to add confidence intervals and p-values based on the Miettinen and
Nurminen (M&N) method. For details, see the [rate compare
vignette](https://merck.github.io/metalite.ae/articles/rate-compare.html).

After extending the analysis, use `display` in
[`format_ae_summary()`](https://merck.github.io/metalite.ae/reference/format_ae_summary.md)
to select statistics and set their order. Available options are:

- `"n"`: number of participants with an adverse event.
- `"prop"`: proportion of participants with an adverse event.
- `"total"`: total columns.
- `"diff"`: risk difference.
- `"diff_ci"`: 95% confidence interval for the risk difference using the
  Miettinen and Nurminen method.
- `"diff_p"`: p-value for the risk difference using the Miettinen and
  Nurminen method.
- `"dur"`: average adverse event duration.
- `"events_avg"`: average number of adverse events per participant.
- `"events_count"`: number of adverse events per participant.

The `"diff_ci"` and `"diff_p"` statistics are added by
[`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md),
`"dur"` is added by
[`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md),
and the event statistics are added by
[`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md).

For example, include `"diff"` in addition to the number and proportion
of participants with an adverse event:

``` r

rtf_dir <- if (dir.exists("vignettes/rtf")) "vignettes/rtf" else "rtf"
rtf_file <- file.path(rtf_dir, "ae0summary2.rtf")

prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
) |>
  extend_ae_specific_inference() |>
  format_ae_summary(display = c("n", "prop", "diff", "diff_ci")) |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary", # Provide analysis type defined in meta$analysis
    col_rel_width = c(3, rep(1, 6)),
    path_outtable = rtf_file
  )
#> any
#> rel
#> ser
#> Warning in foo(border_top): The input is not a single value, with length equal
#> to number of columns or a matrix with same dimension of the table.
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length [7]
#> is not a sub-multiple or multiple of the number of columns [5]
#> Warning in foo(border_left): The input is not a single value, with length equal
#> to number of columns or a matrix with same dimension of the table.
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length [7]
#> is not a sub-multiple or multiple of the number of columns [5]
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length [5]
#> is not a sub-multiple or multiple of the number of columns [3]
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length [7]
#> is not a sub-multiple or multiple of the number of columns [5]
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length [5]
#> is not a sub-multiple or multiple of the number of columns [3]
#> Warning in matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE): data length [7]
#> is not a sub-multiple or multiple of the number of columns [5]
#> Warning in matrix(width, nrow = nrow(tbl), ncol = ncol(tbl), byrow = TRUE):
#> data length [5] is not a sub-multiple or multiple of the number of columns [3]
#> Warning in matrix(width, nrow = nrow(tbl), ncol = ncol(tbl), byrow = TRUE):
#> data length [7] is not a sub-multiple or multiple of the number of columns [5]
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0summary2.rtf
```

Generated RTF file: ae0summary2.rtf
