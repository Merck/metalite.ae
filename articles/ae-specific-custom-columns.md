# Customize Columns in an AE Specific Table

``` r

library(metalite.ae)
```

## Overview

This vignette demonstrates how to generate a static AE-specific table
reporting patients with **drug-related adverse events** by treatment
group.

The `display` argument of
[`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
controls which statistics appear in an AE specific table. This vignette
demonstrates how to add risk difference inference, adverse event
duration, and event frequency statistics.

## Define metadata

The example uses ADSL and ADAE data from the
[forestly](https://merck.github.io/forestly/) package. The metadata
follows the same approach used in the [AE Specific
Table](https://merck.github.io/metalite.ae/articles/ae-specific.md)
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

## Select columns

Use `display` to select statistics and set their order. Available
options are:

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

The `"diff_ci"` and `"diff_p"` values require
[`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md).
The `"dur"` value requires
[`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md),
and the event statistics require
[`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md).

### Add a column for risk difference inference

The following example adds a 95% confidence interval and p-value based
on the Miettinen and Nurminen method. See the [rate comparison
vignette](https://merck.github.io/metalite.ae/articles/rate-compare.md)
for methodological details.

``` r

prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  extend_ae_specific_inference() |>
  format_ae_specific(
    display = c("n", "prop", "diff", "diff_ci", "diff_p")
  ) |>
  tlf_ae_specific(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific",
    meddra_version = "24.0",
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/Rtmp0HJ64s/file1d3231caf3a1.rtf
```

### Add a column for average event duration

Use
[`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md)
to calculate the average duration of adverse events. The `duration_var`
argument identifies the analysis variable that contains event duration.

``` r

prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  extend_ae_specific_duration(duration_var = "ADURN") |>
  format_ae_specific(display = c("n", "prop", "dur")) |>
  tlf_ae_specific(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific",
    meddra_version = "24.0",
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/Rtmp0HJ64s/file1d32b0b8375.rtf
```

### Add a column for event frequency

Use
[`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md)
to add the event count and the average number of events per participant.

``` r

prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  extend_ae_specific_events() |>
  format_ae_specific(
    display = c("n", "prop", "events_count", "events_avg")
  ) |>
  tlf_ae_specific(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific",
    meddra_version = "24.0",
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/Rtmp0HJ64s/file1d3212473612.rtf
```
