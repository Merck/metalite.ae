# Format AE specific analysis

Format AE specific analysis

## Usage

``` r
format_ae_specific(
  outdata,
  display = c("n", "prop", "total"),
  hide_soc_stats = FALSE,
  digits_prop = 1,
  digits_ci = 1,
  digits_p = 3,
  digits_dur = c(1, 1),
  digits_events = c(1, 1),
  filter_method = c("percent", "count"),
  filter_criteria = 0,
  sort_order = c("alphabetical", "count_des", "count_asc"),
  sort_column = NULL,
  mock = FALSE
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- display:

  A character vector of measurement to be displayed:

  - `n`: Number of subjects with adverse event.

  - `prop`: Proportion of subjects with adverse event.

  - `total`: Total columns.

  - `diff`: Risk difference.

  - `diff_ci`: 95% confidence interval of risk difference using M&N
    method.

  - `diff_p`: p-value of risk difference using M&N method.

  - `dur`: Average of adverse event duration.

  - `events_avg`: Average number of adverse event per subject.

  - `events_count`: Count number of adverse event per subject.

- hide_soc_stats:

  A boolean value to hide stats for SOC rows.

- digits_prop:

  A numeric value of number of digits for proportion value.

- digits_ci:

  A numeric value of number of digits for confidence interval.

- digits_p:

  A numeric value of number of digits for p-value.

- digits_dur:

  A numeric value of number of digits for average duration of adverse
  event.

- digits_events:

  A numeric value of number of digits for average of number of adverse
  events per subject.

- filter_method:

  A character value to specify how to filter rows:

  - `count`: Filtered based on participant count.

  - `percent`: Filtered based percent incidence.

- filter_criteria:

  A numeric value to display rows where at least one therapy group has a
  percent incidence or participant count greater than or equal to the
  specified value. If `filter_method` is `percent`, the value should be
  between 0 and 100. If `filter_method` is `count`, the value should be
  greater than 0.

- sort_order:

  A character value to specify sorting order:

  - `alphabetical`: Sort by alphabetical order.

  - `count_des`: Sort by count in descending order.

  - `count_asc`: Sort by count in ascending order.

- sort_column:

  A character value of `group` in `outdata` used to sort a table with.

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
  analysis = "ae_specific",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
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

outdata <- prepare_ae_specific(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

# Basic example
tbl <- outdata |>
  format_ae_specific()
head(tbl$tbl)
#>                                            name n_1 prop_1 n_2 prop_2 n_3
#> 1                    Participants in population  84   <NA>  86   <NA> 170
#> 2  with one or more drug-related adverse events  73 (86.9)  44 (51.2) 117
#> 3           with no drug-related adverse events  11 (13.1)  42 (48.8)  53
#> 4                                                NA   <NA>  NA   <NA>  NA
#> 98                            Cardiac disorders   7  (8.3)   6  (7.0)  13
#> 22                          Atrial fibrillation   0  (0.0)   1  (1.2)   1
#>    prop_3
#> 1    <NA>
#> 2  (68.8)
#> 3  (31.2)
#> 4    <NA>
#> 98  (7.6)
#> 22  (0.6)

# Filtering
tbl <- outdata |>
  format_ae_specific(
    filter_method = "percent",
    filter_criteria = 10
  )
head(tbl$tbl)
#>                                                     name n_1 prop_1 n_2 prop_2
#> 1                             Participants in population  84   <NA>  86   <NA>
#> 2           with one or more drug-related adverse events  73 (86.9)  44 (51.2)
#> 3                    with no drug-related adverse events  11 (13.1)  42 (48.8)
#> 4                                                         NA   <NA>  NA   <NA>
#> 103 General disorders and administration site conditions  43 (51.2)  18 (20.9)
#> 9                            Application site dermatitis   9 (10.7)   5  (5.8)
#>     n_3 prop_3
#> 1   170   <NA>
#> 2   117 (68.8)
#> 3    53 (31.2)
#> 4    NA   <NA>
#> 103  61 (35.9)
#> 9    14  (8.2)

# Display different measurements
tbl <- outdata |>
  extend_ae_specific_events() |>
  format_ae_specific(display = c("n", "prop", "events_count"))
head(tbl$tbl)
#>                                            name n_1 prop_1 eventscount_1 n_2
#> 1                    Participants in population  84   <NA>            NA  86
#> 2  with one or more drug-related adverse events  73 (86.9)           292  44
#> 3           with no drug-related adverse events  11 (13.1)            NA  42
#> 4                                                NA   <NA>            NA  NA
#> 98                            Cardiac disorders   7  (8.3)            13   6
#> 22                          Atrial fibrillation   0  (0.0)             0   1
#>    prop_2 eventscount_2
#> 1    <NA>            NA
#> 2  (51.2)           133
#> 3  (48.8)            NA
#> 4    <NA>            NA
#> 98  (7.0)            14
#> 22  (1.2)             1
```
