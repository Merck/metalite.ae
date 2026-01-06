# Format AE summary analysis

Format AE summary analysis

## Usage

``` r
format_ae_summary(
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
meta <- meta_ae_example()
outdata <- prepare_ae_summary(meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
)
#> any
#> rel
#> ser
tbl <- outdata |>
  format_ae_summary()
head(tbl$tbl)
#>                                    name n_1 prop_1 n_2 prop_2 n_3 prop_3 n_4
#> 1            Participants in population  86   <NA>  84   <NA>  84   <NA> 254
#> 2       with one or more adverse events  69 (80.2)  77 (91.7)  79 (94.0) 225
#> 3                with no adverse events  17 (19.8)   7  (8.3)   5  (6.0)  29
#> 21 with drug-related{^a} adverse events  44 (51.2)  73 (86.9)  70 (83.3) 187
#> 22          with serious adverse events   0  (0.0)   1  (1.2)   2  (2.4)   3
#>    prop_4
#> 1    <NA>
#> 2  (88.6)
#> 3  (11.4)
#> 21 (73.6)
#> 22  (1.2)
```
