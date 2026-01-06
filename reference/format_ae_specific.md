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
meta <- meta_ae_example()

outdata <- prepare_ae_specific(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

# Basic example
tbl <- outdata |>
  format_ae_specific()
head(tbl$tbl)
#>                                             name n_1 prop_1 n_2 prop_2 n_3
#> 1                     Participants in population  86   <NA>  84   <NA>  84
#> 2   with one or more drug-related adverse events  44 (51.2)  73 (86.9)  70
#> 3            with no drug-related adverse events  42 (48.8)  11 (13.1)  14
#> 4                                                 NA   <NA>  NA   <NA>  NA
#> 122                            Cardiac disorders   6  (7.0)   7  (8.3)   4
#> 25                           Atrial fibrillation   1  (1.2)   0  (0.0)   2
#>     prop_3 n_4 prop_4
#> 1     <NA> 254   <NA>
#> 2   (83.3) 187 (73.6)
#> 3   (16.7)  67 (26.4)
#> 4     <NA>  NA   <NA>
#> 122  (4.8)  17  (6.7)
#> 25   (2.4)   3  (1.2)

# Filtering
tbl <- outdata |>
  format_ae_specific(
    filter_method = "percent",
    filter_criteria = 10
  )
head(tbl$tbl)
#>                                                     name n_1 prop_1 n_2 prop_2
#> 1                             Participants in population  86   <NA>  84   <NA>
#> 2           with one or more drug-related adverse events  44 (51.2)  73 (86.9)
#> 3                    with no drug-related adverse events  42 (48.8)  11 (13.1)
#> 4                                                         NA   <NA>  NA   <NA>
#> 126                           Gastrointestinal disorders   4  (4.7)   8  (9.5)
#> 127 General disorders and administration site conditions  18 (20.9)  43 (51.2)
#>     n_3 prop_3 n_4 prop_4
#> 1    84   <NA> 254   <NA>
#> 2    70 (83.3) 187 (73.6)
#> 3    14 (16.7)  67 (26.4)
#> 4    NA   <NA>  NA   <NA>
#> 126  10 (11.9)  22  (8.7)
#> 127  35 (41.7)  96 (37.8)

# Display different measurements
tbl <- outdata |>
  extend_ae_specific_events() |>
  format_ae_specific(display = c("n", "prop", "events_count"))
head(tbl$tbl)
#>                                             name n_1 prop_1 eventscount_1 n_2
#> 1                     Participants in population  86   <NA>            NA  84
#> 2   with one or more drug-related adverse events  44 (51.2)           133  73
#> 3            with no drug-related adverse events  42 (48.8)            NA  11
#> 4                                                 NA   <NA>            NA  NA
#> 122                            Cardiac disorders   6  (7.0)            14   7
#> 25                           Atrial fibrillation   1  (1.2)             1   0
#>     prop_2 eventscount_2 n_3 prop_3 eventscount_3
#> 1     <NA>            NA  84   <NA>            NA
#> 2   (86.9)           292  70 (83.3)           279
#> 3   (13.1)            NA  14 (16.7)            NA
#> 4     <NA>            NA  NA   <NA>            NA
#> 122  (8.3)            13   4  (4.8)             5
#> 25   (0.0)             0   2  (2.4)             3
```
