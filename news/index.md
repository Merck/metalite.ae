# Changelog

## metalite.ae 0.1.3

CRAN release: 2024-10-23

### New features

- [`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
  now has new arguments `filter_method` and `filter_criteria` to control
  displayed rows in a table, `sort_order` and `sort_column` to sort an
  output table, and `hide_soc_stats` to control display of statistics
  for SOC rows
  ([\#191](https://github.com/Merck/metalite.ae/issues/191),
  [\#192](https://github.com/Merck/metalite.ae/issues/192),
  [\#203](https://github.com/Merck/metalite.ae/issues/203)).
- Add `data/metalite_ae_adesxum.rda`
  ([\#189](https://github.com/Merck/metalite.ae/issues/189)).
- Add event counts to `avg_count()` and
  [`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md)
  ([\#194](https://github.com/Merck/metalite.ae/issues/194)).
- Add `empty_table()` to return an empty table object when data has no
  population ([\#200](https://github.com/Merck/metalite.ae/issues/200)).

### Bug fixes

- Fix bug to correctly display the order of means of duration and means
  of event count in
  [`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md)
  and
  [`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md)([\#194](https://github.com/Merck/metalite.ae/issues/194),
  [\#199](https://github.com/Merck/metalite.ae/issues/199)).
- Fix bug to avoid an error when a treatment group has no observation to
  calculate event counts and duration in `avg_event()` and
  `avg_duration()`
  ([\#199](https://github.com/Merck/metalite.ae/issues/199)).
- Fix bug to avoid an error when there is no observation to summarize
  event counts and duration in `avg_event()`, `avg_duration()`,
  [`extend_ae_specific_events()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_events.md)
  and
  [`extend_ae_specific_duration()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_duration.md)
  ([\#200](https://github.com/Merck/metalite.ae/issues/200)).

### Improvements

- Update
  [`fmt_est()`](https://merck.github.io/metalite.ae/reference/fmt_est.md)
  to change display of missing values from a character string of “NA” to
  a blank in an output table
  ([\#194](https://github.com/Merck/metalite.ae/issues/194)).
- Update
  [`extend_ae_specific_inference()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_inference.md)
  to pass additional arguments to
  [`rate_compare_sum()`](https://merck.github.io/metalite.ae/reference/rate_compare_sum.md)
  ([\#197](https://github.com/Merck/metalite.ae/issues/197)).

## metalite.ae 0.1.2

CRAN release: 2024-04-16

### New features

- [`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md)
  now has a new argument `title` for a user-customized title
  ([\#133](https://github.com/Merck/metalite.ae/issues/133)).
- Add new functions:
  [`extend_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/extend_ae_specific_subgroup.md),
  [`prepare_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific_subgroup.md),
  [`format_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/format_ae_specific_subgroup.md),
  [`tlf_ae_specific_subgroup()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific_subgroup.md),
  [`extend_ae_summary_eaer()`](https://merck.github.io/metalite.ae/reference/extend_ae_summary_eaer.md),
  [`format_ae_exp_adj()`](https://merck.github.io/metalite.ae/reference/format_ae_exp_adj.md)
  and
  [`tlf_ae_exp_adj()`](https://merck.github.io/metalite.ae/reference/tlf_ae_exp_adj.md)
  ([\#150](https://github.com/Merck/metalite.ae/issues/150),
  [\#161](https://github.com/Merck/metalite.ae/issues/161),
  [\#174](https://github.com/Merck/metalite.ae/issues/174),
  [\#180](https://github.com/Merck/metalite.ae/issues/180)).
- Add `data/metalite_ae_adex.rda`
  ([\#164](https://github.com/Merck/metalite.ae/issues/164)).
- Add new vignettes:
  [`vignette("ae-specific-subgroup")`](https://merck.github.io/metalite.ae/articles/ae-specific-subgroup.md)
  and
  [`vignette("exposure-adjusted-event-rate")`](https://merck.github.io/metalite.ae/articles/exposure-adjusted-event-rate.md)
  ([\#167](https://github.com/Merck/metalite.ae/issues/167),
  [\#169](https://github.com/Merck/metalite.ae/issues/169)).
- Add System Organ Classes (SOC) information to `outdata` in
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
  ([\#186](https://github.com/Merck/metalite.ae/issues/186)).

### Bug fixes

- Fix bug to avoid warning messages when no comparison variables are
  requested in
  [`format_ae_specific()`](https://merck.github.io/metalite.ae/reference/format_ae_specific.md)
  ([\#135](https://github.com/Merck/metalite.ae/issues/135)).
- Fix bug to align the row order of output data with the input one in
  `avg_event()` and `avg_duration()`
  ([\#136](https://github.com/Merck/metalite.ae/issues/136)).
- Fix bug to check missing grouping variable after a subset of input
  data in
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
  ([\#137](https://github.com/Merck/metalite.ae/issues/137)).
- Fix bug to display item if AE term has `NA` in
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
  ([\#166](https://github.com/Merck/metalite.ae/issues/166)).
- Fix bug to correctly assign treatment labels when a treatment variable
  has `NA` ([\#179](https://github.com/Merck/metalite.ae/issues/179)).

### Improvements

- Update GitHub Actions workflow
  ([\#131](https://github.com/Merck/metalite.ae/issues/131),
  [\#163](https://github.com/Merck/metalite.ae/issues/163),
  [\#181](https://github.com/Merck/metalite.ae/issues/181)).
- Add styler workflow
  ([\#134](https://github.com/Merck/metalite.ae/issues/134)).
- Add CRAN download badge
  ([\#140](https://github.com/Merck/metalite.ae/issues/140)).
- Remove dependencies on `dplyr` and `tidyr` from `avg_event()`,
  `avg_duration()` and `fmt_val()`
  ([\#136](https://github.com/Merck/metalite.ae/issues/136)).
- Change the default value of the `title` argument in
  [`tlf_ae_specific()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific.md)
  ([\#138](https://github.com/Merck/metalite.ae/issues/138)).
- Remove `R/n_subject.R` and change to use
  [`metalite::n_subject()`](https://merck.github.io/metalite/reference/n_subject.html)
  in
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
  ([\#144](https://github.com/Merck/metalite.ae/issues/144)).
- Race values for adsl and adae are converted to title case in
  `meta_ae_exmple()`
  ([\#158](https://github.com/Merck/metalite.ae/issues/158)).
- Organize files for utility functions
  ([\#159](https://github.com/Merck/metalite.ae/issues/159))
  - Rename `R/rtf_output.R` to `R/utility.R`.
  - Remove `rate_compare_sum.R`, `R/to_mock.R` and `R/to_sentence.R`.
  - Move `to_rate_compare_sum()` to `R/rate_compare.R`, and move
    `to_mock()` and `to_sentence()` to `R/utility.R`.
- Remove library calls from tests
  ([\#183](https://github.com/Merck/metalite.ae/issues/183)).
- Add `R/outdata.R` to control `outdata` object
  ([\#161](https://github.com/Merck/metalite.ae/issues/161)).
- Add an installation section to `README.md`
  ([\#177](https://github.com/Merck/metalite.ae/issues/177)).
- Improve style and formatting for vignettes and code
  ([\#158](https://github.com/Merck/metalite.ae/issues/158),
  [\#168](https://github.com/Merck/metalite.ae/issues/168)).

## metalite.ae 0.1.1

CRAN release: 2023-02-24

- Updated the `DESCRIPTION` file to reformat the reference.
- Uncommented the previously commented code example.

## metalite.ae 0.1.0

- Initial submission to CRAN.
- Added a `NEWS.md` file to track changes to the package.
