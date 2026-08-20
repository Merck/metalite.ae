# metalite.ae 0.1.4

## New features

- The new `gt_ae_specific()` function is developed to output AE specific tables in
   gt format (#235, thanks to @LittleBeannie).
- The new `gt_ae_summary()` function renders formatted adverse event summaries
    as gt tables, including titles, treatment-group spanners, footnotes, and
    source notes (#215, thanks to @ginnaram13).
- `rate_compare()` and `rate_compare_sum()` now accept a `bisection` argument to
    control the number of sections used by the bisection method (#221, thanks to
    @lm3388).
- The `analysis` argument is now required by all `tlf_*()` functions so that
    table titles are generated from the selected analysis metadata (#207, thanks
    to @lm3388).
- `meta_ae_example()` is no longer exported. Package examples now construct
    metadata directly from the example datasets in forestly (#229, thanks to
    @LittleBeannie).
- Added interactive AE listing workflow support with new
    `format_ae_listing()` and `react_ae_listing()` functions for structured
    listing formatting and reactable-based rendering (#239, thanks to
    @LittleBeannie).

## Bug fixes

- `format_ae_summary()` now uses parameter labels when custom parameters do not
    define `summ_row`, preventing row-name and summary-data length mismatches
    (#224, thanks to @LittleBeannie).
- `gt_ae_summary()` now supports missing source notes and footnotes without
    passing `NULL` to `gt::md()` (#226, thanks to @LittleBeannie).
- `prepare_ae_specific()` now preserves SOC and preferred-term row ordering
    when the data exceed the previous fixed ordering placeholder (#214, thanks to
    @ginnaram13).

## Improvements

- The `metalite_ae_adexsum` example dataset has been expanded for adverse event
    forest plot workflows (#220, thanks to @lm3388).
- Unused arguments and R CMD check warnings in `gt_ae_summary()` and
    `prepare_ae_specific()` have been removed (#232, thanks to @fukuhiro2023).
- Package examples now use direct, reproducible metadata definitions based on
    forestly example data (#229, thanks to @LittleBeannie and @fukuhiro2023).

## Documentation

- The adverse event vignettes and pkgdown site have been reorganized, with new
    articles for custom columns, filtering and sorting, and mock tables (#204,
    #231, thanks to @lm3388 and @LittleBeannie).
- Added a new vignette for interactive AE listing tables and updated listing
    workflow documentation and pkgdown references for the new listing functions
    (#239, thanks to @LittleBeannie).

# metalite.ae 0.1.3

## New features

-   `format_ae_specific()` now has new arguments `filter_method` and `filter_criteria` to control displayed rows in a table, `sort_order` and `sort_column` to sort an output table, and `hide_soc_stats` to control display of statistics for SOC rows (#191, #192, #203thanks to @fukuhiro2023).
-   Add `data/metalite_ae_adesxum.rda` (#189).
-   Add event counts to `avg_count()` and `extend_ae_specific_events()` (#194).
-   Add `empty_table()` to return an empty table object when data has no population (#200).

## Bug fixes

-   Fix bug to correctly display the order of means of duration and means of event count in `extend_ae_specific_events()` and `extend_ae_specific_duration()`(#194, #199).
-   Fix bug to avoid an error when a treatment group has no observation to calculate event counts and duration in `avg_event()` and `avg_duration()` (#199).
-   Fix bug to avoid an error when there is no observation to summarize event counts and duration in `avg_event()`, `avg_duration()`, `extend_ae_specific_events()` and `extend_ae_specific_duration()` (#200).

## Improvements

-   Update `fmt_est()` to change display of missing values from a character string of "NA" to a blank in an output table (#194).
-   Update `extend_ae_specific_inference()` to pass additional arguments to `rate_compare_sum()` (#197).

# metalite.ae 0.1.2

## New features

-   `tlf_ae_summary()` now has a new argument `title` for a user-customized title (#133).
-   Add new functions: `extend_ae_specific_subgroup()`, `prepare_ae_specific_subgroup()`, `format_ae_specific_subgroup()`, `tlf_ae_specific_subgroup()`, `extend_ae_summary_eaer()`, `format_ae_exp_adj()` and `tlf_ae_exp_adj()` (#150, #161, #174, #180).
-   Add `data/metalite_ae_adex.rda` (#164).
-   Add new vignettes: `vignette("ae-specific-subgroup")` and `vignette("exposure-adjusted-event-rate")` (#167, #169).
-   Add System Organ Classes (SOC) information to `outdata` in `prepare_ae_specific()` (#186).

## Bug fixes

-   Fix bug to avoid warning messages when no comparison variables are requested in `format_ae_specific()` (#135).
-   Fix bug to align the row order of output data with the input one in `avg_event()` and `avg_duration()` (#136).
-   Fix bug to check missing grouping variable after a subset of input data in `prepare_ae_specific()` (#137).
-   Fix bug to display item if AE term has `NA` in `prepare_ae_specific()` (#166).
-   Fix bug to correctly assign treatment labels when a treatment variable has `NA` (#179).

## Improvements

-   Update GitHub Actions workflow (#131, #163, #181).
-   Add styler workflow (#134).
-   Add CRAN download badge (#140).
-   Remove dependencies on `dplyr` and `tidyr` from `avg_event()`, `avg_duration()` and `fmt_val()` (#136).
-   Change the default value of the `title` argument in `tlf_ae_specific()` (#138).
-   Remove `R/n_subject.R` and change to use `metalite::n_subject()` in `prepare_ae_specific()` (#144).
-   Race values for adsl and adae are converted to title case in `meta_ae_exmple()` (#158).
-   Organize files for utility functions (#159)
    -   Rename `R/rtf_output.R` to `R/utility.R`.
    -   Remove `rate_compare_sum.R`, `R/to_mock.R` and `R/to_sentence.R`.
    -   Move `to_rate_compare_sum()` to `R/rate_compare.R`, and move `to_mock()` and `to_sentence()` to `R/utility.R`.
-   Remove library calls from tests (#183).
-   Add `R/outdata.R` to control `outdata` object (#161).
-   Add an installation section to `README.md` (#177).
-   Improve style and formatting for vignettes and code (#158, #168).

# metalite.ae 0.1.1

-   Updated the `DESCRIPTION` file to reformat the reference.
-   Uncommented the previously commented code example.

# metalite.ae 0.1.0

-   Initial submission to CRAN.
-   Added a `NEWS.md` file to track changes to the package.
