# metalite.ae 0.1.2

## New features
- `tlf_ae_summary()` and `tlf_ae_specific` now have a new argument `title` for a user-customized title.
- Add new functions: `rate_compare_sum()`, `extend_ae_specific_subgroup()`, `prepare_ae_specific_subgroup()`, `format_ae_specific_subgroup()`, `tlf_ae_specific_subgroup()`, `extend_ae_summary_eaer()`, 
`format_ae_exp_adj()` and `tlf_ae_exp_adj()`.
- Add `data/metalite_ae_adex.Rda`.
- Add new vignettes: 'Subgroup Analysis for Specific AE' and 'Exposure Adjusted Event Rate'.
- Add test cases for new functions.
- Add system organ class(SOC) information to `outdata` in `prepare_ae_specific()`

## Bug fixes
- Fix bug to avoid warning messages when no comparison variables are requested in `format_ae_specific()`.
- Fix bug to align the row order of output data with the input one in `avg_event()` and `avg_duration()`.
- Fix bug to display item if AE term has `NA` in `prepare_ae_specific()`.
- Fix bug to correctly assign treatment labels when a treatment variable has `NA`.

## Improvements
- Update Github Actions workflow.
- Add styler workflow.
- Add CRAN download badge.
- Remove dependencies on `dplyr` and `tidyr` from `avg_event()`, `avg_duration()` and `fmt_val()`.
- Remove `R/n_subject.R` and change to use `metalite::n_subject()` in `prepare_ae_specific()`.
- Race values for adsl and adae are converted to title case in `meta_ae_exmple()`.
- Rename `R/rtf_output.R` to `R/utility.R`.
- Remove `R/to_mock.R` and `R/to_sentence.R`, and `to_mock()` and `to_sentence()` are moved to `R/utility.R`.
- Remove library calls from tests.
- Add `R/outdata.R` to control `outdata` object.
- Add an installation section to `README.Rd`.
- Improve style and formatting for vignettes and codes.

# metalite.ae 0.1.1

- Updated the `DESCRIPTION` file to reformat the reference.
- Uncommented the previously commented code example.

# metalite.ae 0.1.0

- Initial submission to CRAN.
- Added a `NEWS.md` file to track changes to the package.
