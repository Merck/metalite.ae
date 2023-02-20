# metalite.ae <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Merck/metalite.ae/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/metalite.ae?branch=main)
[![R-CMD-check](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml)
[![status](https://tinyverse.netlify.com/badge/metalite.ae)](https://tinyverse.netlify.app/)
<!-- badges: end -->

## Overview

metalite.ae is an R package to analyze adverse events (AE) in clinical trials,
including:

<details>
<summary>AE summary.</summary>
<img src="https://merck.github.io/metalite.ae/articles/fig/ae0summary.png">
</details>
<details>
<summary>Specific AE analysis.</summary>
<img src="https://merck.github.io/metalite.ae/articles/fig/ae0specific.png">
</details>
<details>
<summary>AE listing.</summary>
<img src="https://merck.github.io/metalite.ae/articles/fig/ae0listing.png">
</details>

The R package simplifies the workflow to create production-ready
tables, listings, and figures discussed in the
[AE summary chapter](https://r4csr.org/aesummary.html) and the
[specific AE chapter](https://r4csr.org/specific-ae.html) of the
_R for Clinical Study Reports and Submission_ book with full traceability.

The R package is created using the [metalite](https://merck.github.io/metalite/)
data structure that provides an end-to-end software development lifecycle (SDLC)
solution including defining, developing, validating, and finalizing the analysis.

## Workflow

The general workflow covers:

1. Define metadata information using metalite.
1. Prepare outdata using `prepare_*()` functions.
1. Extend outdata using `extend_*()` functions (optional).
1. Format outdata using `format_*()` functions.
1. Create TLFs using `tlf_*()` functions.

Tutorials with examples are listed on the
[package website](https://merck.github.io/metalite.ae/articles/).

## Highlighted features

- Avoid duplicated input by using metadata structure.
  - For example, define analysis population once to use in all adverse events analysis.
- Consistent input and output in standard functions.
- Streamlines mock table generation.
