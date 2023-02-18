# metalite.ae <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Merck/metalite.ae/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/metalite.ae?branch=main)
[![R-CMD-check](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml)
[![status](https://tinyverse.netlify.com/badge/metalite.ae)](https://tinyverse.netlify.app/)
<!-- badges: end -->

## Overview

metalite.ae is an R package to analyze adverse events (AE) in clinical trials including:

- AE summary.
- Specific AE analysis.
- AE listing.

The R package is created using 'metalite' data structure that provide end-to-end software development lifecycle (SDLC) solution 
including define, develop, validate and finalize the analysis with full traceability. 

## Workflow

The general workflow split into three parts.

1. Define metadata information using the metalite package.
1. Prepare outdata using `prepare_*()` functions.
1. Create TLFs using `tlf_*()` functions.

## Highlighted features

- Avoid duplicated input by using metadata structure.
  + e.g. define analysis population one time to use in all adverse events analysis.
- Consistent input and output in standard functions.
- Streamlines mock table generation.

## Examples 
