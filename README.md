# metalite.ae <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Merck/metalite.ae/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/metalite.ae?branch=main)
[![R-CMD-check](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml)
[![status](https://tinyverse.netlify.com/badge/metalite.ae)](https://tinyverse.netlify.app/)
<!-- badges: end -->

## Overview

metalite.ae is an R package to analyze adverse events (AE) in clinical trials including:

<details><summary>AE summary.</summary><img src="https://merck.github.io/metalite.ae/articles/fig/ae0summary.png"></details>
<details><summary>Specific AE analysis.</summary><img src="https://merck.github.io/metalite.ae/articles/fig/ae0specific.png"></details> 
<details><summary>AE listing.</summary><img src="https://merck.github.io/metalite.ae/articles/fig/ae0listing.png"></details>  

The R package simplifies the workflow to create production-ready table, listing and figures discussed in the 
[Chpater 7](https://r4csr.org/aesummary.html) and [Chapter 8](https://r4csr.org/specific-ae.html) of 
the R for Clinical Study Reports and Submission book with full tractability. 

The R package is created using the [metalite](https://merck.github.io/metalite/) data structure 
that provide end-to-end software development lifecycle (SDLC) solution 
including define, develop, validate and finalize the analysis with full .

## Workflow

The general workflow split into:

1. Define metadata information using the metalite package.
1. Prepare outdata using `prepare_*()` functions.
1. (optional) Extend outdata using `extend_*()` functions.
1. Format outdata using `format_*()` functions.
1. Create TLFs using `tlf_*()` functions.

Tutorials with examples are listed in the [package website](https://merck.github.io/metalite.ae/articles/index.html)

## Highlighted features

- Avoid duplicated input by using metadata structure.
  + e.g. define analysis population one time to use in all adverse events analysis.
- Consistent input and output in standard functions.
- Streamlines mock table generation.
