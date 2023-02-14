# metalite.ae

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Merck/metalite.ae/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/metalite.ae?branch=main)
[![R-CMD-check](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml)
[![status](https://tinyverse.netlify.com/badge/metalite.ae)](https://tinyverse.netlify.app/)
<!-- badges: end -->

## Overview

metalite.ae is an R package for standard adverse events analysis, including:

- AE summary.
- Specific AE analysis.

## Workflow

The general workflow is split into three parts.

1. Define metadata information using the metalite package.
1. Prepare outdata using `prepare_*()` functions.
1. Create TLFs using `tlf_*()` functions.

## Highlighted features

- Enables metadata structure.
- Consistent input and output in standard functions.
- Streamlines mock table generation.
