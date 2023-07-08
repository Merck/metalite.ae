# metalite.ae <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Merck/metalite.ae/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Merck/metalite.ae?branch=main)
[![R-CMD-check](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Merck/metalite.ae/actions/workflows/R-CMD-check.yaml)
[![status](https://tinyverse.netlify.com/badge/metalite.ae)](https://tinyverse.netlify.app/)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/metalite.ae)](https://cran.r-project.org/package=metalite.ae)
<!-- badges: end -->

## Overview

metalite.ae is an R package designed for the analysis of adverse events (AE) in clinical trials. 
It operates on ADaM datasets and adheres to the metalite structure. 
The package encompasses the following components:

<details>
<summary>AE summary.</summary>
<img src="https://merck.github.io/metalite.ae/articles/fig/ae0summary.png" width="100%">
</details>
<details>
<summary>
Specific AE analysis.</summary>
<img src="https://merck.github.io/metalite.ae/articles/fig/ae0specific.png" width="100%">
</details>
<details>
<summary>AE listing.</summary>
<img src="https://merck.github.io/metalite.ae/articles/fig/ae0listing.png" width="100%">
</details>

The R package streamlines the process of generating production-ready tables, listings, and figures
as outlined in the [AE summary chapter](https://r4csr.org/tlf-ae-summary.html) and the
[specific AE chapter](https://r4csr.org/tlf-ae-specific.html) of the
_R for Clinical Study Reports and Submission_ book. 
It ensures complete traceability throughout the development lifecycle, leveraging the metalite data structure.

This R package offers a comprehensive software development lifecycle (SDLC) solution, 
encompassing activities such as definition, development, validation, and finalization of the analysis.

## Highlighted features

- Avoid duplicated input by using metadata structure.
  - For example, define analysis population once to use in all adverse events analysis.
- Consistent input and output in standard functions.
- Streamlines mock table generation.

## Example 

```
meta_ae_example() |> # Example AE data created using metalite
  prepare_ae_summary(
    population = "apat", # Select population by keywords
    observation = "wk12", # Select observation by keywords
    parameter = "any;rel;ser" # Select AE terms by keywords
  ) |>
  format_ae_summary() |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]", # Define data source
    path_outtable = "ae0summary.rtf" # Define output
  )
```

- [Additional tutorials](https://merck.github.io/metalite.ae/articles/metalite-ae.html)

