# Introduction to metalite.ae

## Overview

metalite.ae is an R package designed for the analysis of adverse events
(AE) in clinical trials. It operates on ADaM datasets and adheres to the
metalite structure. The package encompasses the following components:

AE summary.

![](https://merck.github.io/metalite.ae/articles/fig/ae0summary.png)

Specific AE analysis.

![](https://merck.github.io/metalite.ae/articles/fig/ae0specific.png)

AE listing.

![](https://merck.github.io/metalite.ae/articles/fig/ae0listing.png)

The R package streamlines the process of generating production-ready
tables, listings, and figures as outlined in the [AE summary
chapter](https://r4csr.org/tlf-ae-summary.html) and the [specific AE
chapter](https://r4csr.org/tlf-ae-specific.html) of the *R for Clinical
Study Reports and Submission* book. It ensures complete traceability
throughout the development lifecycle, leveraging the metalite data
structure.

This R package offers a comprehensive software development lifecycle
(SDLC) solution, encompassing activities such as definition,
development, validation, and finalization of the analysis.

## Highlighted features

- Avoid duplicated input by using metadata structure.
  - For example, define analysis population once to use in all adverse
    events analysis.
- Consistent input and output in standard functions.
- Streamlines mock table generation.

## Workflow

The overall workflow includes the following steps:

1.  Define metadata information using metalite R package.
2.  Prepare outdata using `prepare_*()` functions.
3.  Extend outdata using `extend_*()` functions (optional).
4.  Format outdata using `format_*()` functions.
5.  Create TLFs using `tlf_*()` functions.

For instance, we can illustrate the creation of a straightforward AE
summary table as shown below.

``` r
meta_ae_example() |> # Example AE data created using metalite
  prepare_ae_summary(
    population = "apat", # Select population by keywords
    observation = "wk12", # Select observation by keywords
    parameter = "any;rel;ser" # Select AE terms by keywords
  ) |>
  format_ae_summary() |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]", # Define data source
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis
    path_outtable = "ae0summary.rtf" # Define output
  )
```

Additional examples and tutorials can be found on the [package
website](https://merck.github.io/metalite.ae/articles/), offering
further guidance and illustrations.

## Input

To implement the workflow in metalite.ae, it is necessary to establish a
metadata structure using the metalite R package. For detailed
instructions, please consult the [metalite
tutorial](https://merck.github.io/metalite/articles/metalite.html) and
refer to the source code of the function
[`meta_ae_example()`](https://github.com/Merck/metalite.ae/blob/main/R/meta_ae_example.R).
