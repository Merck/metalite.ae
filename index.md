# metalite.ae

## Installation

The easiest way to get metalite.ae is to install from CRAN:

``` r
install.packages("metalite.ae")
```

Alternatively, to use a new feature or get a bug fix, you can install
the development version of metalite.ae from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("Merck/metalite.ae")
```

## Overview

metalite.ae is an R package designed for the analysis of adverse events
(AE) in clinical trials. It operates on ADaM datasets and adheres to the
metalite structure. The R package streamlines the process of generating
production-ready tables, listings, and figures as outlined in the [AE
summary chapter](https://r4csr.org/tlf-ae-summary.html) and the
[specific AE chapter](https://r4csr.org/tlf-ae-specific.html) of the *R
for Clinical Study Reports and Submission* book. The package encompasses
the following components:

#### AE summary

![](https://merck.github.io/metalite.ae/articles/fig/ae0summary.png)

#### Specific AE analysis

![](https://merck.github.io/metalite.ae/articles/fig/ae0specific.png)

#### AE listing

![](https://merck.github.io/metalite.ae/articles/fig/ae0listing.png)

## Highlighted features

- Avoid duplicated input by using metadata structure.
  - For example, define analysis population once to use in all adverse
    events analysis.
- Consistent input and output in standard functions.
- Streamlines mock table generation.

## Example

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
    path_outtable = "ae0summary.rtf" # Define output
  )
```

- [Additional
  tutorials](https://merck.github.io/metalite.ae/articles/metalite-ae.html)
