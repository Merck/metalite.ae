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

# Define metadata
 adsl <- forestly::forestly_adsl
 adae <- forestly::forestly_adae

 adsl$TRT01A <- factor(
   adsl$TRT01A,
   levels = c("Xanomeline Low Dose", "Placebo"),
   labels = c("Low Dose", "Placebo")
 )
 adae$TRTA <- factor(
   adae$TRTA,
   levels = c("Xanomeline Low Dose", "Placebo"),
   labels = c("Low Dose", "Placebo")
 )

 analysis_plan <- metalite::plan(
   analysis = "ae_summary",
   population = "apat",
   observation = "wk12",
   parameter = "any;rel;ser"
 )

 meta <- metalite::meta_adam(observation = adae, population = adsl) |>
   metalite::define_plan(analysis_plan) |>
   metalite::define_population(
     name = "apat",
     var = c(
       "USUBJID", "SAFFL", "TRT01A", "TRTDUR",
       "SITEID", "SEX", "RACE", "AGE"
     ),
     group = "TRT01A",
     subset = SAFFL == "Y",
     label = "All Participants as Treated"
   ) |>
   metalite::define_observation(
     name = "wk12",
     var = c(
       "USUBJID", "SAFFL", "TRTA", "AEDECOD", "AEBODSYS", "AEREL",
       "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
     ),
     group = "TRTA",
     subset = SAFFL == "Y",
     label = "Weeks 0 to 12"
   ) |>
   metalite::define_parameter(
     name = "any",
     term1 = "",
     term2 = "",
     var = "AEDECOD",
     soc = "AEBODSYS",
     label = "All AEs"
   ) |>
   metalite::define_parameter(
     name = "rel",
     term1 = "Drug-Related",
     term2 = "",
     subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
     var = "AEDECOD",
     soc = "AEBODSYS",
     label = "Drug-related AEs"
   ) |>
   metalite::define_parameter(
     name = "ser",
     term1 = "Serious",
     term2 = "",
     subset = AESER == "Y",
     var = "AEDECOD",
     soc = "AEBODSYS",
     label = "Serious AEs"
   ) |>
   metalite::define_analysis(
     name = "ae_summary",
     title = "Adverse Event Summary"
   ) |>
   metalite::meta_build()
 
meta |> # Example AE data created using metalite
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
