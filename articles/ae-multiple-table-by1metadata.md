# Generate Multiple AE Tables via 1 Single Metadata

``` r

library(forestly)
library(metalite)
library(metalite.ae)
```

## Overview

This vignette demonstrates how to define one metadata object and reuse
it to generate multiple adverse event (AE) outputs.

In many reporting workflows, summary tables, AE-specific tables,
listings, and visualizations are produced from the same analysis
population and the same core AE definitions. Centralizing those
definitions in one metadata object helps:

- reduce duplicated setup code,
- keep analysis assumptions consistent across outputs, and
- simplify maintenance when specifications change.

The outputs in this vignette include:

- a static AE summary table,
- a static AE-specific table,
- an interactive AE listing table, and
- an interactive AE forest plot.

## Define metadata

The example uses ADSL and ADAE data from the
[forestly](https://merck.github.io/forestly/) package.

``` r

adsl <- forestly_adsl
adae <- forestly_adae

adsl$TRTA <- factor(forestly_adsl$TRT01A,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)
adae$TRTA <- factor(forestly_adae$TRTA,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)
```

## Define multiple analysis plans in one object

Use [`plan()`](https://merck.github.io/metalite/reference/plan.html) and
[`add_plan()`](https://merck.github.io/metalite/reference/add_plan.html)
to define a set of analysis plans in a single pipeline. Each plan
references one analysis type and one parameter set, while sharing the
same population and observation context.

This approach makes it easy to orchestrate multiple downstream outputs
from one metadata object.

``` r

# 1st analysis plan for AE summary
# i.e., overall AE summary including any AEs, drug-related AEs, and serious AEs
plan <- plan(
  analysis = "ae_summary",
  population = "apat",
  observation = "apat",
  parameter = "any;drug-related;serious"
) |>
  # 2nd analysis plan for AE specific
  # i.e., patients with drug-related AEs
  add_plan(
    analysis = "ae_specific",
    population = "apat",
    observation = "apat",
    parameter = "drug-related"
  ) |>
  # 3rd analysis plan for AE listing
  # i.e., listing of patients with serious AEs
  add_plan(
    analysis = "ae_listing",
    population = "apat",
    observation = "apat",
    parameter = "drug-related"
  ) |>
  # 4th analysis plan for AE forest plot
  # i.e., interactive forest plot for drug-related and serious AEs
  add_plan(
    analysis = "ae_forestly",
    population = "apat",
    observation = "apat",
    parameter = "drug-related;serious"
  )
```

## Build metadata once

After defining the plan set, create the full metadata object with
population, observation, parameter, and analysis definitions.

The resulting `meta` object is the single source of truth for all
outputs below.

``` r

meta <- meta_adam(population = adsl, observation = adae) |>
  define_plan(plan) |>
  define_analysis(name = "ae_summary", label = "AE Summary Table") |>
  define_analysis(name = "ae_specific", label = "AE Specific Table") |>
  define_analysis(
    name = "ae_listing", label = "AE Listing Table",
    var_name = c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT"),
    group_by = c("USUBJID", "ASTDY"), page_by = "TRTA"
  ) |>
  define_analysis(name = "ae_forestly", label = "Interactive Forest Plot") |>
  define_population(
    name = "apat", group = "TRTA", id = "USUBJID",
    subset = SAFFL == "Y", label = "All Patient as Treated"
  ) |>
  define_observation(
    name = "apat", group = "TRTA",
    subset = SAFFL == "Y", label = "All Patient as Treated"
  ) |>
  define_parameter(
    name = "any",
    subset = NULL,
    label = "Any AEs",
    var = "AEDECOD", soc = "AEBODSYS",
    term1 = "", term2 = ""
  ) |>
  define_parameter(
    name = "drug-related",
    subset = toupper(AREL) == "RELATED",
    label = "Drug-related AEs",
    var = "AEDECOD", soc = "AEBODSYS",
    term1 = "Drug Related", term2 = ""
  ) |>
  define_parameter(
    name = "serious",
    subset = toupper(AESER) == "Y",
    label = "Serious AEs",
    var = "AEDECOD", soc = "AEBODSYS",
    term1 = "Serious", term2 = ""
  ) |>
  meta_build()
```

## AE output 1: Static AE summary table

Start with the summary output to review high-level AE incidence across
parameters.

``` r

meta |>
  prepare_ae_summary(
    population = "apat",
    observation = "apat",
    parameter = "any;drug-related"
  ) |>
  format_ae_summary() |>
  gt_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary" # Provide analysis type defined in meta$analysis
  )
#> any
#> drug-related
#> [1] "adverse event summary"  "All Patient as Treated" "All Patient as Treated"
```

[TABLE]

## AE output 2: Static AE-specific table

Next, generate a term-level AE-specific table. This output breaks
results down by system organ class and preferred term, which supports
more detailed clinical review.

``` r

meta |>
  prepare_ae_specific(
    population = "apat",
    observation = "apat",
    parameter = "drug-related"
  ) |>
  format_ae_specific() |>
  gt_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific" # Provide analysis type defined in meta$analysis
  )
```

[TABLE]

## AE output 3: Interactive AE listing table

The listing workflow uses
[`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md)
followed by
[`format_ae_listing()`](https://merck.github.io/metalite.ae/reference/format_ae_listing.md)
and
[`react_ae_listing()`](https://merck.github.io/metalite.ae/reference/react_ae_listing.md).

Compared with a static listing, the interactive output supports
on-screen inspection with pagination and a compact review experience.

``` r

meta |>
  prepare_ae_listing(
    population = "apat",
    observation = "apat",
    parameter = "drug-related",
    analysis = "ae_listing" # Provide analysis type defined in meta$analysis
  ) |>
  format_ae_listing() |>
  react_ae_listing(
    default_page_size = 15
  )
```

## AE output 4: Interactive AE forest plot

Finally, create an interactive AE forest plot from the same metadata
object. This visualization helps compare treatment effects across terms
and complements the tabular outputs.

``` r

meta |>
  prepare_ae_forestly() |>
  format_ae_forestly() |>
  ae_forestly()
#> Warning in prepare_ae_forestly(meta): There is no record for the parameter
#> "serious" to display.
```

AE Criteria

Incidence (%) in One or More Treatment Groups

Show/Hide SOC column

## Summary

This workflow shows that one metadata definition can drive multiple AE
report types consistently. In practice, this pattern improves
reproducibility and makes it easier to evolve analysis specifications
without reworking each output independently.
