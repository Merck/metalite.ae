# Add Term1 and Term2 Example

## Overview

The objective of this tutorial is to explain how users can add term1 and
term2 parameters in the `metalite.ae` package to customize title and
column labels for adverse event analyses. These parameters allow users
to specify additional descriptive text that appears in generated tables
and reports, providing more detail about the type of adverse events
being analyzed.

Load the metalite.ae package

``` r
library(metalite.ae)
library(metalite)
```

Create the example metadata object containing adverse event data and
analysis definitions  
- Verify that the AEOSI (Adverse Events of Special Interest) variable
exists in the observation dataset  
- Check if AEOSI exists

``` r
meta <- meta_ae_example()
"AEOSI" %in% names(meta$data_observation)
```

**Result:** FALSE → AEOSI variable is not present in the dataset  
(TRUE would indicate that AEOSI exists in the original data).

Since AEOSI is missing in the current ADAE dataset, we need to create it
manually:  
 - Access the observation data from the `meta` object for modification  
 - Define adverse events of special interest (e.g., *DIARRHOEA*,
*NAUSEA*, *VOMITING*, *HEADACHE*, *DIZZINESS*, *SOMNOLENCE*, *TREMOR*)  
 - Derive the `AEOSI` flag variable using conditional logic  
 - Update the `meta` object by replacing its observation data with the
modified dataset that now includes `AEOSI`

``` r
adae_modified <- meta$data_observation

special_interest_terms <- c(
  "DIARRHOEA", "NAUSEA", "VOMITING", "HEADACHE",
  "DIZZINESS", "SOMNOLENCE", "TREMOR"
)

adae_modified$AEOSI <- ifelse(
  adae_modified$AEDECOD %in% special_interest_terms,
  "Y",
  "N"
)

meta$data_observation <- adae_modified
```

Verify that the AEOSI variable now exists and count the number of
records flagged as “Y”.

``` r
"AEOSI" %in% names(meta$data_observation)
#> [1] TRUE
table(meta$data_observation$AEOSI)
#> 
#>    N    Y 
#> 1069  122
```

Next, verify whether `term1` and `term2` values are already assigned in
the
[`meta_ae_example()`](https://merck.github.io/metalite.ae/reference/meta_ae_example.md)
metadata.  
Extract the parameter mapping for `aeosi0rel` and review the existing
values of `term1` and `term2`.

``` r
param_info <- metalite::collect_adam_mapping(meta, "aeosi0rel")
print(param_info$term1)
#> NULL
print(param_info$term2)
#> NULL
```

The current parameter mapping shows that both `term1` and `term2` are
NULL.  
To proceed, we need to update the analysis plan and parameter
definitions:  
- Create a new plan that includes `aeosi0rel` under the `ae_specific`
analysis parameters

``` r
new_plan <- plan(
  analysis = "ae_summary", population = "apat",
  observation = c("wk12", "wk24"), parameter = "any;rel;ser"
) |>
  add_plan(
    analysis = "ae_specific", population = "apat",
    observation = c("wk12", "wk24"),
    parameter = c("any", "aeosi", "rel", "ser", "dtc0rel", "aeosi0rel")
  ) |>
  add_plan(
    analysis = "ae_listing", population = "apat",
    observation = c("wk12", "wk24"), parameter = c("any", "rel", "ser")
  ) |>
  add_plan(
    analysis = "ae_exp_adj", population = "apat",
    observation = c("wk12", "wk24"), parameter = "any;rel;ser"
  )
```

Update the meta object with the new plan and define the `aeosi0rel`
parameter

``` r
meta <- meta |>
  define_plan(plan = new_plan) |>
  define_parameter(
    name = "aeosi0rel",
    subset = quote(AEOSI == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    term1 = " ",
    term2 = " ",
    label = "adverse events of special interest"
  ) |>
  meta_build()
```

Note: `term1` and `term2` are intentionally set to blank spaces to show
difference between missing vs non missing `term1` and `term2` This
serves as the “before” example, which will later be compared against a
version with fully specified terms.

``` r
param_info <- metalite::collect_adam_mapping(meta, "aeosi0rel")
print(param_info$term1)
#> [1] " "
print(param_info$term2)
#> [1] " "
```

### Analysis preparation

The function
[`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
is used to create a dataset for AE summary analysis by utilizing
predefined keywords specified in the example data `meta`.

The resulting output of the function is an `outdata` object, which
comprises a collection of raw datasets for analysis and reporting.

``` r
outdata <- prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "aeosi0rel"
)
```

## RTF tables

The last step is to prepare the RTF table using
[`tlf_ae_specific()`](https://merck.github.io/metalite.ae/reference/tlf_ae_specific.md).

``` r
outdata |>
  format_ae_specific() |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis,
    orientation = "landscape",
    col_rel_width = c(6, rep(1, 8)),
    path_outtable = "rtf/ae0specific0aeosi0missing.rtf"
  )
```

We can observe in the current table that `term1` and `term2` are absent
from both the title and the row/column labels. In the next step, we will
demonstrate how to incorporate term1 and term2 into any output table
based on our predefined keywords.

Update the `meta` object with the new plan and redefine the `aeosi0rel`
parameter.  
This time, we give `term1` and `term2` meaningful values instead of
leaving them blank.  
This is the “after” example, which we will compare with the earlier
version to see how proper values change the table titles and row/column
labels.

``` r
new_plan <- plan(
  analysis = "ae_summary", population = "apat",
  observation = c("wk12", "wk24"), parameter = "any;rel;ser"
) |>
  add_plan(
    analysis = "ae_specific", population = "apat",
    observation = c("wk12", "wk24"),
    parameter = c("any", "aeosi", "rel", "ser", "dtc0rel", "aeosi0rel")
  ) |>
  add_plan(
    analysis = "ae_listing", population = "apat",
    observation = c("wk12", "wk24"), parameter = c("any", "rel", "ser")
  ) |>
  add_plan(
    analysis = "ae_exp_adj", population = "apat",
    observation = c("wk12", "wk24"), parameter = "any;rel;ser"
  )
# Update the meta object with new plan
meta <- meta |>
  define_plan(plan = new_plan) |>
  define_parameter(
    name = "aeosi0rel",
    subset = quote(AEOSI == "Y" & AEREL == "Y"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    term1 = " Drug-Related",
    term2 = " of Special Interest",
    label = "adverse events of special interest"
  ) |>
  meta_build()
```

Retrieve the parameter mapping for `aeosi0rel` and display `term1` and
`term2` to confirm they match the values defined in the previous step.

``` r
param_info <- metalite::collect_adam_mapping(meta, "aeosi0rel")
print(param_info$term1)
#> [1] " Drug-Related"
print(param_info$term2)
#> [1] " of Special Interest"
```

### Analysis preparation

Use the
[`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md)
function to build the dataset for AE summary analysis.  
This function applies the predefined keywords in `meta` and produces an
`outdata` object, which contains the raw datasets needed for analysis
and reporting.

``` r
outdata <- prepare_ae_specific(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "aeosi0rel"
)
```

## RTF tables

The last step is to prepare the RTF table using
[`tlf_ae_summary()`](https://merck.github.io/metalite.ae/reference/tlf_ae_summary.md).

``` r
outdata |>
  format_ae_specific() |>
  tlf_ae_specific(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific", # Provide analysis type defined in meta$analysis,
    orientation = "landscape",
    col_rel_width = c(6, rep(1, 8)),
    path_outtable = "rtf/ae0specific0aeosi0rel.rtf"
  )
```

The generated table now includes `term1` and `term2` in both the title
and row labels, showing the effect of assigning proper values compared
to the earlier version where these fields were left blank.

- In the title: `term1` appears as **“Drug Related”** and `term2` is
  appended as **“of Special Interest”**.  
- In the row labels:
  - **“with one or more drug-related adverse events of special
    interest”**  
  - **“with no drug-related adverse events of special interest”**
