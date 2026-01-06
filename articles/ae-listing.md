# AE Listing

``` r
library(metalite.ae)
```

## Overview

The objective of this tutorial is to generate a production-ready adverse
events (AE) listing.

The AE listing offers comprehensive information on the desired adverse
events. There are two essential functions for constructing AE listing
tables with metalite.ae:

- [`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md):
  prepare AE listing datasets.
- [`tlf_ae_listing()`](https://merck.github.io/metalite.ae/reference/tlf_ae_listing.md):
  transfer output datasets to RTF files.

An example output:

## Example data

Within metalite.ae, we utilized the ADSL and ADAE datasets from the
metalite package to create an illustrative dataset. The metadata
structure remains consistent across all analysis examples within
metalite.ae. Additional information can be accessed on the [metalite
package
website](https://merck.github.io/metalite/articles/metalite.html).

``` r
meta <- meta_ae_example()
```

Click to show the output

``` r
meta
#> ADaM metadata: 
#>    .$data_population     Population data with 254 subjects 
#>    .$data_observation    Observation data with 1191 records 
#>    .$plan    Analysis plan with 20 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group var       subset                         label
#> 1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group var          subset           label
#> 1 'wk12' 'USUBJID' 'TRTA'        SAFFL == 'Y' 'Weeks 0 to 12'
#> 2 'wk24' 'USUBJID' 'TRTA'     AOCC01FL == 'Y' 'Weeks 0 to 24'
#> 
#> 
#>   Analysis parameter type:
#>        name                                         label
#> 1     'rel'                 'drug-related adverse events'
#> 2   'aeosi'          'adverse events of special interest'
#> 3 'dtc0rel' 'drug-related adverse events result in death'
#> 4     'any'                          'any adverse events'
#> 5     'ser'                      'serious adverse events'
#>                                 subset
#> 1 AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 2                         AEOSI == 'Y'
#> 3         AESDTH == 'Y' & AEREL == 'Y'
#> 4                                     
#> 5                         AESER == 'Y'
#> 
#> 
#>   Analysis function:
#>            name                             label
#> 1  'ae_summary'    'Table: adverse event summary'
#> 2  'ae_listing'          'Listing: adverse event'
#> 3  'ae_exp_adj' 'Exposure Adjusted Incident Rate'
#> 4 'ae_specific'   'Table: specific adverse event'
```

## Analysis preparation

The function
[`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md)
is used to create a dataset for AE listing by utilizing predefined
keywords specified in the example data `meta`.

The resulting output of the function is an `outdata` object, which
comprises a collection of raw datasets for analysis and reporting.

``` r
tbl <- prepare_ae_listing(
  meta,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "ser"
)
```

``` r
head(tbl$tbl)
#>          USUBJID ASTDY                                        AEDECOD duration
#> 689  01-709-1424     5                                        SYNCOPE    1 Day
#> 1131 01-718-1170    27                                        SYNCOPE    2 Day
#> 1173 01-718-1371    38 PARTIAL SEIZURES WITH SECONDARY GENERALISATION    4 Day
#>         AESEV AESER  related action_taken  outcome      TRTA
#> 689  MODERATE     Y Possible         None Resolved High Dose
#> 1131   SEVERE     Y Probable         None Resolved  Low Dose
#> 1173   SEVERE     Y     None  Interrupted Resolved High Dose
#>                                                                                  subline
#> 689  Subject ID = 01-709-1424, Gender = M, Race = WHITE, AGE = 77 Years, TRT = High Dose
#> 1131  Subject ID = 01-718-1170, Gender = F, Race = WHITE, AGE = 80 Years, TRT = Low Dose
#> 1173 Subject ID = 01-718-1371, Gender = F, Race = WHITE, AGE = 69 Years, TRT = High Dose
```

``` r
head(tbl$col_name)
#>                       USUBJID                         ASTDY 
#>   "Unique Subject Identifier" "Analysis Start Relative Day" 
#>                       AEDECOD                      duration 
#>               "Adverse Event"                    "Duration" 
#>                         AESEV                         AESER 
#>                   "Intensity"                     "Serious"
```

## RTF tables

The last step is to prepare the RTF table using
[`tlf_ae_listing()`](https://merck.github.io/metalite.ae/reference/tlf_ae_listing.md).

``` r
footnote <- c(
  "Related: Investigator-assessed relationship of the adverse event to study medication. Y = RELATED, N = NOT RELATED",
  "Action Taken: Discontinued = DRUG WITHDRAWN, Interrupted = DRUG INTERRUPTED, Reduced = DOSE REDUCED, Increased = DOSE INCREASED, None = DOSE NOT CHANGED, N/A = NOT APPLICABLE.",
  "Outcome: Resolved = RECOVERED/RESOLVED, Resolving = RECOVERING/RESOLVING, Sequelae = RECOVERED/RESOLVED WITH SEQUELAE, Not resolved = NOT RECOVERED/NOT RESOLVED.",
  "Adverse event terms are from MedDRA Version 25.0."
)
```

``` r
tbl |> tlf_ae_listing(
  footnotes = footnote,
  orientation = "portrait",
  source = "Source:  [CDISCpilot: adam-adsl; adae]",
  analysis = "ae_listing", # Provide analysis type defined in meta$analysis
  path_outtable = "rtf/ae0listing0ser0wk12.rtf",
  path_outdata = NULL
)
#> The output is saved in/home/runner/work/metalite.ae/metalite.ae/vignettes/rtf/ae0listing0ser0wk12.rtf
```
