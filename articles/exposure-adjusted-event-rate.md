# Exposure Adjusted Event Rate

This is a supplementary document for the file “Exposure-Adjusted Adverse
Event Summary (Updated)”. We first provide an explanation for the
calculation of the exposure adjusted event rate (EAER) of adverse
events, and then briefly summarize the programming steps of this
calculation.

## EAER formula explanation

### EAER formula

``` math
\begin{aligned}
EAER_j (\text{EAER for } Trt_j)
&= \frac{\text{total number of events for } Trt_j}{\text{total person-days for } Trt_j/(\text{exp factor})}  \\
&= \frac{\text{total number of events for } Trt_j \times \text{exp factor}}{\text{total person-days for } Trt_j}
\end{aligned}
```

The exposure factor (exp factor) will be adjusted depending on the
adjustment unit defined by users. For instance, when the adjustment unit
is ‘100 person-month’, EAER will be computed as follows:

``` math
\begin{aligned}
EAER_j (\text{100 person-months})
&= \frac{\text{total number of events for } Trt_j \times \text{exp factor} (=100\times30.4367)}{\text{total person-days for } Trt_j}  \\
&= \frac{\text{total number of events for } Trt_j \times 3043.67}{\text{total person-days for } Trt_j}
\end{aligned}
```

### EAER for different types of AEs

As an example, below are the EAER definitions for three types of AEs
using three treatment groups (PBO, Low Dose, High Dose).

#### ANY AE adj rate

``` math
EAER_{PBO} (\text{100 person-months}) =\frac{\text{total number of AEs for PBO} \times 3043.67}{\text{total person-days for PBO}}
```

``` math
EAER_{LD} (\text{100 person-months}) =\frac{\text{total number of AEs for Low Dose} \times 3043.67}{\text{total person-days for Low Dose}}
```

``` math
EAER_{HD} (\text{100 person-months}) =\frac{\text{total number of AEs for High Dose} \times 3043.67}{\text{total person-days for High Dose}}
```

#### Serious AE adj rate

``` math
EAER_{PBO} (\text{100 person-months}) =\frac{\text{total number of SAEs for PBO} \times 3043.67}{\text{total person-days for PBO}}
```

``` math
EAER_{LD} (\text{100 person-months}) =\frac{\text{total number of SAEs for Low Dose} \times 3043.67}{\text{total person-days for Low Dose}}
```

``` math
EAER_{HD} (\text{100 person-months}) =\frac{\text{total number of SAEs for High Dose} \times 3043.67}{\text{total person-days for High Dose}}
```

#### REL AE adj rate

``` math
EAER_{PBO} (\text{100 person-months}) =\frac{\text{total number of Drug-Related AEs for PBO} \times 3043.67}{\text{total person-days for PBO}}
```

``` math
EAER_{LD} (\text{100 person-months}) =\frac{\text{total number of Drug-Related AEs for Low Dose} \times 3043.67}{\text{total person-days for Low Dose}}
```

``` math
EAER_{HD} (\text{100 person-months}) =\frac{\text{total number of Drug-Related AEs for High Dose} \times 3043.67}{\text{total person-days for High Dose}}
```

## Programming steps of EAER

- Data preprocessing: At the end of this step, `adae` is created.
- Build a metadata object: At the end of this step, `meta` is created.
- Call the function
  [`prepare_ae_summary()`](https://merck.github.io/metalite.ae/reference/prepare_ae_summary.md)
  and
  [`extend_ae_summary_eaer()`](https://merck.github.io/metalite.ae/reference/extend_ae_summary_eaer.md).
  This extend function has the following arguments: `outdata`,
  `duration_var`, and `adj_unit`. The output will be a list:

``` r

library(metalite.ae)
```

``` r

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

x <- meta |>
  prepare_ae_summary(
    population = "apat",
    observation = "wk12",
    parameter = "any;rel;ser",
  ) |>
  extend_ae_summary_eaer(
    duration_var = "TRTDUR",
    adj_unit = "month"
  )

x
```

    ## List of 17
    ##  $ meta           :List of 7
    ##  $ population     : chr "apat"
    ##  $ observation    : chr "wk12"
    ##  $ parameter      : chr "any;rel;ser"
    ##  $ n              :'data.frame': 5 obs. of  3 variables:
    ##  $ order          : num [1:5] 1 100 200 300 400
    ##  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
    ##  $ reference_group: num 2
    ##  $ prop           :'data.frame': 5 obs. of  3 variables:
    ##  $ diff           : num [1:5, 1] NA 11.43 35.74 1.19 -11.43
    ##  $ n_pop          :'data.frame': 1 obs. of  3 variables:
    ##  $ name           : chr [1:5] "Participants in population" "with one or more adverse events" "with no adverse events" "with drug-related{^a} adverse events" ...
    ##  $ prepare_call   : language prepare_ae_summary(meta = meta, population = "apat", observation = "wk12",      parameter = "any;rel;ser", )
    ##  $ total_exp      :'data.frame': 1 obs. of  3 variables:
    ##  $ event_num      :'data.frame': 3 obs. of  3 variables:
    ##  $ eaer           :'data.frame': 3 obs. of  3 variables:
    ##  $ adj_unit       : chr "month"

Run `x$eaer` to get the EAER:

``` r

x$eaer
```

    ##      Low Dose  Placebo       Total
    ## 1 159.1724513 71.46214 105.9769666
    ## 2 106.8467949 31.57630  61.1959386
    ## 3   0.3659137  0.00000   0.1439904
