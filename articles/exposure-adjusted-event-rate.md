# Exposure Adjusted Event Rate

This is a supplementary document for the file “Exposure-Adjusted Adverse
Event Summary (Updated)”. We first provide an explanation for the
calculation of the exposure adjusted event rate (EAER) of adverse
events, and then briefly summarize the programming steps of this
calculation.

## EAER formula explanation

### EAER formula

$$\begin{aligned}
{EAER_{j}\left( {\text{EAER for}\mspace{6mu}}Trt_{j} \right)} & {= \frac{{\text{total number of events for}\mspace{6mu}}Trt_{j}}{{\text{total person-days for}\mspace{6mu}}Trt_{j}/\left( \text{exp factor} \right)}} \\
 & {= \frac{{\text{total number of events for}\mspace{6mu}}Trt_{j} \times \text{exp factor}}{{\text{total person-days for}\mspace{6mu}}Trt_{j}}}
\end{aligned}$$

The exposure factor (exp factor) will be adjusted depending on the
adjustment unit defined by users. For instance, when the adjustment unit
is ‘100 person-month’, EAER will be computed as follows:

$$\begin{aligned}
{EAER_{j}\left( \text{100 person-months} \right)} & {= \frac{{\text{total number of events for}\mspace{6mu}}Trt_{j} \times \text{exp factor}( = 100 \times 30.4367)}{{\text{total person-days for}\mspace{6mu}}Trt_{j}}} \\
 & {= \frac{{\text{total number of events for}\mspace{6mu}}Trt_{j} \times 3043.67}{{\text{total person-days for}\mspace{6mu}}Trt_{j}}}
\end{aligned}$$

### EAER for different types of AEs

As an example, below are the EAER definitions for three types of AEs
using three treatment groups (PBO, Low Dose, High Dose).

#### ANY AE adj rate

$$EAER_{PBO}\left( \text{100 person-months} \right) = \frac{\text{total number of AEs for PBO} \times 3043.67}{\text{total person-days for PBO}}$$

$$EAER_{LD}\left( \text{100 person-months} \right) = \frac{\text{total number of AEs for Low Dose} \times 3043.67}{\text{total person-days for Low Dose}}$$

$$EAER_{HD}\left( \text{100 person-months} \right) = \frac{\text{total number of AEs for High Dose} \times 3043.67}{\text{total person-days for High Dose}}$$

#### Serious AE adj rate

$$EAER_{PBO}\left( \text{100 person-months} \right) = \frac{\text{total number of SAEs for PBO} \times 3043.67}{\text{total person-days for PBO}}$$

$$EAER_{LD}\left( \text{100 person-months} \right) = \frac{\text{total number of SAEs for Low Dose} \times 3043.67}{\text{total person-days for Low Dose}}$$

$$EAER_{HD}\left( \text{100 person-months} \right) = \frac{\text{total number of SAEs for High Dose} \times 3043.67}{\text{total person-days for High Dose}}$$

#### REL AE adj rate

$$EAER_{PBO}\left( \text{100 person-months} \right) = \frac{\text{total number of Drug-Related AEs for PBO} \times 3043.67}{\text{total person-days for PBO}}$$

$$EAER_{LD}\left( \text{100 person-months} \right) = \frac{\text{total number of Drug-Related AEs for Low Dose} \times 3043.67}{\text{total person-days for Low Dose}}$$

$$EAER_{HD}\left( \text{100 person-months} \right) = \frac{\text{total number of Drug-Related AEs for High Dose} \times 3043.67}{\text{total person-days for High Dose}}$$

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
meta <- meta_ae_example()

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
    ##  $ n              :'data.frame': 5 obs. of  4 variables:
    ##  $ order          : num [1:5] 1 100 200 300 400
    ##  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
    ##  $ reference_group: num 1
    ##  $ prop           :'data.frame': 5 obs. of  4 variables:
    ##  $ diff           :'data.frame': 5 obs. of  2 variables:
    ##  $ n_pop          :'data.frame': 1 obs. of  4 variables:
    ##  $ name           : chr [1:5] "Participants in population" "with one or more adverse events" "with no adverse events" "with drug-related{^a} adverse events" ...
    ##  $ prepare_call   : language prepare_ae_summary(meta = meta, population = "apat", observation = "wk12",      parameter = "any;rel;ser", )
    ##  $ total_exp      :'data.frame': 1 obs. of  4 variables:
    ##  $ event_num      :'data.frame': 3 obs. of  4 variables:
    ##  $ eaer           :'data.frame': 3 obs. of  4 variables:
    ##  $ adj_unit       : chr "month"

Run `x$eaer` to get the EAER:

``` r
x$eaer
```

    ##    Placebo    Low Dose   High Dose       Total
    ## 1 71.46214 159.1724513 165.8725416 122.9359029
    ## 2 31.57630 106.8467949 101.7108552  72.6674019
    ## 3  0.00000   0.3659137   0.7291101   0.3096622
