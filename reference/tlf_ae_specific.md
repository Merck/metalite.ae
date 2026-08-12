# Specific adverse events table

Specific adverse events table

## Usage

``` r
tlf_ae_specific(
  outdata,
  meddra_version,
  source,
  analysis,
  col_rel_width = NULL,
  text_font_size = 9,
  orientation = "portrait",
  footnotes = NULL,
  title = c("analysis", "observation", "population"),
  path_outdata = NULL,
  path_outtable = NULL
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- meddra_version:

  A character value of the MedDRA version for this dataset.

- source:

  A character value of the data source.

- analysis:

  One of analysis name existing at `outdata$meta$analysis`

- col_rel_width:

  Column relative width in a vector e.g. c(2,1,1) refers to 2:1:1.
  Default is NULL for equal column width.

- text_font_size:

  Text font size. To vary text font size by column, use numeric vector
  with length of vector equal to number of columns displayed e.g.
  c(9,20,40).

- orientation:

  Orientation in 'portrait' or 'landscape'.

- footnotes:

  A character vector of table footnotes.

- title:

  Term "analysis", "observation"and "population") for collecting title
  from metadata or a character vector of table titles.

- path_outdata:

  A character string of the outdata path.

- path_outtable:

  A character string of the outtable path.

## Value

RTF file and the source dataset for AE specific table.

## Examples

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
  analysis = "ae_specific",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

analysis_plan <- metalite::plan(
  analysis = "ae_specific",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)
meta <- metalite::meta_adam(observation = adae, population = adsl) |>
  metalite::define_plan(analysis_plan) |>
  metalite::define_population(
    name = "apat",
    var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
    group = "TRT01A",
    subset = SAFFL == "Y",
    label = "All Participants as Treated"
  ) |>
  metalite::define_observation(
    name = "wk12",
    var = c(
      "USUBJID", "SAFFL", "TRTA", "SEX", "AEDECOD", "AEBODSYS",
      "AEREL", "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
    ),
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
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
  metalite::define_analysis(
    name = "ae_specific",
    title = "Participants With Drug-Related Adverse Events"
  ) |>
  metalite::meta_build()

meta |>
  prepare_ae_specific(
    population = "apat",
    observation = "wk12",
    parameter = "rel"
  ) |>
  format_ae_specific() |>
  tlf_ae_specific(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific",
    meddra_version = "24.0",
    path_outdata = tempfile(fileext = ".Rdata"),
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The outdata is saved in/tmp/Rtmpb4lFg4/file1a9246e7531b.Rdata
#> The output is saved in/tmp/Rtmpb4lFg4/file1a927cec9076.rtf
```
