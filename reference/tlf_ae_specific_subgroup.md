# Specific adverse events table for subgroup analysis

Specific adverse events table for subgroup analysis

## Usage

``` r
tlf_ae_specific_subgroup(
  outdata,
  meddra_version,
  source,
  analysis,
  col_rel_width = NULL,
  text_font_size = 9,
  orientation = "landscape",
  footnotes = NULL,
  title = NULL,
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

RTF file and the source dataset for AE specific subgroup analysis table.

## Examples

``` r
# Define metadata
adsl <- forestly::forestly_adsl
adae <- forestly::forestly_adae

adsl$TRTA <- factor(
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

meta <- metalite::meta_adam(observation = adae, population = adsl) |>
  metalite::define_plan(analysis_plan) |>
  metalite::define_population(
    name = "apat",
    var = c("USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE"),
    group = "TRTA",
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

prepare_ae_specific_subgroup(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  subgroup_var = "SEX",
  display_subgroup_total = TRUE
) |>
  format_ae_specific_subgroup() |>
  tlf_ae_specific_subgroup(
    meddra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_specific",
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The output is saved in/tmp/RtmpPGF8JG/file22c756ad3e1a.rtf
```
