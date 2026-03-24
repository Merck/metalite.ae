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
meta <- meta_ae_example()

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
#> The outdata is saved in/tmp/RtmpcVSTF0/file1d3d66f82a11.Rdata
#> The output is saved in/tmp/RtmpcVSTF0/file1d3d4fcdb917.rtf
```
