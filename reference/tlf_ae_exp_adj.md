# Exposure-adjusted AE summary table

Exposure-adjusted AE summary table

## Usage

``` r
tlf_ae_exp_adj(
  outdata,
  source,
  analysis,
  col_rel_width = NULL,
  text_font_size = 9,
  orientation = "portrait",
  title = c("analysis", "observation", "population"),
  footnotes = NULL,
  path_outdata = NULL,
  path_outtable = NULL
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

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

- title:

  Term "analysis", "observation"and "population") for collecting title
  from metadata or a character vector of table titles.

- footnotes:

  A character vector of table footnotes.

- path_outdata:

  A character string of the outdata path.

- path_outtable:

  A character string of the outtable path.

## Value

RTF file and source dataset for exposure-adjusted AE summary table.

## Examples

``` r
meta <- meta_ae_example()
outdata <- meta |>
  prepare_ae_summary(
    population = "apat",
    observation = "wk12",
    parameter = "any;rel;ser"
  ) |>
  extend_ae_summary_eaer(adj_unit = "month")
#> any
#> rel
#> ser
#> any
#> rel
#> ser
outdata |>
  format_ae_exp_adj() |>
  tlf_ae_exp_adj(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_exp_adj",
    path_outdata = tempfile(fileext = ".Rdata"),
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The outdata is saved in/tmp/RtmpRietUf/file1b40624eb9b2.Rdata
#> The output is saved in/tmp/RtmpRietUf/file1b4066183a4e.rtf
```
