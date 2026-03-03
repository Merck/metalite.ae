# Generate AE listing

Generate AE listing

## Usage

``` r
tlf_ae_listing(
  outdata,
  footnotes = NULL,
  source = NULL,
  analysis,
  col_rel_width = NULL,
  text_font_size = 9,
  orientation = "landscape",
  path_outdata = NULL,
  path_outtable = NULL
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_listing()`](https://merck.github.io/metalite.ae/reference/prepare_ae_listing.md).

- footnotes:

  A character vector of table footnotes.

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

- path_outdata:

  A character string of the outdata path.

- path_outtable:

  A character string of the outtable path.

## Value

RTF file and the source dataset for AE listing.

## Examples

``` r
library(r2rtf)
library(metalite)

meta <- meta_ae_example()
prepare_ae_listing(meta, "ae_listing", "apat", "wk12", "ser") |>
  tlf_ae_listing(
    footnotes = "footnote1",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_listing",
    path_outdata = tempfile(fileext = ".Rdata"),
    path_outtable = tempfile(fileext = ".rtf")
  )
#> The outdata is saved in/tmp/RtmpRietUf/file1b403ee52d5a.Rdata
#> The output is saved in/tmp/RtmpRietUf/file1b40695fb2fd.rtf
```
