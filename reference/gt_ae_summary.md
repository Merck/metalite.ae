# AE summary table

AE summary table

## Usage

``` r
gt_ae_summary(
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

RTF file and the source dataset for AE summary table.

## Examples

``` r
library(gt)
meta <- meta_ae_example()
outdata <- prepare_ae_summary(meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
)
#> any
#> rel
#> ser
outdata |>
  format_ae_summary() |>
  gt_ae_summary(
    analysis = "ae_summary",
    source = "Source:  [CDISCpilot: adam-adsl; adae]"
  )
#> [1] "Summary of Adverse Events"   "Weeks 0 to 12"              
#> [3] "All Participants as Treated"


  


Summary of Adverse Events

Weeks 0 to 12

All Participants as Treated
```

Placebo

Low Dose

High Dose

Total

n

(%)

n

(%)

n

(%)

n

(%)

Participants in population

86

  

84

  

84

  

254

  

with one or more adverse events

69

(80.2)

77

(91.7)

79

(94.0)

225

(88.6)

with no adverse events

17

(19.8)

7

(8.3)

5

(6.0)

29

(11.4)

with drug-related^(a) adverse events

44

(51.2)

73

(86.9)

70

(83.3)

187

(73.6)

with serious adverse events

0

(0.0)

1

(1.2)

2

(2.4)

3

(1.2)

^(a)Determined by the investigator to be related to the drug.

Source: \[CDISCpilot: adam-adsl; adae\]
