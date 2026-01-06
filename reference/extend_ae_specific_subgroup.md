# Add subgroup analysis in AE specific analysis

Add subgroup analysis in AE specific analysis

## Usage

``` r
extend_ae_specific_subgroup(outdata, subgroup_var)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- subgroup_var:

  a character string for subgroup variable name

## Value

A list of analysis raw datasets.

## Examples

``` r
meta <- meta_ae_example()
tbl <- prepare_ae_specific(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel"
) |>
  extend_ae_specific_subgroup(subgroup_var = "SEX")
```
