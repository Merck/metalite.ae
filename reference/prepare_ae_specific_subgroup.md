# Prepare datasets for AE specific subgroup analysis

Prepare datasets for AE specific subgroup analysis

## Usage

``` r
prepare_ae_specific_subgroup(
  meta,
  population,
  observation,
  parameter,
  subgroup_var,
  subgroup_header = c(meta$population[[population]]$group, subgroup_var),
  components = c("soc", "par"),
  display_subgroup_total = TRUE
)
```

## Arguments

- meta:

  A metadata object created by metalite.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- subgroup_var:

  A character value of subgroup variable name in observation data saved
  in `meta$data_observation`.

- subgroup_header:

  A character vector for column header hierarchy. The first element will
  be the first level header and the second element will be second level
  header.

- components:

  A character vector of components name.

- display_subgroup_total:

  Logical. Display total column for subgroup analysis or not.

## Value

A list of analysis datasets needed for AE specific subgroup analysis.

## Examples

``` r
meta <- meta_ae_example()
prepare_ae_specific_subgroup(meta, "apat", "wk12", "rel", subgroup_var = "SEX")$data
#> NULL
```
