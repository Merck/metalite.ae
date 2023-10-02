library(metalite)
meta <- meta_ae_example()
outdata <- prepare_ae_specific_subgroup(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  subgroup_var = "SEX"
)

test_that("rtf output: n, and prop w/o total", {
  path_rtf <- file.path(tempdir(), "ednp_ae0specific1.rtf")
  path_rdata <- tempfile(fileext = ".Rdata")

  tbl <- outdata |>
    # extend_ae_specific_events() |>
    # extend_ae_specific_duration(duration_var = "ADURN") |>
    format_ae_specific_subgroup() |>
    tlf_ae_specific_subgroup(
      meddra_version = "24.0",
      source = "Source:  [CDISCpilot: adam-adsl; adae]",
      # path_outdata = path_rdata,
      path_outtable = path_rtf
    )

  testthat::expect_snapshot_file(path_rtf)
})
