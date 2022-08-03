library(metalite)

meta <- meta_ae_dummy()
outdata <- prepare_ae_specific(meta,
                               population = "apat",
                               observation = "wk12",
                               parameter = "rel")

path_rtf <- file.path(tempdir(), "ae0specific1.rtf")
path_rdata <- tempfile(fileext = '.Rdata')

tbl <- outdata |>
  format_ae_specific() |>
  tlf_ae_specific(
    medra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    path_outdata =  path_rdata,
    path_outtable =  path_rtf
  )

test_that("ae0specific1.rtf file output", {
  expect_snapshot_file(path_rtf)
})
