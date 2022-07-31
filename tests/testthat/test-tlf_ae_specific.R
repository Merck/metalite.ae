library(metalite)

meta <- meta_ae_dummy()
outdata <- prepare_ae_specific(meta,
                               population = "apat",
                               observation = "wk12",
                               parameter = "rel")
tbl <- outdata |>
  format_ae_specific() |>
  tlf_ae_specific(
    medra_version = "24.0",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    path_outdata =  file.path(testthat::test_path(),'fixtures', 'ae0specific1.rdata'),
    path_outtable =  file.path(testthat::test_path(),'fixtures', 'ae0specific1.rtf')
  )

test_that("tlf_ae_specific() test", {
  expect_snapshot_file(file.path(testthat::test_path(),'fixtures', 'ae0specific1.rdata'))
  expect_snapshot_file(file.path(testthat::test_path(),'fixtures', 'ae0specific1.rtf'))
})

