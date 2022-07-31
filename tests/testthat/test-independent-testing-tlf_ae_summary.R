library(metalite)

meta <- meta_ae_dummy()
outdata <- prepare_ae_summary(meta,
                              population = "apat",
                              observation = "wk12",
                              parameter = "any;rel;ser"
)

tbl <- outdata |>
  format_ae_summary() |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    path_outtable = file.path(testthat::test_path(),'fixtures', 'aesummary2.rtf'),
    path_outdata = file.path(testthat::test_path(),'fixtures', 'aesummary2.rdata')
  )

test_that("aesummary.rtf file output", {
  expect_snapshot_file(file.path(testthat::test_path(),'fixtures', 'aesummary2.rtf'))
  expect_snapshot_file(file.path(testthat::test_path(),'fixtures', 'aesummary2.rdata'))
})


#
# test_that("aesummary.rdata file output", {
#   expect_snapshot_file(tbl |>
#                          tlf_ae_summary(
#                            source = "Source:  [CDISCpilot: adam-adsl; adae]",
#                            path_outdata = "./tests/testthat/fixtures/aesummary2.rdata"),
#                        "./tests/testthat/fixtures/aesummary2.rdata")
# })

