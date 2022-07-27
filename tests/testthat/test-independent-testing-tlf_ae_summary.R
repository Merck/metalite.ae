library(metalite)
library(metalite.ae)

meta <- meta_ae_dummy()
outdata <- prepare_ae_summary(meta,
                              population = "apat",
                              observation = "wk12",
                              parameter = "any;rel;ser")
tbl <- outdata |>
  format_ae_summary() |>
                        tlf_ae_summary(
                           source = "Source:  [CDISCpilot: adam-adsl; adae]",
                           path_outtable = "./tests/testthat/fixtures/aesummary2.rtf",
                           path_outdata = "./tests/testthat/fixtures/aesummary2.rdata")

test_that("aesummary.rtf file output", {
  # expect_snapshot_file(tbl |>
  #                        tlf_ae_summary(
  #                          source = "Source:  [CDISCpilot: adam-adsl; adae]",
  #                          path_outtable = "./tests/testthat/fixtures/aesummary2.rtf"),
  #                      "./tests/testthat/fixtures/aesummary2.rtf")
  expect_snapshot_file("./tests/testthat/fixtures/aesummary2.rtf")
})
#
# test_that("aesummary.rdata file output", {
#   expect_snapshot_file(tbl |>
#                          tlf_ae_summary(
#                            source = "Source:  [CDISCpilot: adam-adsl; adae]",
#                            path_outdata = "./tests/testthat/fixtures/aesummary2.rdata"),
#                        "./tests/testthat/fixtures/aesummary2.rdata")
# })
