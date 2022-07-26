library(metalite)
library(metalite.ae)

tempdir()
meta <- meta_ae_dummy()
outdata <- prepare_ae_summary(meta,
                              population = "apat",
                              observation = "wk12",
                              parameter = "any;rel;ser")
tbl <- outdata |>
  format_ae_summary()

tbl |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    path_outdata = paste0(tempdir(),"/aesummary2.rdata"),
    path_outtable = paste0(tempdir(),"/aesummary2.rtf"))

test_that("aesummary.rtf file output", {
  expect_snapshot_file(paste0(tempdir(),"/aesummary2.rtf"))
})

test_that("aesummary.rdata file output", {
  expect_snapshot_file(paste0(tempdir(),"/aesummary2.rdata"))
})
