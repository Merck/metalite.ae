meta <- meta_ae_example()

outdata <- prepare_ae_summary(
  meta,
  population = "apat",
  observation = "wk12",
  parameter = "any;rel;ser"
)

path_rtf <- file.path(tempdir(), "tlf_ae_summary.rtf")
path_rdata <- tempfile(fileext = ".Rdata")

tbl <- outdata |>
  format_ae_summary() |>
  tlf_ae_summary(
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    analysis = "ae_summary",
    path_outtable = path_rtf,
    path_outdata = path_rdata
  )

test_that("aesummary.rtf file output", {
  expect_snapshot(tbl)
})
