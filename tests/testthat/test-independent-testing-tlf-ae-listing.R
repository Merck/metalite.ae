x <- meta_ae_example()

outdata <- prepare_ae_listing(
  x,
  analysis = "ae_listing",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

path_rtf <- file.path(tempdir(), "ednp_ae0listing.rtf")
path_rdata <- tempfile(fileext = ".Rdata")

tbl <- outdata |>
  tlf_ae_listing(
    footnotes = "footnote1",
    source = "Source:  [CDISCpilot: adam-adsl; adae]",
    path_outdata = path_rdata,
    path_outtable = path_rtf
  )

test_that("rtf output: ae listing", {
  testthat::expect_snapshot(tbl)
})
