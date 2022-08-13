library(metalite)

meta <- meta_ae_dummy()
outdata <- prepare_ae_specific(meta,
                               population = "apat",
                               observation = "wk12",
                               parameter = "rel")


# test_that("rtf output: n and prop w/ total", {
#   path_rtf <- file.path(tempdir(), "npt_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     format_ae_specific() |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
# test_that("rtf output: n and prop w/o total", {
#   path_rtf <- file.path(tempdir(), "np_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     format_ae_specific(display = c("n", "prop")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
# test_that("rtf output: n only", {
#   path_rtf <- file.path(tempdir(), "n_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     format_ae_specific(display = c("n")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
#
# test_that("rtf output: events, n, and prop w/ total", {
#   path_rtf <- file.path(tempdir(), "enpt_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_events() |>
#     format_ae_specific(display = c("events", "n", "prop", "total")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
# test_that("rtf output: events, n, and prop w/o total", {
#   path_rtf <- file.path(tempdir(), "enp_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_events() |>
#     format_ae_specific(display = c("events", "n", "prop")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
#
# test_that("rtf output: events, dur, n, and prop w/o total", {
#   path_rtf <- file.path(tempdir(), "ednp_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_events() |>
#     extend_ae_specific_duration(duration_var = "ADURN") |>
#     format_ae_specific(display = c("events", "dur", "n", "prop")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
# test_that("rtf output: events, dur, n, and prop w/ total", {
#   path_rtf <- file.path(tempdir(), "ednpt_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_events() |>
#     extend_ae_specific_duration(duration_var = "ADURN") |>
#     format_ae_specific(display = c("events", "dur", "n", "prop", "total")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
# test_that("rtf output: events, n, and prop w/ total", {
#   path_rtf <- file.path(tempdir(), "enpt_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_events() |>
#     format_ae_specific(display = c("events", "n", "prop", "total")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
# test_that("rtf output: events, n, and prop w/o total", {
#   path_rtf <- file.path(tempdir(), "enp_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_events() |>
#     format_ae_specific(display = c("events", "n", "prop")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })


test_that("rtf output: events, dur, n, and prop w/o total", {
  path_rtf <- file.path(tempdir(), "ednp_ae0specific1.rtf")
  path_rdata <- tempfile(fileext = '.Rdata')

  tbl <- outdata |>
    extend_ae_specific_events() |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    format_ae_specific(display = c("events", "dur", "n", "prop")) |>
    tlf_ae_specific(
      medra_version = "24.0",
      source = "Source:  [CDISCpilot: adam-adsl; adae]",
      path_outdata =  path_rdata,
      path_outtable =  path_rtf
    )

  testthat::expect_snapshot_file(path_rtf)
})

test_that("rtf output: events, dur, n, and prop w/ total", {
  path_rtf <- file.path(tempdir(), "ednpt_ae0specific1.rtf")
  path_rdata <- tempfile(fileext = '.Rdata')

  tbl <- outdata |>
    extend_ae_specific_events() |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    format_ae_specific(display = c("events", "dur", "n", "prop", "total")) |>
    tlf_ae_specific(
      medra_version = "24.0",
      source = "Source:  [CDISCpilot: adam-adsl; adae]",
      path_outdata =  path_rdata,
      path_outtable =  path_rtf
    )

  testthat::expect_snapshot_file(path_rtf)
})


test_that("rtf output: diff, events, dur, n, and prop w/o total", {
  path_rtf <- file.path(tempdir(), "diednp_ae0specific1.rtf")
  path_rdata <- tempfile(fileext = '.Rdata')

  tbl <- outdata |>
    extend_ae_specific_inference() |>
    extend_ae_specific_events() |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    format_ae_specific(display = c("events", "dur", "n", "prop",
      "diff", "diff_p", "diff_ci")) |>
    tlf_ae_specific(
      medra_version = "24.0",
      source = "Source:  [CDISCpilot: adam-adsl; adae]",
      path_outdata =  path_rdata,
      path_outtable =  path_rtf
    )

  testthat::expect_snapshot_file(path_rtf)
})

test_that("rtf output: diff, events, dur, n, and prop w/ total", {
  path_rtf <- file.path(tempdir(), "diednpt_ae0specific1.rtf")
  path_rdata <- tempfile(fileext = '.Rdata')

  tbl <- outdata |>
    extend_ae_specific_inference() |>
    extend_ae_specific_events() |>
    extend_ae_specific_duration(duration_var = "ADURN") |>
    format_ae_specific(display = c("events", "dur", "n", "prop", "total",
      "diff", "diff_p", "diff_ci")) |>
    tlf_ae_specific(
      medra_version = "24.0",
      source = "Source:  [CDISCpilot: adam-adsl; adae]",
      path_outdata =  path_rdata,
      path_outtable =  path_rtf
    )

  testthat::expect_snapshot_file(path_rtf)
})

#
# test_that("rtf output: diff, n, and prop w/ total", {
#   path_rtf <- file.path(tempdir(), "dinpt_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_inference() |>
#     format_ae_specific(display = c("n", "prop", "total",
#       "diff", "diff_p", "diff_ci")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })
#
# test_that("rtf output: diff, n, and prop w/o total", {
#   path_rtf <- file.path(tempdir(), "dinp_ae0specific1.rtf")
#   path_rdata <- tempfile(fileext = '.Rdata')
#
#   tbl <- outdata |>
#     extend_ae_specific_inference() |>
#     format_ae_specific(display = c("n", "prop", "total",
#       "diff", "diff_p", "diff_ci")) |>
#     tlf_ae_specific(
#       medra_version = "24.0",
#       source = "Source:  [CDISCpilot: adam-adsl; adae]",
#       path_outdata =  path_rdata,
#       path_outtable =  path_rtf
#     )
#
#   testthat::expect_snapshot_file(path_rtf)
# })

test_that("relative width 'works'", {
  path_rtf <- file.path(tempdir(), "dinp_ae0specific1.rtf")
  path_rdata <- tempfile(fileext = '.Rdata')

  expect_error({
  tbl <- outdata |>
    format_ae_specific(display = c("n", "prop")) |>
    tlf_ae_specific(
      medra_version = "24.0",
      source = "Source:  [CDISCpilot: adam-adsl; adae]",
      path_outdata =  path_rdata,
      path_outtable =  path_rtf,
      col_rel_width = c(rep(1, 8))
    )},
    regexp = "col_rel_width must have the same length"
  )

  expect_message({
    tbl <- outdata |>
      format_ae_specific(display = c("n", "prop")) |>
      tlf_ae_specific(
        medra_version = "24.0",
        source = "Source:  [CDISCpilot: adam-adsl; adae]",
        path_outdata =  path_rdata,
        path_outtable =  path_rtf,
        col_rel_width = c(rep(1, 7))
      )},
    regexp = "The outdata is saved")
})