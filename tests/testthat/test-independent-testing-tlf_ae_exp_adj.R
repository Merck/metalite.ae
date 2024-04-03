test_that("rtf output: n, total_exp, events, eaer, total", {
  adsl <- r2rtf::r2rtf_adsl
  adae <- r2rtf::r2rtf_adae |> dplyr::mutate(TRT01A = TRTA)

  meta <- meta_ae_example()

  outdata <- prepare_ae_summary(
    meta,
    population = "apat",
    observation = "wk12",
    parameter = "any"
  ) |> extend_ae_summary_eaer(adj_unit = "year")

  path_rtf <- file.path(tempdir(), "ae0summary0a.rtf")
  path_rdata <- tempfile(fileext = ".Rdata")

  tbl <- outdata |>
    format_ae_exp_adj(
      display = c("n", "total_exp"),
      digits_total_exp = 4
    ) |>
    tlf_ae_exp_adj(
      source = "Source: [CDISCpilot: adam-adsl]",
      path_outdata = path_rdata,
      path_outtable = path_rtf
    )

  expect_snapshot_file(path_rtf)
})

test_that("rtf output: n, total_exp, events, eaer, total", {
  adsl <- r2rtf::r2rtf_adsl
  adae <- r2rtf::r2rtf_adae |> dplyr::mutate(TRT01A = TRTA)

  meta <- meta_ae_example()

  outdata <- prepare_ae_summary(
    meta,
    population = "apat",
    observation = "wk12",
    parameter = "any"
  ) |> extend_ae_summary_eaer(adj_unit = "year")

  path_rtf <- file.path(tempdir(), "ae0summary0b.rtf")
  path_rdata <- tempfile(fileext = ".Rdata")

  tbl <- outdata |>
    format_ae_exp_adj(
      display = c("n", "total_exp"),
      digits_total_exp = 3
    ) |>
    tlf_ae_exp_adj(
      source = "Source: [CDISCpilot: adam-adsl]",
      path_outdata = path_rdata,
      path_outtable = path_rtf
    )

  expect_snapshot_file(path_rtf)
})
