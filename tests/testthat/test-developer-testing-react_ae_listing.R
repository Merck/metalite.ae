test_that("react_ae_listing creates an interactive table", {
  skip_if_not_installed("lt")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  result <- react_ae_listing(outdata)

  expect_s3_class(result, "lt_tbl")
  expect_false(is.null(result$interactive))
  expect_true(result$interactive$resize)
  # per-column filter boxes and global search in non-folding mode
  expect_true(result$interactive$filter$default)
  expect_true(result$interactive$search)
})


test_that("react_ae_listing supports patient folding mode", {
  skip_if_not_installed("lt")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  result <- react_ae_listing(outdata, patient_folding = TRUE)
  expect_s3_class(result, "lt_tbl")
  # no global search; only the patient-ID column has a (dropdown) filter
  expect_false(result$interactive$search)
  id_column <- names(result$data)[1]
  expect_null(result$interactive$filter$default)
  expect_identical(result$interactive$filter$cols[[id_column]]$type, "select")
})


test_that("react_ae_listing supports non-folding mode", {
  skip_if_not_installed("lt")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  result <- react_ae_listing(outdata, patient_folding = FALSE)
  expect_s3_class(result, "lt_tbl")
})


test_that("react_ae_listing supports custom table widths", {
  skip_if_not_installed("lt")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  expect_match(react_ae_listing(outdata)$wrap$style, "width:1400px")
  expect_match(react_ae_listing(outdata, width = 700)$wrap$style, "width:700px")
  expect_match(react_ae_listing(outdata, width = "75%")$wrap$style, "width:75%")
})


test_that("react_ae_listing validates input", {
  skip_if_not_installed("lt")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  bad_outdata <- list()

  expect_error(
    react_ae_listing(bad_outdata),
    "created by `format_ae_listing()`",
    fixed = TRUE
  )

  expect_error(
    react_ae_listing(outdata = outdata, patient_folding = NA),
    "`patient_folding` must be either TRUE or FALSE.",
    fixed = TRUE
  )
})
