test_that("react_ae_listing creates an interactive table", {
  skip_if_not_installed("reactable")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  result <- react_ae_listing(outdata)

  expect_s3_class(result, "reactable")
  expect_s3_class(result, "htmlwidget")
})


test_that("react_ae_listing supports patient folding mode", {
  skip_if_not_installed("reactable")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  result <- react_ae_listing(outdata, patient_folding = TRUE)
  expect_s3_class(result, "reactable")
})


test_that("react_ae_listing supports non-folding mode", {
  skip_if_not_installed("reactable")

  outdata <- meta_ae_test() |>
    prepare_ae_listing(
      analysis = "ae_listing",
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_listing()

  result <- react_ae_listing(outdata, patient_folding = FALSE)
  expect_s3_class(result, "reactable")
})


test_that("react_ae_listing validates input", {
  skip_if_not_installed("reactable")

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
