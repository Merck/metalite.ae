test_that("gt_ae_specific creates a gt table", {
  outdata <- meta_ae_test() |>
    prepare_ae_specific(
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    extend_ae_specific_inference() |>
    format_ae_specific(display = c("n", "prop", "diff", "diff_ci", "diff_p"))

  result <- outdata |>
    gt_ae_specific(
      meddra_version = "24.0",
      source = "Source: test data",
      analysis = "ae_specific"
    )

  html <- gt::as_raw_html(result)

  expect_s3_class(result, "gt_tbl")
  expect_match(html, "Placebo", fixed = TRUE)
  expect_match(html, "Difference in % Low Dose vs. Placebo", fixed = TRUE)
  expect_match(html, "MedDRA Version 24.0", fixed = TRUE)
  expect_match(html, "Source: test data", fixed = TRUE)
})

test_that("gt_ae_specific validates the analysis name", {
  outdata <- meta_ae_test() |>
    prepare_ae_specific(
      population = "apat",
      observation = "wk12",
      parameter = "rel"
    ) |>
    format_ae_specific()

  expect_error(
    gt_ae_specific(
      outdata,
      meddra_version = "24.0",
      source = NULL,
      analysis = "invalid"
    ),
    "Please provide a valid analysis"
  )
})