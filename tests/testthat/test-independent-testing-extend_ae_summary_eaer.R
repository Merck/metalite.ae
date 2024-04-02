test_that("Comparing specific values from datasets", {
  res <- test_extend_ae_summary_eaer()
  ab1 <- res$ab1
  outdata_year <- res$outdata_year

  ab2y <- data.frame(
    TRTA = c("Placebo", "Low Dose", "High Dose", "Total"),
    Value = ab1$eaer_all_year
  )

  ab3y <- ab2y |> tidyr::pivot_wider(names_from = TRTA, values_from = Value)

  ab4y <- as.data.frame(ab3y)
  eaer_yr <- as.data.frame(outdata_year$eaer)

  expect_equal(ab4y, eaer_yr)
})

test_that("Comparing specific values from datasets", {
  res <- test_extend_ae_summary_eaer()
  ab1 <- res$ab1
  outdata_month <- res$outdata_month

  ab2m <- data.frame(
    TRTA = c("Placebo", "Low Dose", "High Dose", "Total"),
    Value = ab1$eaer_all_month
  )

  ab3m <- ab2m |> tidyr::pivot_wider(names_from = TRTA, values_from = Value)

  ab4m <- as.data.frame(ab3m)
  eaer_mo <- as.data.frame(outdata_month$eaer)

  expect_equal(ab4m, eaer_mo)
})
