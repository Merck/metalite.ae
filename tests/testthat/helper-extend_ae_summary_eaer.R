test_extend_ae_summary_eaer <- function() {
  meta <- meta_ae_example()

  outdata_year <- prepare_ae_summary(meta,
    population = "apat",
    observation = "wk12",
    parameter = "any"
  ) |> extend_ae_summary_eaer(adj_unit = "year")

  outdata_month <- prepare_ae_summary(
    meta,
    population = "apat",
    observation = "wk12",
    parameter = "any"
  ) |> extend_ae_summary_eaer(adj_unit = "month")

  adsl <- r2rtf::r2rtf_adsl |> dplyr::mutate(TRTA = TRT01A, TRTAN = TRT01AN)
  adae <- r2rtf::r2rtf_adae

  # output total in adsl
  adsl_total <- adsl |>
    dplyr::group_by(USUBJID) |>
    dplyr::summarize(dplyr::across(dplyr::everything(), dplyr::first)) |>
    dplyr::mutate(TRTA = "Total", TRTAN = 99)

  adsl_new <- dplyr::bind_rows(adsl, adsl_total) |> dplyr::arrange(USUBJID)

  # output total in adae
  adae_total <- adae |>
    dplyr::mutate(TRTA = as.character(TRTA), TRTAN = as.integer(TRTAN)) %>%
    dplyr::bind_rows(., dplyr::mutate(., TRTA = "Total", TRTAN = 99))

  adae_new <- adae_total

  # Create table a
  a <- adsl_new |>
    dplyr::group_by(TRTA, TRTAN) |>
    dplyr::summarize(tot = sum(TRTDUR), n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(TRTAN)

  # Create table b
  b <- adae_new |>
    dplyr::group_by(TRTA, TRTAN) |>
    dplyr::summarize(cnt = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(TRTAN)

  # Merge table a and b
  ab <- merge(a, b, by = c("TRTA", "TRTAN")) |>
    dplyr::arrange(TRTAN)

  # Calculate eaer value by month and year in table ab
  ab1 <- ab |>
    dplyr::mutate(
      trty = tot / 365.24,
      trtm = tot / 30.4367,
      eaer_all_year = (cnt * 100) / trty,
      eaer_all_month = (cnt * 100) / trtm
    )

  list(
    "outdata_year" = outdata_year,
    "outdata_month" = outdata_month,
    "ab1" = ab1
  )
}
