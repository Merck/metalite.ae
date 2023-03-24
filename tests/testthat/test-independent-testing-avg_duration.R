test_that("if group = ... is not a factor, throw errors", {
  r2rtf_adae <- r2rtf::r2rtf_adae
  expect_error(avg_duration(r2rtf_adae$USUBJID, r2rtf_adae$TRTA))
})

test_that("if par = NULL, return the average duration in each group (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae <- r2rtf::r2rtf_adae
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
  db <- data.frame(
    id = r2rtf_adae$USUBJID,
    group = r2rtf_adae$TRTA,
    dur = r2rtf_adae$ADURN
  )
  res <- db |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      avg = mean(dur, na.rm = TRUE),
      se = sd(dur, na.rm = TRUE) / sqrt(dplyr::n()), .groups = "keep"
    )

  avg <- res$avg
  names(avg) <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

  se <- res$se
  names(se) <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

  avg_dur1 <- list(avg = avg, se = se)
  avg_dur2 <- avg_duration(
    r2rtf_adae$USUBJID,
    r2rtf_adae$TRTA,
    dur = r2rtf_adae$ADURN
  )

  expect_equal(avg_dur1, avg_dur2)
})

test_that("if, say, par = AEDECOD, return the average duration per group per AE (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae <- r2rtf::r2rtf_adae
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
  db <- data.frame(
    id = r2rtf_adae$USUBJID,
    group = r2rtf_adae$TRTA,
    dur = r2rtf_adae$ADURN,
    par = r2rtf_adae$AEDECOD
  )

  tmp <- db |>
    dplyr::group_by(group, par) |>
    dplyr::summarise(
      avg = mean(dur, na.rm = TRUE),
      se = sd(dur, na.rm = TRUE) / sqrt(dplyr::n()), .groups = "keep"
    ) |>
    tidyr::pivot_wider(
      id_cols = par,
      names_from = group,
      values_from = c("avg", "se")
    )

  avg <- dplyr::left_join(data.frame(par = unique(r2rtf_adae$AEDECOD)),
    tmp,
    by = dplyr::join_by(par)
  ) |>
    dplyr::select(par, starts_with("avg")) |>
    as.data.frame()
  names(avg) <- stringr::str_remove(names(avg), pattern = "avg_")

  # Expect that avg is ordered by unique AEDECOD which was input.
  expect_equal(unique(r2rtf_adae$AEDECOD), avg$par)

  se <- dplyr::left_join(data.frame(par = unique(r2rtf_adae$AEDECOD)),
    tmp,
    by = dplyr::join_by(par)
  ) |>
    dplyr::select(starts_with("se")) |>
    as.data.frame()
  names(se) <- stringr::str_remove(names(se), pattern = "se_")

  avg_dur1 <- list(avg = avg |> dplyr::select(-par), se = se)
  avg_dur2 <- avg_duration(
    r2rtf_adae$USUBJID,
    r2rtf_adae$TRTA,
    dur = r2rtf_adae$ADURN,
    par = r2rtf_adae$AEDECOD
  )

  expect_equal(avg_dur1, avg_dur2)
})
