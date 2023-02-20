test_that("if group = ... is not a factor, throw errors", {
  expect_error(avg_duration(r2rtf_adae$USUBJID, r2rtf_adae$TRTA))
})

test_that("if par = NULL, return the average duration in each group (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
  db <- data.frame(
    id = r2rtf_adae$USUBJID,
    group = r2rtf_adae$TRTA,
    dur = r2rtf_adae$ADURN
  )
  res <- db |>
    group_by(group) |>
    summarise(
      avg = mean(dur, na.rm = TRUE),
      se = sd(dur, na.rm = TRUE) / sqrt(n())
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
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)
  db <- data.frame(
    id = r2rtf_adae$USUBJID,
    group = r2rtf_adae$TRTA,
    dur = r2rtf_adae$ADURN,
    par = r2rtf_adae$AEDECOD
  )

  tmp <- db |>
    group_by(group, par) |>
    summarise(
      avg = mean(dur, na.rm = TRUE),
      se = sd(dur, na.rm = TRUE) / sqrt(n())
    ) |>
    tidyr::pivot_wider(
      id_cols = par,
      names_from = group,
      values_from = c("avg", "se")
    )

  avg <- tmp |>
    select(starts_with("avg")) |>
    as.data.frame()
  names(avg) <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

  se <- tmp |>
    select(starts_with("se")) |>
    as.data.frame()
  names(se) <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

  avg_dur1 <- list(avg = avg, se = se)
  avg_dur2 <- avg_duration(
    r2rtf_adae$USUBJID,
    r2rtf_adae$TRTA,
    dur = r2rtf_adae$ADURN,
    par = r2rtf_adae$AEDECOD
  )

  expect_equal(avg_dur1, avg_dur2)
})
