test_that("if group = ... is not a factor, throw errors", {
  r2rtf_adae <- r2rtf::r2rtf_adae
  expect_error(avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA))
})

test_that("if par = NULL, return the average number of events in each group (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae <- r2rtf::r2rtf_adae
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)

  db <- data.frame(id = r2rtf_adae$USUBJID, group = r2rtf_adae$TRTA)
  res <- table(db$id, db$group) |> data.frame()

  avg <- vapply(
    split(res, res$Var2),
    function(x) mean(x$Freq, na.rm = TRUE),
    FUN.VALUE = numeric(1)
  )
  se <- vapply(
    split(res, res$Var2),
    function(x) sd(x$Freq, na.rm = TRUE) / sqrt(nrow(x)),
    FUN.VALUE = numeric(1)
  )

  avg1 <- list(avg = avg, se = se)
  avg2 <- avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)

  expect_equal(avg1, avg2)
})

test_that("if par = NULL, return the average number of events in each group (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae <- r2rtf::r2rtf_adae
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)

  db <- data.frame(
    id = r2rtf_adae$USUBJID,
    group = r2rtf_adae$TRTA,
    par = r2rtf_adae$AEDECOD
  )
  res <- table(db$id, db$group, db$par)
  res <- data.frame(res)

  tmp <- db |>
    count(group, par, id) |>
    group_by(group, par) |>
    summarise(avg = mean(n, na.rm = TRUE), se = sd(n, na.rm = TRUE) / sqrt(n())) |>
    tidyr::pivot_wider(id_cols = par, names_from = group, values_from = c("avg", "se"))

  avg <- tmp |>
    dplyr::select(par, starts_with("avg")) |>
    as.data.frame() |>
    dplyr::arrange(par) |>
    dplyr::select(-par)
  names(avg) <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

  se <- tmp |>
    dplyr::select(par, starts_with("se")) |>
    as.data.frame()|>
    dplyr::arrange(par) |>
    dplyr::select(-par)
  names(se) <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

  avg1 <- list(avg = avg, se = se)
  avg2 <- avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$AEDECOD)

  expect_equal(avg1, avg2)
})
