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

  count <- vapply(
    split(res, res$Var2),
    function(x) sum(x$Freq, na.rm = TRUE),
    FUN.VALUE = numeric(1)
  )

  avg1 <- list(avg = avg, se = se, count = count)
  avg2 <- avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)

  expect_equal(avg1, avg2)
})

test_that("if par not NULL, return the average number of events in each group (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae <- r2rtf::r2rtf_adae
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)

  db <- data.frame(
    id = r2rtf_adae$USUBJID,
    group = r2rtf_adae$TRTA,
    par = r2rtf_adae$AEDECOD
  )

  tmp <- db |>
    dplyr::count(group, par, id) |>
    dplyr::group_by(group, par) |>
    dplyr::summarise(
      avg = mean(n, na.rm = TRUE),
      se = sd(n, na.rm = TRUE) / sqrt(dplyr::n()),
      count = sum(n),
      .groups = "keep"
    ) |>
    tidyr::pivot_wider(id_cols = par, names_from = group, values_from = c("avg", "se", "count"))

  avg <- dplyr::left_join(data.frame(par = unique(r2rtf_adae$AEDECOD)),
    tmp,
    by = dplyr::join_by(par)
  ) |>
    dplyr::select(par, starts_with("avg")) |>
    dplyr::arrange(par) |>
    dplyr::select(-par)
  names(avg) <- sub(x = names(avg), pattern = "avg_", replacement = "")

  se <- dplyr::left_join(data.frame(par = unique(r2rtf_adae$AEDECOD)),
    tmp,
    by = dplyr::join_by(par)
  ) |>
    dplyr::select(par, starts_with("se")) |>
    dplyr::arrange(par)
  names(se) <- sub(x = names(se), pattern = "se_", replacement = "")

  count <- dplyr::left_join(data.frame(par = unique(r2rtf_adae$AEDECOD)),
    tmp,
    by = dplyr::join_by(par)
  ) |>
    dplyr::select(par, starts_with("count")) |>
    dplyr::arrange(par)
  names(count) <- sub(x = names(count), pattern = "count_", replacement = "")

  avg1 <- list(
    avg = avg,
    se = se |>
      dplyr::select(-par),
    count = count |>
      dplyr::select(-par)
  )
  avg2 <- avg_event(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$AEDECOD)

  expect_equal(avg1, avg2)
})
