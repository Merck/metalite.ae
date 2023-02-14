library(r2rtf)
library(dplyr)
library(tidyr)

test_that("if group = ... is not a factor, throw errors", {
  expect_error(n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA))
})

test_that("if par = NULL, return the number of subjects in each group (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)

  count <- r2rtf_adae |>
    group_by(TRTA) |>
    summarise(n = n_distinct(USUBJID)) |>
    pivot_wider(names_from = "TRTA", values_from = "n", names_sep = " ") |>
    data.frame()

  n_sub <- n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA)
  names(count) <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
  expect_equal(n_sub, count)
})

test_that("if, say, par = AEDECOD, return the number of subject per group per AE (take the r2rtf::r2rtf_adae dataset as an example)", {
  r2rtf_adae$TRTA <- factor(r2rtf_adae$TRTA)

  count <- r2rtf_adae |>
    group_by(TRTA, AEDECOD) |>
    summarise(n = n_distinct(USUBJID)) |>
    pivot_wider(names_from = "TRTA", values_from = "n", names_sep = " ") |>
    data.frame() |>
    arrange(AEDECOD)

  count[is.na(count)] <- 0


  n_sub <- n_subject(r2rtf_adae$USUBJID, r2rtf_adae$TRTA, r2rtf_adae$AEDECOD)
  names(count) <- c("name", "Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
  attr(count$name, "label") <- NULL
  expect_equal(n_sub, count)
})
