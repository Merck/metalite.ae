adsl <- r2rtf::r2rtf_adsl
adae <- r2rtf::r2rtf_adae |> dplyr::mutate(TRT01A = TRTA)

adsl |>
  dplyr::group_by(adsl$TRT01P) |>
  dplyr::summarize(n = dplyr::n())


plan <- plan(
  analysis = "ae_specific",
  population = "apat",
  observation = c("trtemfl"),
  parameter = c("any"),
  measurement = c("all"),
  pilot = TRUE
)

adsl$TRT01P <- factor(adsl$TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adsl$TRT01A <- factor(adsl$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adae$TRT01A <- factor(adae$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

meta_test <- meta_adam(
  observation = adae,
  population = adsl
)

plan_pilot <- plan |>
  subset(pilot) |>
  dplyr::mutate(display_total = analysis %in% c("ae_specific"))

meta_test <- meta_test |>
  define_plan(plan_pilot)

meta_test <- meta_test |>
  define_population(
    name = "apat",
    group = "TRT01A",
    subset = ITTFL == "Y",
    label = "(All Participants as Treated)"
  )

meta_test <- meta_test |> define_observation(
  name = "trtemfl",
  group = "TRT01A",
  var = "AEDECOD",
  subset = TRTEMFL == "Y",
  label = "Treatment emergent"
)

meta_test <- meta_test |> define_parameter(name = "any")
meta_test <- meta_test |>
  define_analysis(
    name = "ae_specific",
    title = "Participants With {term1} Adverse Events {term2}
                         \\line (Incidence > 0% in One or More Treatment Groups)"
  )


meta_test <- meta_test |> meta_build()

meta_test$plan <- meta_test$plan |>
  dplyr::mutate(output_report = spec_filename(meta_test))

test_meta_specific <- meta_test


x_any <- prepare_ae_specific(test_meta_specific,
  population = "apat",
  observation = "trtemfl",
  parameter = "any"
)

yy <- tibble::tibble(cbind(x_any$name, x_any$n)) |> dplyr::mutate(f = dplyr::row_number())

yyp <- x_any$prop |> dplyr::mutate(f = dplyr::row_number())
yypd <- x_any$diff |> dplyr::mutate(f = dplyr::row_number())

adsl_tot <- adsl |> dplyr::mutate(TRT01AN = 99) # Add total rows into calculation
adsl_tot <- rbind(adsl, adsl_tot)

res_tot <- adsl_tot |>
  dplyr::group_by(TRT01AN, ITTFL) |>
  dplyr::summarize(n = dplyr::n_distinct(USUBJID), .groups = "drop")

res_tot1 <- res_tot |>
  dplyr::mutate(z = n) |>
  dplyr::select(-c(n))

adae_itt <- dplyr::full_join(
  adsl |> dplyr::select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
  adae,
  by = "USUBJID",
  multiple = "all"
) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

adae_tot <- adae_itt |> dplyr::mutate(TRT01AN = 99) # Add total rows into calculation
adae_tot <- rbind(adae_itt, adae_tot) |> dplyr::mutate(AEDECOD = metalite.ae:::to_sentence(AEDECOD), AEBODSYS = metalite.ae:::to_sentence(AEBODSYS))


test_that("Group matching", {
  expect_equal(x_any$group, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total"))
})

test_that("population count", {
  # Population count
  res <- adsl_tot |>
    dplyr::group_by(TRT01AN, ITTFL) |>
    dplyr::summarize(n = dplyr::n_distinct(USUBJID), .groups = "drop")

  res <- res |>
    dplyr::ungroup() |>
    dplyr::mutate(x = paste0("n_", dplyr::row_number()))

  pop_cnt <- res |>
    tidyr::pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )
  expect_equal(yy |> dplyr::filter(f == 1) |> dplyr::select(-c("x_any$name", f)), tibble::tibble(pop_cnt) |> dplyr::select(-c(ITTFL)))
  expect_equal(x_any$name[1], "Participants in population")
})

# With adverse events
test_that("With one or more adverse events", {
  res_ae <- adae_tot |>
    dplyr::group_by(TRT01AN, ITTFL) |>
    dplyr::summarize(n = dplyr::n_distinct(USUBJID), .groups = "drop")

  res_ae <- data.frame(
    dplyr::full_join(
      res_ae, res_tot1,
      by = c("TRT01AN", "ITTFL"),
      multiple = "all"
    )
  ) |>
    dplyr::mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    dplyr::mutate(
      x = paste0("n_", dplyr::row_number()),
      prop = paste0("prop_", dplyr::row_number())
    )


  res_with_ae <- res_ae |>
    tidyr::pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_with_prop <- res_ae |>
    tidyr::pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_with_diff <- res_with_prop |> dplyr::mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  expect_equal(
    yy |> dplyr::filter(f == 2) |> dplyr::select(-c("x_any$name", f)),
    tibble::tibble(res_with_ae) |> dplyr::select(-c(ITTFL))
  )
  # Proportion count
  expect_equal(
    yyp |> dplyr::filter(f == 2) |> dplyr::select(-c(f)),
    data.frame(res_with_prop) |> dplyr::select(-c(ITTFL))
  )
  # Diff count
  expect_equal(
    yypd |> dplyr::filter(f == 2) |> dplyr::select(-c(f)),
    data.frame(res_with_diff) |> dplyr::select(c(diff_2, diff_3))
  )
  # Name matches
  expect_equal(x_any$name[2], "with one or more adverse events")
})

test_that("With no adverse events", {
  res_ae <- adae_tot |>
    dplyr::group_by(TRT01AN, ITTFL) |>
    dplyr::summarize(n = dplyr::n_distinct(USUBJID), .groups = "drop")

  res_noadv <- dplyr::full_join(
    res_tot1, # Merge with adsl to get percentage
    res_ae,
    by = c("TRT01AN", "ITTFL"),
    multiple = "all"
  ) |> dplyr::mutate(p = z - n)

  res_noadv <- res_noadv |>
    dplyr::mutate(pct = formatC(100 * p / z, digits = 13, format = "f", flag = "0"))

  res_noadv$pct <- as.numeric(res_noadv$pct)

  res_noadv <- res_noadv |>
    dplyr::mutate(
      x = paste0("n_", dplyr::row_number()),
      prop = paste0("prop_", dplyr::row_number())
    )

  res_ae3 <- res_noadv |>
    tidyr::pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = p,
      values_fill = list(p = 0)
    )

  res_prop3 <- res_noadv |>
    tidyr::pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff3 <- res_prop3 |> dplyr::mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(
    yy |> dplyr::filter(f == 3) |> dplyr::select(-c("x_any$name", f)),
    res_ae3 |> dplyr::select(-c(ITTFL))
  )
  # Proportion count
  expect_equal(
    yyp |> dplyr::filter(f == 3) |> dplyr::select(-c(f)),
    data.frame(res_prop3) |> dplyr::select(-c(ITTFL))
  )
  # Diff count
  expect_equal(
    yypd |> dplyr::filter(f == 3) |> dplyr::select(-c(f)),
    data.frame(res_diff3) |> dplyr::select(c(diff_2, diff_3))
  )
  # Name matches
  expect_equal(x_any$name[3], "with no adverse events")
})

test_that("With aebodsys, aedecod count", {
  # Count by AEBODSYS, AEDECOD
  res_ae_aed <- adae_tot |>
    dplyr::group_by(TRT01AN, ITTFL, AEBODSYS, AEDECOD) |>
    dplyr::summarize(n = dplyr::n_distinct(USUBJID), .groups = "drop") |>
    dplyr::arrange(AEBODSYS, AEDECOD, TRT01AN) |>
    dplyr::mutate(x = dplyr::case_when(
      TRT01AN == 0 ~ "n_1",
      TRT01AN == 54 ~ "n_2",
      TRT01AN == 81 ~ "n_3",
      TRT01AN == 99 ~ "n_4"
    )) |>
    dplyr::ungroup()

  res_ae_aed <- data.frame(
    dplyr::full_join(
      res_ae_aed, res_tot1,
      by = c("TRT01AN", "ITTFL"),
      multiple = "all"
    )
  ) |>
    dplyr::mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0")) |>
    dplyr::mutate(p = dplyr::case_when(
      TRT01AN == 0 ~ "prop_1",
      TRT01AN == 54 ~ "prop_2",
      TRT01AN == 81 ~ "prop_3",
      TRT01AN == 99 ~ "prop_4"
    )) |>
    dplyr::ungroup()

  res_ae_aed$pct <- as.numeric(res_ae_aed$pct)

  res_ae_aed1 <- res_ae_aed |>
    tidyr::pivot_wider(
      id_cols = c("AEBODSYS", "AEDECOD"),
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_ae_aed2 <- res_ae_aed |>
    tidyr::pivot_wider(
      id_cols = c("AEBODSYS", "AEDECOD"),
      names_from = "p",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  # Count by AEBODSYS
  res_ae_aeb <- adae_tot |>
    dplyr::group_by(TRT01AN, ITTFL, AEBODSYS) |>
    dplyr::summarize(n = dplyr::n_distinct(USUBJID), .groups = "drop") |>
    dplyr::arrange(AEBODSYS, TRT01AN) |>
    dplyr::mutate(x = dplyr::case_when(
      TRT01AN == 0 ~ "n_1",
      TRT01AN == 54 ~ "n_2",
      TRT01AN == 81 ~ "n_3",
      TRT01AN == 99 ~ "n_4"
    )) |>
    dplyr::ungroup()

  res_ae_aeb <- data.frame(
    dplyr::full_join(
      res_ae_aeb, res_tot1,
      by = c("TRT01AN", "ITTFL"),
      multiple = "all"
    )
  ) |>
    dplyr::mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0")) |>
    dplyr::mutate(p = dplyr::case_when(
      TRT01AN == 0 ~ "prop_1",
      TRT01AN == 54 ~ "prop_2",
      TRT01AN == 81 ~ "prop_3",
      TRT01AN == 99 ~ "prop_4"
    )) |>
    dplyr::ungroup()

  res_ae_aeb$pct <- as.numeric(res_ae_aeb$pct)

  res_ae_aeb1 <- res_ae_aeb |>
    tidyr::pivot_wider(
      id_cols = c("AEBODSYS"),
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    ) |>
    dplyr::mutate(AEDECOD = "NA")

  res_ae_aeb2 <- res_ae_aeb |>
    tidyr::pivot_wider(
      id_cols = c("AEBODSYS"),
      names_from = "p",
      values_from = pct,
      values_fill = list(pct = 0)
    ) |>
    dplyr::mutate(AEDECOD = "NA")

  res_ae_aebd <- rbind(res_ae_aeb1, res_ae_aed1) |>
    dplyr::arrange(AEBODSYS) |>
    dplyr::mutate("x_any$name" = ifelse(AEDECOD == "NA", AEBODSYS, AEDECOD)) |>
    dplyr::select(c("x_any$name", n_1, n_2, n_3, n_4))

  res_ae_aebd2 <- rbind(res_ae_aeb2, res_ae_aed2) |>
    dplyr::arrange(AEBODSYS) |>
    dplyr::mutate("x_any$name" = ifelse(AEDECOD == "NA", AEBODSYS, AEDECOD)) |>
    dplyr::select(c(prop_1, prop_2, prop_3, prop_4))

  res_ae_prop <- res_ae_aebd2 |>
    dplyr::mutate(
      diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
      diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
    ) |>
    dplyr::select(c(diff_2, diff_3))

  # n count
  expect_equal(yy |> dplyr::filter(f >= 5) |> dplyr::select(-c(f)), tibble::tibble(res_ae_aebd))
  # Prop count
  expect_equal(yyp |> dplyr::filter(f >= 5) |> dplyr::select(-c(f)), data.frame(res_ae_aebd2))
  # Diff count
  expect_equal(yypd |> dplyr::filter(f >= 5) |> dplyr::select(-c(f)), data.frame(res_ae_prop))
})

test_that("population matches", {
  expect_equal(x_any$population, "apat")
})

test_that("observation matches", {
  expect_equal(x_any$observation, "trtemfl")
})

test_that("parameter matches", {
  expect_equal(x_any$parameter, "any")
})

test_that("Reference group matches", {
  expect_equal(levels(adsl$TRT01A)[x_any$reference_group], "Placebo")
})

test_that("output from prepare_ae_specific is a list", {
  expect_true("outdata" %in% class(x_any))
})
