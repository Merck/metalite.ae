# Test plan
#
# The output is a list.
# Test each component:
# - `population`: match the expectation.
# - `parameter`: match the expectation.
# - `n`: match that from SAS
#   (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example).
# - `order`: match the expectation.
# - `group`: match the expectation.
# - `reference_group`: match the expectation.
# - `prop`: match that from SAS
#   (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example).
# - `diff`: match that from SAS
#   (take r2rtf::r2rtf_adsl and r2rtf::r2rtf_adae as an example).
# - `name`: match the expectation.

library(metalite.ae)
library(metalite)
library(dplyr)
library(tidyr)
library(testthat)

adsl <- r2rtf::r2rtf_adsl
adae <- r2rtf::r2rtf_adae |> mutate(TRT01A = TRTA)

adsl |>
  group_by(TRT01P) |>
  summarise(n = n())

sum_par <- "any;rel;nonser;ser;ser0rel;dth;dtc0rel;disc;disc0drel;disc0ser;disc0ser0rel"
plan <- plan(
  analysis = "ae_summary",
  population = "apat",
  observation = "trtemfl",
  parameter = sum_par,
  pilot = TRUE
)

adsl$TRT01P <- factor(adsl$TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adsl$TRT01A <- factor(adsl$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
adae$TRT01A <- factor(adae$TRT01A, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

meta <- meta_adam(
  observation = adae,
  population = adsl
)

plan_pilot <- plan |>
  subset(pilot) |>
  mutate(display_total = analysis == "ae_summary")

meta <- meta |> define_plan(plan_pilot)

meta <- meta |>
  define_population(
    name = "apat",
    group = "TRT01A",
    subset = ITTFL == "Y",
    label = "(All Participants as Treated)"
  )

meta <- meta |> define_observation(
  name = "trtemfl",
  group = "TRT01A",
  var = "AEDECOD",
  subset = TRTEMFL == "Y",
  label = "Treatment emergent"
)

meta_example <- meta |>
  define_parameter(name = "any") |>
  define_parameter(
    name = "rel",
    subset = AEREL == "RELATED",
    label = "drug-related adverse events"
  ) |>
  define_parameter(
    name = "nonser",
    subset = AESER != "Y" | AESER == "",
    label = "non-serious adverse events"
  ) |>
  define_parameter(
    name = "ser",
    subset = AESER == "Y",
    label = "serious adverse events"
  ) |>
  define_parameter(
    name = "ser0rel",
    subset = AESER == "Y" & AEREL == "RELATED",
    label = "serious drug-related adverse events"
  ) |>
  define_parameter(
    name = "dth",
    subset = AESDTH == "Y",
    label = "adverse events result in death"
  ) |>
  define_parameter(
    name = "dtc0rel",
    subset = AESDTH == "Y" & AEREL == "RELATED",
    label = "drug-related adverse events result in death"
  ) |>
  define_parameter(
    name = "disc",
    subset = toupper(AEACN) == "DRUG WITHDRAWN",
    label = "adverse events resulting in discontinuation"
  ) |>
  define_parameter(
    name = "disc0drel",
    subset = toupper(AEACN) == "DRUG WITHDRAWN" & AEREL == "RELATED",
    label = "drug-related adverse events resulting in discontinuation"
  ) |>
  define_parameter(
    name = "disc0ser",
    subset = toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y",
    label = "serious adverse events resulting in discontinuation"
  ) |>
  define_parameter(
    name = "disc0ser0rel",
    subset = toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y" & AEREL == "RELATED",
    label = "serious drug-related adverse events resulting in discontinuation"
  )


meta_example <- meta_example |>
  define_analysis(
    name = "ae_summary",
    title = "Adverse Event Summary"
  )


meta_example <- meta_example |> meta_build()

meta_example$plan <- meta_example$plan |>
  mutate(output_report = spec_filename(meta))

test_meta <- meta_example
# usethis::use_data(test_meta, overwrite = TRUE)

x <- prepare_ae_summary(
  meta = test_meta,
  population = "apat",
  observation = "trtemfl",
  parameter = sum_par
)

test_that("output from prepare_ae_summary is a list", {
  expect_true("outdata" %in% class(x))
})

xx <- tibble(cbind(x$name, x$n)) |> mutate(x = row_number())
xxp <- x$prop |> mutate(x = row_number())
xxpd <- x$diff
xxpd <- xxpd[order(as.numeric(row.names(xxpd))), ] |> mutate(x = row_number())

rownames(xxp) <- NULL
rownames(xxpd) <- NULL

adsl_tot <- adsl |> mutate(TRT01AN = 99) # Add total rows into calculation
adsl_tot <- rbind(adsl, adsl_tot)

res_tot <- adsl_tot |>
  group_by(TRT01AN, ITTFL) |>
  summarise(n = n_distinct(USUBJID), .groups = "drop")

res_tot1 <- res_tot |>
  mutate(z = n) |>
  select(-c(n))

test_that("Parameter matching", {
  expect_equal(x$parameter, sum_par)
})

test_that("Group matching", {
  expect_equal(x$group, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose", "Total"))
})

test_that("reference_group matching", {
  expect_equal(levels(adsl$TRT01A)[x$reference_group], "Placebo")
})

test_that("Participants in population", {
  res <- res_tot |>
    mutate(x = paste0("n_", row_number()))

  res1 <- res |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  expect_equal(
    xx |> filter(x == 1) |> select(-c("x$name", x)),
    res1 |> select(-c(ITTFL))
  )
  # Population count
  expect_equal(
    xx |> filter(x == 1) |> select(-c("x$name", x)),
    tibble(x$n_pop)
  )
  # Name matches
  expect_equal(x$name[1], "Participants in population")
})

test_that("with one or more adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae2 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop2 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff2 <- res_prop2 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 2) |> select(-c("x$name", x)), res_ae2 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 2) |> select(-c(x)), data.frame(res_prop2) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 2) |> select(-c(x)), data.frame(res_diff2) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[2], "with one or more adverse events")
})

test_that("with one or more adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_no <- full_join(
    res_tot1, # Merge with adsl to get percentage
    res_ae,
    by = c("TRT01AN", "ITTFL")
  ) |> mutate(p = z - n)

  res_no <- res_no |>
    mutate(pct = formatC(100 * p / z, digits = 13, format = "f", flag = "0"))

  res_no$pct <- as.numeric(res_no$pct)

  res_no <- res_no |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae3 <- res_no |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = p,
      values_fill = list(p = 0)
    )

  res_prop3 <- res_no |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff3 <- res_prop3 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 3) |> select(-c("x$name", x)), res_ae3 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 3) |> select(-c(x)), data.frame(res_prop3) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 3) |> select(-c(x)), data.frame(res_diff3) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[3], "with no adverse events")
})

test_that("with drug-related adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AEREL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  if (!"RELATED" %in% res_ae$AEREL) {
    res_ae <- res_ae |>
      select(TRT01AN, ITTFL) |>
      distinct() |>
      mutate(AEREL = "RELATED", n = 0)
  }

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae4 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop4 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff4 <- res_prop4 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 4) |> select(-c("x$name", x)), res_ae4 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 4) |> select(-c(x)), data.frame(res_prop4) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 4) |> select(-c(x)), data.frame(res_diff4) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[4], "with drug-related{^a} adverse events")
})



test_that("non-serious adverse events count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y" & AESER != "Y" | AESER == "")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae5 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop5 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff5 <- res_prop5 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 5) |> select(-c("x$name", x)), res_ae5 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 5) |> select(-c(x)), data.frame(res_prop5) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 5) |> select(-c(x)), data.frame(res_diff5) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[5], "with non-serious adverse events")
})

test_that("serious adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AESER) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AESER = "Y", t = as.integer(0))

  res_ae <- res_ae |> subset(AESER == "Y")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESER")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae6 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop6 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff6 <- res_prop6 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 6) |> select(-c("x$name", x)), res_ae6 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 6) |> select(-c(x)), data.frame(res_prop6) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 6) |> select(-c(x)), data.frame(res_diff6) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[6], "with serious adverse events")
})


test_that("serious drug-related adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AESER, AEREL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    ungroup() |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AESER = "Y", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae |> subset(AESER == "Y" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESER", "AEREL")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae7 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop7 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff7 <- res_prop7 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 7) |> select(-c("x$name", x)), res_ae7 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 7) |> select(-c(x)), data.frame(res_prop7) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 7) |> select(-c(x)), data.frame(res_diff7) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[7], "with serious drug-related adverse events")
})


test_that("death count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AESDTH) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    ungroup() |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AESDTH = "Y", t = as.integer(0))

  res_ae <- res_ae |> subset(AESDTH == "Y")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESDTH")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae8 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop8 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff8 <- res_prop8 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 8) |> select(-c("x$name", x)), res_ae8 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 8) |> select(-c(x)), data.frame(res_prop8) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 8) |> select(-c(x)), data.frame(res_diff8) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[8], "who died")
})


test_that("died due to drug-related adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AESDTH, AEREL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    ungroup() |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AESDTH = "Y", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae |> subset(AESDTH == "Y" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AESDTH", "AEREL")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae9 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop9 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff9 <- res_prop9 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 9) |> select(-c("x$name", x)), res_ae9 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 9) |> select(-c(x)), data.frame(res_prop9) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 9) |> select(-c(x)), data.frame(res_diff9) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[9], "who died due to a drug-related adverse event")
})

test_that("discontinued due to an adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AEACN) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    ungroup() |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AEACN = "DRUG WITHDRAWN", t = as.integer(0))

  res_ae <- res_ae |> subset(toupper(AEACN) == "DRUG WITHDRAWN")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae10 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop10 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff10 <- res_prop10 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 10) |> select(-c("x$name", x)), res_ae10 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 10) |> select(-c(x)), data.frame(res_prop10) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 10) |> select(-c(x)), data.frame(res_diff10) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[10], "discontinued any drug due to an adverse events")
})


test_that("discontinued due to drug-related adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AEACN, AEREL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    ungroup() |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AEACN = "DRUG WITHDRAWN", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae |> subset(toupper(AEACN) == "DRUG WITHDRAWN" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN", "AEREL")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae11 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop11 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff11 <- res_prop11 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 11) |> select(-c("x$name", x)), res_ae11 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 11) |> select(-c(x)), data.frame(res_prop11) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 11) |> select(-c(x)), data.frame(res_diff11) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[11], "discontinued any drug due to a drug-related adverse events")
})


test_that("discontinued due to serious adverse event count", {
  adae_itt <- full_join(adsl |> select(USUBJID, TRT01AN, ITTFL), # merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AEACN, AESER) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    ungroup() |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AEACN = "DRUG WITHDRAWN", AESER = "Y", t = as.integer(0))

  res_ae <- res_ae |> subset(toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN", "AESER")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae12 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop12 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff12 <- res_prop12 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 12) |> select(-c("x$name", x)), res_ae12 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 12) |> select(-c(x)), data.frame(res_prop12) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 12) |> select(-c(x)), data.frame(res_diff12) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[12], "discontinued any drug due to a serious adverse event")
})

test_that("discontinued due to serious drug-related adverse event count", {
  adae_itt <- full_join(
    adsl |> select(USUBJID, TRT01AN, ITTFL), # Merge with adsl to get percentage
    adae,
    by = "USUBJID"
  ) |> subset(ITTFL == "Y" & TRTEMFL == "Y")

  adae_tot <- adae_itt |> mutate(TRT01AN = 99) # Add total rows into calculation
  adae_tot <- rbind(adae_itt, adae_tot)

  res_ae <- adae_tot |>
    group_by(TRT01AN, ITTFL, AEACN, AESER, AEREL) |>
    summarise(n = n_distinct(USUBJID), .groups = "drop")

  res_ae_example <- res_ae |>
    select(TRT01AN, ITTFL) |>
    distinct() |>
    mutate(AEACN = "DRUG WITHDRAWN", AESER = "Y", AEREL = "RELATED", t = as.integer(0))

  res_ae <- res_ae |> subset(toupper(AEACN) == "DRUG WITHDRAWN" & AESER == "Y" & AEREL == "RELATED")

  res_ae <- left_join(res_ae_example,
    res_ae,
    by = c("TRT01AN", "ITTFL", "AEACN", "AESER", "AEREL")
  ) |> select(-c(t))

  res_ae[is.na(res_ae)] <- 0

  res_ae <- data.frame(full_join(res_ae, res_tot1, by = c("TRT01AN", "ITTFL"))) |>
    mutate(pct = formatC(100 * n / z, digits = 13, format = "f", flag = "0"))

  res_ae$pct <- as.numeric(res_ae$pct)

  res_ae <- res_ae |>
    mutate(
      x = paste0("n_", row_number()),
      prop = paste0("prop_", row_number())
    )

  res_ae13 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "x",
      values_from = n,
      values_fill = list(n = 0)
    )

  res_prop13 <- res_ae |>
    pivot_wider(
      id_cols = "ITTFL",
      names_from = "prop",
      values_from = pct,
      values_fill = list(pct = 0)
    )

  res_diff13 <- res_prop13 |> mutate(
    diff_2 = as.numeric(prop_2) - as.numeric(prop_1),
    diff_3 = as.numeric(prop_3) - as.numeric(prop_1)
  )

  # n count
  expect_equal(xx |> filter(x == 13) |> select(-c("x$name", x)), res_ae13 |> select(-c(ITTFL)))
  # Proportion count
  expect_equal(xxp |> filter(x == 13) |> select(-c(x)), data.frame(res_prop13) |> select(-c(ITTFL)))
  # Diff count
  expect_equal(xxpd |> filter(x == 13) |> select(-c(x)), data.frame(res_diff13) |> select(c(diff_2, diff_3)))
  # Name matches
  expect_equal(x$name[13], "discontinued any drug due to a serious drug-related adverse event")
})
