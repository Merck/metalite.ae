test_that("format_ae_summary supports a custom parameter without summ_row", {
  adsl <- forestly::forestly_adsl
  adae <- forestly::forestly_adae

  adsl$TRT01A <- factor(
    adsl$TRT01A,
    levels = c("Xanomeline Low Dose", "Placebo"),
    labels = c("Low Dose", "Placebo")
  )
  adae$TRTA <- factor(
    adae$TRTA,
    levels = c("Xanomeline Low Dose", "Placebo"),
    labels = c("Low Dose", "Placebo")
  )

  analysis_plan <- plan(
    analysis = "ae_summary",
    population = "apat",
    observation = "wk12",
    parameter = "any;drug-related;dtc0rel"
  )

  meta <- meta_adam(observation = adae, population = adsl) |>
    define_plan(analysis_plan) |>
    define_population(
      name = "apat",
      var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
      group = "TRT01A",
      subset = SAFFL == "Y",
      label = "All Participants as Treated"
    ) |>
    define_observation(
      name = "wk12",
      var = c(
        "USUBJID", "SAFFL", "TRTA", "AEDECOD", "AEBODSYS", "AEREL",
        "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
      ),
      group = "TRTA",
      subset = SAFFL == "Y",
      label = "Weeks 0 to 12"
    ) |>
    define_parameter(
      name = "any",
      term1 = "",
      term2 = "",
      var = "AEDECOD",
      soc = "AEBODSYS",
      label = "All AEs"
    ) |>
    define_parameter(
      name = "drug-related",
      term1 = "Drug-Related",
      term2 = "",
      subset = AEREL == "RELATED",
      var = "AEDECOD",
      soc = "AEBODSYS",
      label = "Drug-related AEs"
    ) |>
    define_parameter(
      name = "dtc0rel",
      term1 = "Drug-Related",
      term2 = "Resulting in Death",
      subset = AESDTH == "Y" & AEREL == "Y",
      var = "AEDECOD",
      soc = "AEBODSYS",
      label = "Drug-related AE resulting in death"
    ) |>
    define_analysis(
      name = "ae_summary",
      title = "Adverse Event Summary"
    ) |>
    meta_build()

  outdata <- prepare_ae_summary(
    meta,
    population = "apat",
    observation = "wk12",
    parameter = "any;drug-related;dtc0rel"
  )

  result <- format_ae_summary(
    outdata,
    display = c("n", "prop", "total")
  )

  expect_equal(nrow(result$tbl), nrow(outdata$n))
  expect_equal(
    unname(result$tbl$name),
    c(
      "Participants in population",
      "with one or more adverse events",
      "with no adverse events",
      "Drug-related AEs",
      "who died due to a drug-related adverse event"
    )
  )
})
