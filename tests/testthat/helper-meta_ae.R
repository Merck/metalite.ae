meta_ae_test <- function() {
  adsl <- r2rtf::r2rtf_adsl
  adsl$TRTA <- adsl$TRT01A
  adsl$TRTA <- factor(
    adsl$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )
  adsl$RACE <- tools::toTitleCase(adsl$RACE)

  adae <- r2rtf::r2rtf_adae
  adae$TRTA <- factor(
    adae$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )
  adae$RACE <- tools::toTitleCase(adae$RACE)

  adae$related <- ifelse(
    adae$AEREL == "RELATED",
    "Y",
    ifelse(
      toupper(adae$AEREL) == "NOT RELATED",
      "N",
      tools::toTitleCase(tolower(adae$AEREL))
    )
  )

  for (index in seq_along(adae$AEOUT)) {
    adae$outcome <- switch(adae$AEOUT[index],
      "RECOVERED/RESOLVED" = "Resolved",
      "RECOVERING/RESOLVING" = "Resolving",
      "RECOVERED/RESOLVED WITH SEQUELAE" = "Sequelae",
      "NOT RECOVERED/NOT RESOLVED" = "Not Resolved",
      tools::toTitleCase(tolower(adae$AEOUT[index]))
    )
  }

  adae$AEACN <- sample(
    x = c("DOSE NOT CHANGED", "DRUG INTERRUPTED", "DRUG WITHDRAWN", "NOT APPLICABLE", "UNKNOWN"),
    size = length(adae$USUBJID),
    prob = c(0.7, 0.1, 0.05, 0.1, 0.05), replace = TRUE
  )

  for (index in seq_along(adae$AEACN)) {
    adae$action_taken[index] <- switch(adae$AEACN[index],
      "DOSE NOT CHANGED" = "None",
      "DRUG INTERRUPTED" = "Interrupted",
      "DRUG WITHDRAWN" = "Discontinued",
      "NOT APPLICABLE" = "N/A",
      "UNKNOWN" = "Unknown",
      "''" = "None",
      tools::toTitleCase(tolower(adae$AEACN[index]))
    )
  }

  adae$duration <- paste(
    ifelse(
      is.na(adae$ADURN),
      "",
      as.character(adae$ADURN)
    ),
    tools::toTitleCase(tolower(adae$ADURU)),
    sep = " "
  )

  for (index in seq_along(adae$duration)) {
    if (is.na(adae$ADURN[index])) {
      adae$duration[index] <- ifelse(
        charmatch(toupper(adae$AEOUT[index]), "RECOVERING/RESOLVING") > 0 |
          charmatch(toupper(adae$AEOUT[index]), "NOT RECOVERED/NOT RESOLVED") > 0,
        "Continuing",
        "Unknown"
      )
    }
  }

  adae$subline <- paste0(
    "Subject ID = ", adae$USUBJID,
    ", Gender = ", adae$SEX,
    ", Race = ", adae$RACE,
    ", AGE = ", adae$AGE, " Years",
    ", TRT = ", adae$TRTA
  )

  adae <- metalite::assign_label(
    adae,
    var = c("related", "outcome", "duration", "AESEV", "AESER", "AEDECOD", "action_taken"),
    label = c("Related", "Outcome", "Duration", "Intensity", "Serious", "Adverse Event", "Action Taken")
  )

  analysis_plan <- plan(
    analysis = "ae_summary", population = "apat",
    observation = c("wk12", "wk24"), parameter = "any;rel;ser"
  ) |>
    add_plan(
      analysis = "ae_specific", population = "apat",
      observation = c("wk12", "wk24"),
      parameter = c("any", "aeosi", "rel", "ser", "dtc0rel")
    ) |>
    add_plan(
      analysis = "ae_listing", population = "apat",
      observation = c("wk12", "wk24"), parameter = c("any", "rel", "ser")
    ) |>
    add_plan(
      analysis = "ae_exp_adj", population = "apat",
      observation = c("wk12", "wk24"), parameter = "any;rel;ser"
    )

  meta_adam(
    population = adsl,
    observation = adae
  ) |>
    define_plan(plan = analysis_plan) |>
    define_population(
      name = "apat",
      group = "TRTA",
      subset = quote(SAFFL == "Y")
    ) |>
    define_observation(
      name = "wk12",
      group = "TRTA",
      subset = quote(SAFFL == "Y"),
      label = "Weeks 0 to 12"
    ) |>
    define_observation(
      name = "wk24",
      group = "TRTA",
      subset = quote(AOCC01FL == "Y"),
      label = "Weeks 0 to 24"
    ) |>
    define_parameter(
      name = "rel",
      subset = quote(AEREL %in% c("POSSIBLE", "PROBABLE"))
    ) |>
    define_parameter(
      name = "aeosi",
      subset = quote(AEOSI == "Y"),
      var = "AEDECOD",
      soc = "AEBODSYS",
      term1 = "",
      term2 = "of special interest",
      label = "adverse events of special interest"
    ) |>
    define_parameter(
      name = "dtc0rel",
      subset = quote(AESDTH == "Y" & AEREL == "Y"),
      var = "AEDECOD",
      soc = "AEBODSYS",
      term1 = "Drug-Related",
      term2 = "Resulting in Death",
      label = "drug-related adverse events result in death"
    ) |>
    define_analysis(
      name = "ae_summary",
      title = "Summary of Adverse Events"
    ) |>
    define_analysis(
      name = "ae_listing",
      var_name = c(
        "USUBJID", "ASTDY", "AEDECOD", "duration",
        "AESEV", "AESER", "related", "action_taken", "outcome"
      ),
      subline_by = NULL,
      group_by = c("USUBJID", "ASTDY"),
      page_by = c("TRTA", "subline")
    ) |>
    define_analysis(
      name = "ae_exp_adj",
      label = "Exposure Adjusted Incident Rate",
      title = "Exposure-Adjusted Adverse Event Summary"
    ) |>
    meta_build()
}
