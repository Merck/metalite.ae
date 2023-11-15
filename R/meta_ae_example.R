# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the metalite.ae program.
#
# metalite.ae is a free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Create an example `meta_adam` object
#'
#' This function is only for illustration purpose.
#' r2rtf is required.
#'
#' @return A metadata object.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
meta_ae_example <- function() {
  # Create adsl ----
  adsl <- r2rtf::r2rtf_adsl
  adsl$TRTA <- adsl$TRT01A
  adsl$TRTA <- factor(
    adsl$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )
  adsl$RACE <- tools::toTitleCase(adsl$RACE)

  # Create adae ----
  adae <- r2rtf::r2rtf_adae
  adae$TRTA <- factor(
    adae$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )
  adae$RACE <- tools::toTitleCase(adae$RACE)

  # Drug-related AE values
  adae$related <- ifelse(
    adae$AEREL == "RELATED",
    "Y",
    ifelse(
      toupper(adae$AEREL) == "NOT RELATED",
      "N",
      tools::toTitleCase(tolower(adae$AEREL))
    )
  )

  # AE outcome
  for (i in seq_along(adae$AEOUT)) {
    adae$outcome <- switch(adae$AEOUT[i],
      "RECOVERED/RESOLVED" = "Resolved",
      "RECOVERING/RESOLVING" = "Resolving",
      "RECOVERED/RESOLVED WITH SEQUELAE" = "Sequelae",
      "NOT RECOVERED/NOT RESOLVED" = "Not Resolved",
      tools::toTitleCase(tolower(adae$AEOUT[i]))
    )
  }

  # AE action
  adae$AEACN <- gsub("", "DOSE NOT CHANGED", adae$AEACN)

  for (i in seq_along(adae$AEACN)) {
    adae$action_taken[i] <- switch(adae$AEACN[i],
      "DOSE NOT CHANGED" = "None",
      "DOSE REDUCED" = "Reduced",
      "DRUG INTERRUPTED" = "Interrupted",
      "DOSE INCREASED" = "Increased",
      "NOT APPLICABLE" = "N/A",
      "UNKNOWN" = "Unknown",
      "''" = "None",
      tools::toTitleCase(tolower(adae$AEACN[i]))
    )
  }

  # AE duration with unit
  adae$duration <- paste(
    ifelse(
      is.na(adae$ADURN),
      "",
      as.character(adae$ADURN)
    ),
    tools::toTitleCase(tolower(adae$ADURU)),
    sep = " "
  )

  for (i in seq_along(adae$duration)) {
    if (is.na(adae$ADURN[i])) {
      adae$duration[i] <- ifelse(
        charmatch(toupper(adae$AEOUT[i]), "RECOVERING/RESOLVING") > 0 |
          charmatch(toupper(adae$AEOUT[i]), "NOT RECOVERED/NOT RESOLVED") > 0,
        "Continuing",
        "Unknown"
      )
    }
  }

  # AE subject line
  adae$subline <- paste0(
    "Subject ID = ", adae$USUBJID,
    ", Gender = ", adae$SEX,
    ", Race = ", adae$RACE,
    ", AGE = ", adae$AGE, " Years",
    ", TRT = ", adae$TRTA
  )

  # Assign label
  adae <- metalite::assign_label(
    adae,
    var = c("related", "outcome", "duration", "AESEV", "AESER", "AEDECOD", "action_taken"),
    label = c("Related", "Outcome", "Duration", "Intensity", "Serious", "Adverse Event", "Action Taken")
  )

  # Define plan ----
  plan <- plan(
    analysis = "ae_summary", population = "apat",
    observation = c("wk12", "wk24"), parameter = "any;rel;ser"
  ) |>
    add_plan(
      analysis = "ae_specific", population = "apat",
      observation = c("wk12", "wk24"),
      parameter = c("any", "aeosi", "rel", "ser")
    ) |>
    add_plan(
      analysis = "ae_listing", population = "apat",
      observation = c("wk12", "wk24"), parameter = c("any", "rel", "ser")
    ) |>
    add_plan(
      analysis = "ae_exp_adj", population = "apat",
      observation = c("wk12", "wk24"), parameter = "any;rel;ser"
    )

  # Create meta_adam ----
  meta_adam <- meta_adam(
    population = adsl,
    observation = adae
  ) |>
    define_plan(plan = plan) |>
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
      subset = quote(AOCC01FL == "Y"), # Just for demo, another flag should be used
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

  meta_adam
}
