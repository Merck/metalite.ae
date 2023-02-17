# Copyright (c) 2023 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
# All rights reserved.
#
# This file is part of the metalite.ae program.
#
# metalite.ae is free software: you can redistribute it and/or modify
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
#' meta_ae_example()
meta_ae_example <- function() {
  adsl <- r2rtf::r2rtf_adsl
  adsl$TRTA <- adsl$TRT01A
  adsl$TRTA <- factor(adsl$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )

  adae <- r2rtf::r2rtf_adae
  adae$TRTA <- factor(adae$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )

  plan <- plan(
    analysis = "ae_summary", population = "apat",
    observation = c("wk12", "wk24"), parameter = "any;rel;ser"
  ) |>
    add_plan(
      analysis = "ae_specific", population = "apat",
      observation = c("wk12", "wk24"),
      parameter = c("any", "aeosi", "rel", "ser")
    )

  meta_adam(
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
    meta_build()
}

#' Create an example `meta_adam` object for AE listing
#'
#' This function is only for illustration purpose.
#' r2rtf is required.
#'
#' @return A metadata object.
#'
#' @export
#'
#' @examples
#' meta_ae_listing_example()
meta_ae_listing_example <- function() {
  adsl <- r2rtf::r2rtf_adsl
  adsl$TRTA <- adsl$TRT01A
  adsl$TRTA <- factor(adsl$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )

  adae <- r2rtf::r2rtf_adae
  adae$TRTA <- factor(adae$TRTA,
    levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
    labels = c("Placebo", "Low Dose", "High Dose")
  )

  adae$subline <- paste0("Age=", adae$AGE, ", ID=", adae$USUBJID, ", Gender=", adae$SEX)

  plan <- plan(
    analysis = "ae_listing", population = "apat",
    observation = "wk12", parameter = c("any", "rel", "ser")
  )

  meta_adam(
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
      label = "Weeks 0 to 12",
      rel_day = "ASTDY"
    ) |>
    define_parameter(
      name = "rel",
      subset = quote(AEREL %in% c("POSSIBLE", "PROBABLE"))
    ) |>
    define_parameter(
      name = "ser",
      subset = quote(AESER == "Y")
    ) |>
    define_analysis(
      name = "ae_listing",
      var_name = c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT"),
      subline_by = NULL,
      group_by = c("USUBJID", "ASTDY"),
      page_by = c("TRTA", "subline")
    ) |>
    meta_build()
}
