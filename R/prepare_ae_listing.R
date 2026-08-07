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

#' Prepare datasets for AE listing
#'
#' @param meta A metadata object created by metalite.
#' @param analysis Analysis name from `meta`.
#' @param population A character value of population term name.
#'   The term name is used as key to link information.
#' @param observation A character value of observation term name.
#'   The term name is used as key to link information.
#' @param parameter A character value of parameter term name.
#'   The term name is used as key to link information.
#'
#' @return A list of analysis datasets needed for AE listing.
#'
#' @import metalite
#'
#' @export
#'
#' @examples
#' # Define metadata
#' adsl <- forestly::forestly_adsl
#' adae <- forestly::forestly_adae
#'
#' adsl$TRT01A <- factor(
#'   adsl$TRT01A,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#' adae$TRTA <- factor(
#'   adae$TRTA,
#'   levels = c("Xanomeline Low Dose", "Placebo"),
#'   labels = c("Low Dose", "Placebo")
#' )
#'
#' analysis_plan <- metalite::plan(
#'   analysis = "ae_listing", population = "apat",
#'   observation = "wk12", parameter = "ser"
#' )
#'
#' meta <- metalite::meta_adam(observation = adae, population = adsl) |>
#'   metalite::define_plan(analysis_plan) |>
#'   metalite::define_population(
#'     name = "apat",
#'     var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
#'     group = "TRT01A",
#'     subset = SAFFL == "Y",
#'     label = "All Participants as Treated"
#'   ) |>
#'   metalite::define_observation(
#'     name = "wk12",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRTA", "SEX", "AEDECOD", "AEBODSYS",
#'       "AEREL", "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
#'     ),
#'     group = "TRTA",
#'     subset = SAFFL == "Y",
#'     label = "Weeks 0 to 12"
#'   ) |>
#'   metalite::define_parameter(
#'     name = "ser",
#'     term1 = "Serious",
#'     term2 = "",
#'     subset = AESER == "Y",
#'     var = "AEDECOD",
#'     soc = "AEBODSYS",
#'     label = "Serious AEs"
#'   ) |>
#'   metalite::define_analysis(
#'     name = "ae_listing",
#'     var_name = c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT"),
#'     group_by = c("USUBJID", "ASTDY"),
#'     page_by = "TRTA"
#'   ) |>
#'   metalite::meta_build()
#'
#' str(prepare_ae_listing(meta, "ae_listing", "apat", "wk12", "ser"))
prepare_ae_listing <- function(meta,
                               analysis,
                               population,
                               observation,
                               parameter) {
  mapping <- collect_adam_mapping(meta, analysis)
  var_name <- eval(mapping$var_name)
  subline <- eval(mapping$subline)
  subline_by <- eval(mapping$subline_by)
  group_by <- eval(mapping$group_by)
  page_by <- eval(mapping$page_by)

  var_names <- c(var_name, subline, subline_by, group_by, page_by)

  res <- collect_observation_record(meta, population, observation, parameter,
    var = var_names
  )

  res <- res[names(res) %in% var_names]

  # Sort res data frame by order of var_names
  res <- res[, unique(var_names)]

  # Extract label from data frame as column name of listing
  col_name <- get_label(res)

  # Return value
  outdata(meta, population, observation, parameter,
    n = NULL, order = NULL, group = NULL, reference_group = NULL,
    col_name = col_name, tbl = res,
    prepare_call = match.call()
  )
}
