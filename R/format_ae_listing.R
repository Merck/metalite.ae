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

#' Format AE listing analysis
#'
#' @param outdata An `outdata` object created by [prepare_ae_listing()].
#' @param mock A boolean value to display mock table.
#'
#' @return An `outdata` object (a structured list) for AE listing. Key elements
#'   include:
#'   - `meta`: metadata used for analysis.
#'   - `population`, `observation`, `parameter`: selected analysis terms.
#'   - `col_name`: named vector of display labels for listing columns.
#'   - `tbl`: formatted listing data frame used by [tlf_ae_listing()].
#'   - `prepare_call` and `extend_call`: recorded function calls for
#'     reproducibility.
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
#'   analysis = "ae_listing",
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "ser"
#' )
#'
#' meta <- metalite::meta_adam(observation = adae, population = adsl) |>
#'   metalite::define_plan(analysis_plan) |>
#'   metalite::define_population(
#'     name = "apat",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRT01A", "TRTDUR",
#'       "SITEID", "SEX", "RACE", "AGE"
#'     ),
#'     group = "TRT01A",
#'     subset = SAFFL == "Y",
#'     label = "All Participants as Treated"
#'   ) |>
#'   metalite::define_observation(
#'     name = "wk12",
#'     var = c(
#'       "USUBJID", "SAFFL", "TRTA", "AEDECOD", "AEBODSYS", "AEREL",
#'       "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
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
#'     var_name = c(
#'       "USUBJID", "ASTDY", "AEDECOD", "ADURN",
#'       "AESEV", "AESER", "AEREL", "AEOUT"
#'     ),
#'     group_by = c("USUBJID", "ASTDY"),
#'     page_by = "TRTA"
#'   ) |>
#'   metalite::meta_build()
#'
#' outdata <- prepare_ae_listing(
#'   meta,
#'   analysis = "ae_listing",
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "ser"
#' )
#' tbl <- outdata |>
#'   format_ae_listing()
#' head(tbl$tbl)
format_ae_listing <- function(outdata, mock = FALSE) {
  if (is.null(outdata$tbl)) {
    stop("Please provide an outdata object created by `prepare_ae_listing()`.", call. = FALSE)
  }

  res <- as.data.frame(outdata$tbl)

  # Keep the listing columns in the prepared display order.
  if (!is.null(outdata$col_name)) {
    col_order <- names(outdata$col_name)
    col_order <- col_order[col_order %in% names(res)]
    if (length(col_order) > 0) {
      res <- res[, col_order, drop = FALSE]
      outdata$col_name <- outdata$col_name[col_order]
    }
  }

  if (mock) {
    n_mock <- min(20, nrow(res), na.rm = TRUE)
    res <- to_mock(res, n = nrow(res)) |>
      as.data.frame()
    res <- res[1:n_mock, , drop = FALSE]
  }

  outdata$tbl <- res
  outdata$extend_call <- c(outdata$extend_call, match.call())

  outdata
}