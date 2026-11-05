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

#' Interactive AE listing table
#'
#' @param outdata An `outdata` object created by [format_ae_listing()].
#' @param default_page_size Number of rows to display per page.
#' @param searchable A boolean value to enable global search. The default is TRUE.
#' @param striped A boolean value to display striped rows. The default is TRUE.
#' @param highlight A boolean value to highlight row on hover. The default is TRUE.
#' @param patient_folding A boolean value to control patient-level folding. The default is FALSE.
#'   If `TRUE`, the table starts empty and only the first column (patient ID
#'   column) is filterable, as a dropdown of full patient IDs; other column
#'   filters and the global search are disabled. Records are shown only after a
#'   full patient ID is selected (exact match).
#'   If `FALSE`, all rows are displayed and filters are available for all columns.
#' @param width Table width in pixels or as a CSS value. The default is 1400 pixels.
#'
#' @return An `lt_tbl` object (from the \pkg{lt} package).
#'
#' @export
#'
#' @examples
#' if (requireNamespace("lt", quietly = TRUE) &&
#'   requireNamespace("forestly", quietly = TRUE)) {
#'   adsl <- forestly::forestly_adsl
#'   adae <- forestly::forestly_adae
#'
#'   adsl$TRT01A <- factor(
#'     adsl$TRT01A,
#'     levels = c("Xanomeline Low Dose", "Placebo"),
#'     labels = c("Low Dose", "Placebo")
#'   )
#'   adae$TRTA <- factor(
#'     adae$TRTA,
#'     levels = c("Xanomeline Low Dose", "Placebo"),
#'     labels = c("Low Dose", "Placebo")
#'   )
#'
#'   analysis_plan <- metalite::plan(
#'     analysis = "ae_listing",
#'     population = "apat",
#'     observation = "wk12",
#'     parameter = "rel"
#'   )
#'
#'   meta <- metalite::meta_adam(observation = adae, population = adsl) |>
#'     metalite::define_plan(analysis_plan) |>
#'     metalite::define_population(
#'       name = "apat",
#'       var = c(
#'         "USUBJID", "SAFFL", "TRT01A", "TRTDUR",
#'         "SITEID", "SEX", "RACE", "AGE"
#'       ),
#'       group = "TRT01A",
#'       subset = SAFFL == "Y",
#'       label = "All Participants as Treated"
#'     ) |>
#'     metalite::define_observation(
#'       name = "wk12",
#'       var = c(
#'         "USUBJID", "SAFFL", "TRTA", "AEDECOD", "AEBODSYS", "AEREL",
#'         "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
#'       ),
#'       group = "TRTA",
#'       subset = SAFFL == "Y",
#'       label = "Weeks 0 to 12"
#'     ) |>
#'     metalite::define_parameter(
#'       name = "rel",
#'       term1 = "Related",
#'       term2 = "",
#'       subset = AEREL == "RELATED",
#'       var = "AEDECOD",
#'       soc = "AEBODSYS",
#'       label = "Related AEs"
#'     ) |>
#'     metalite::define_analysis(
#'       name = "ae_listing",
#'       var_name = c(
#'         "USUBJID", "ASTDY", "AEDECOD", "ADURN",
#'         "AESEV", "AESER", "AEREL", "AEOUT"
#'       ),
#'       group_by = c("USUBJID", "ASTDY"),
#'       page_by = "TRTA"
#'     ) |>
#'     metalite::meta_build()
#'
#'   meta |>
#'     prepare_ae_listing(
#'       analysis = "ae_listing",
#'       population = "apat",
#'       observation = "wk12",
#'       parameter = "rel"
#'     ) |>
#'     format_ae_listing() |>
#'     react_ae_listing()
#' }
react_ae_listing <- function(outdata,
                             default_page_size = 15,
                             searchable = TRUE,
                             striped = TRUE,
                             highlight = TRUE,
                             patient_folding = FALSE,
                             width = 1400) {
  if (!requireNamespace("lt", quietly = TRUE)) {
    stop(
      "Package `lt` is required. Please install it with remotes::install_github('yihui/lt').",
      call. = FALSE
    )
  }

  if (is.null(outdata$tbl)) {
    stop("Please provide an outdata object created by `format_ae_listing()`.", call. = FALSE)
  }

  if (!is.logical(patient_folding) || length(patient_folding) != 1 || is.na(patient_folding)) {
    stop("`patient_folding` must be either TRUE or FALSE.", call. = FALSE)
  }

  tbl <- as.data.frame(outdata$tbl, stringsAsFactors = FALSE)

  # Keep the raw column names as data keys and map display labels via lt_label,
  # preserving the prepared column order.
  labels <- NULL
  if (!is.null(outdata$col_name)) {
    col_order <- names(outdata$col_name)
    col_order <- col_order[col_order %in% names(tbl)]
    if (length(col_order) > 0) {
      tbl <- tbl[, col_order, drop = FALSE]
      labels <- outdata$col_name[col_order]
    }
  }

  id_column <- names(tbl)[1]

  # Offer the requested page size first, then common sizes.
  pager <- unique(c(default_page_size, 10, 25, 50, 100))

  if (patient_folding) {
    # Privacy mode: no global search; only the patient-ID column is filterable,
    # as a dropdown of full IDs (exact match). Seed with a control-character
    # sentinel absent from the data so the table starts empty until a
    # participant is picked.
    filter <- stats::setNames(
      list(list(type = "select", box = FALSE, selected = "\u0001")),
      id_column
    )
    search <- FALSE
  } else {
    filter <- TRUE
    search <- isTRUE(searchable)
  }

  x <- lt::lt(tbl, auto_format = FALSE, auto_label = is.null(labels))
  if (!is.null(labels)) {
    x <- lt::lt_label(x, list(names(labels), unname(labels)))
  }
  x <- lt::lt_interactive(
    x,
    search = search,
    filter = filter,
    pager = pager,
    resize = TRUE
  )

  # Striped rows and hover highlight (lt has no default for either).
  rules <- list()
  if (isTRUE(striped)) rules[["tbody tr:nth-child(even)"]] <- "background: #f7f7f7"
  if (isTRUE(highlight)) rules[["tbody tr:hover"]] <- "background: #f0f0f0"
  if (length(rules)) x <- do.call(lt::lt_css, c(list(x), rules))

  # Size the table's container (a wider table scrolls inside it).
  width_css <- if (is.numeric(width)) paste0(width, "px") else width
  x <- lt::lt_wrap(x, style = paste0("width:", width_css, ";overflow-x:auto"))

  x
}
