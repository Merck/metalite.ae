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
#'   If `TRUE`, all rows are hidden by default and only the first column
#'   (patient ID column) is filterable; other column filters are disabled.
#'   In this mode, records are shown only when the entered value exactly matches
#'   a full patient ID in the first column.
#'   If `FALSE`, all rows are displayed and filters are available for all columns.
#'
#' @return A `reactable` htmlwidget object.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("reactable", quietly = TRUE) &&
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
                             patient_folding = FALSE) {
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop(
      "Package `reactable` is required. Please install it with install.packages('reactable').",
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

  # Apply display labels while preserving the prepared column order.
  if (!is.null(outdata$col_name)) {
    col_order <- names(outdata$col_name)
    col_order <- col_order[col_order %in% names(tbl)]
    if (length(col_order) > 0) {
      tbl <- tbl[, col_order, drop = FALSE]
      col_labels <- outdata$col_name[col_order]
      names(tbl) <- unname(col_labels)
    }
  }

  id_column <- names(tbl)[1]

  column_defs <- lapply(names(tbl), function(column_name) {
    filterable_col <- if (patient_folding) {
      identical(column_name, id_column)
    } else {
      TRUE
    }

    filter_method_col <- NULL
    if (patient_folding && identical(column_name, id_column)) {
      # In folding mode, only an exact full patient ID should return records.
      filter_method_col <- reactable::JS(
        "function(rows, columnId, filterValue) {
          var value = String(filterValue == null ? '' : filterValue).trim();
          if (value === '') {
            return [];
          }

          return rows.filter(function(row) {
            var cell = row.values[columnId];
            return String(cell == null ? '' : cell).trim() === value;
          });
        }"
      )
    }

    reactable::colDef(
      name = column_name,
      filterable = filterable_col,
      filterMethod = filter_method_col,
      minWidth = 120
    )
  })
  names(column_defs) <- names(tbl)

  row_style <- NULL
  if (patient_folding) {
    id_column_js <- gsub("'", "\\\\'", id_column)
    row_style <- reactable::JS(sprintf(
      "function(rowInfo, state) {
        if (!rowInfo || !rowInfo.values) {
          return {};
        }

        var filters = (state && state.filters) ? state.filters : [];
        var idFilter = '';
        for (var i = 0; i < filters.length; i++) {
          if (filters[i] && filters[i].id === '%s') {
            idFilter = String(filters[i].value == null ? '' : filters[i].value).trim();
            break;
          }
        }

        if (idFilter === '') {
          return { display: 'none' };
        }

        var patientId = String(rowInfo.values['%s'] == null ? '' : rowInfo.values['%s']).trim();
        if (patientId === idFilter) {
          return {};
        }

        return { display: 'none' };
      }",
      id_column_js,
      id_column_js,
      id_column_js
    ))
  }

  widget <- reactable::reactable(
    tbl,
    columns = column_defs,
    filterable = TRUE,
    searchable = if (patient_folding) FALSE else searchable,
    striped = striped,
    highlight = highlight,
    rowStyle = row_style,
    defaultPageSize = default_page_size,
    pagination = TRUE,
    bordered = TRUE,
    compact = TRUE,
    wrap = FALSE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 15, 25, 50)
  )

  widget
}
