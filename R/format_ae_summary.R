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

#' Format AE summary analysis
#'
#' @inheritParams format_ae_specific
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' outdata <- prepare_ae_summary(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "any;rel;ser"
#' )
#' tbl <- outdata |>
#'   format_ae_summary()
#' head(tbl$tbl)
format_ae_summary <- function(outdata,
                              display = c("n", "prop", "total"),
                              hide_soc_stats = FALSE,
                              digits_prop = 1,
                              digits_ci = 1,
                              digits_p = 3,
                              digits_dur = c(1, 1),
                              digits_events = c(1, 1),
                              filter_method = c("percent", "count"),
                              filter_criteria = 0,
                              sort_order = c("alphabetical", "count_des", "count_asc"),
                              sort_column = NULL,
                              mock = FALSE) {
  if (length(outdata$name) != nrow(outdata$n)) {
    parameters <- unlist(strsplit(outdata$parameter, ";"))
    summary_names <- vapply(parameters, function(parameter) {
      mapping <- collect_adam_mapping(outdata$meta, parameter)

      if (is.null(mapping$summ_row)) mapping$label else mapping$summ_row
    }, character(1))

    if ("any" %in% parameters) {
      summary_names <- append(
        summary_names,
        "with no adverse events",
        after = 1
      )
    }

    outdata$name <- c(outdata$name[1], summary_names)
  }

  format_ae_specific(
    outdata = outdata,
    display = display,
    hide_soc_stats = hide_soc_stats,
    digits_prop = digits_prop,
    digits_ci = digits_ci,
    digits_p = digits_p,
    digits_dur = digits_dur,
    digits_events = digits_events,
    filter_method = filter_method,
    filter_criteria = filter_criteria,
    sort_order = sort_order,
    sort_column = sort_column,
    mock = mock
  )
}
