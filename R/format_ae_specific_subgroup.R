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

#' Format AE specific analysis
#'
#' @inheritParams format_ae_specific
#' @param digits_prop A numeric value of number of digits for proportion value.
#' @param digits_ci A numeric value of number of digits for confidence interval
#' @param digits_p A numeric value of number of digits for p-value .
#' @param digits_dur A numeric value of number of digits for
#'   average duration of AE.
#' @param digits_events A numeric value of number of digits for
#'   average of number of AE per subjects.
#' @param display A character vector of measurement to be displayed.
#'   - `n`: Number of subjects with AE.
#'   - `prop`: Proportion of subjects with AE.
#'   - `total`: Total columns.
#'   - `diff`: Risk difference.
#'   - `diff_ci`: 95% confidence interval of risk difference using M&N method.
#'   - `diff_p`: p-value of risk difference using M&N method.
#'   - `dur`: Average of AE duration.
#'   - `events`: Average number of AE per subject.
#' @param mock Logical. Display mock table or not.
#'
#' @return A list of analysis raw datasets.
#'
#' @export
#'
#' @examples
#' meta <- meta_ae_example()
#' prepare_ae_specific_subgroup(meta,
#'   population = "apat",
#'   observation = "wk12",
#'   parameter = "rel",
#'   subgroup_var = "SEX",
#'   display_subgroup_total = TRUE
#' ) |>
#'   format_ae_specific_subgroup()
format_ae_specific_subgroup <- function(
    outdata,
    display = c("n", "prop"),
    digits_prop = 1,
    digits_ci = 1,
    digits_p = 3,
    digits_dur = c(1, 1),
    digits_events = c(1, 1),
    mock = FALSE) {
  if ("total" %in% display) {
    display <- display[!display %in% "total"]
    print(paste("total is not supported within Sub-Group"))
  }

  out_all <- outdata$out_all

  outlst <- list()
  for (i in seq_along(out_all)) {
    tbl <- out_all[[i]] |>
      format_ae_specific(
        display = display,
        digits_prop = digits_prop,
        digits_ci = digits_ci,
        digits_p = digits_p,
        digits_dur = digits_dur,
        digits_events = digits_events,
        mock = mock
      )

    names(tbl$tbl)[-1] <- paste0(names(out_all[i]), names(tbl$tbl)[-1])
    if (i == length(out_all)) {
      tbl$tbl$order <- out_all[[i]]$order
    }

    outlst[[i]] <- tbl$tbl
  }

  names(outlst) <- names(out_all)

  i <- 1
  while (i < length(outlst)) {
    if (i == 1) {
      tbl <- merge(outlst[[i]], outlst[[i + 1]], by = "name", all = TRUE)
    }

    i <- i + 1

    if (i > 1 && i < length(outlst)) {
      tbl <- merge(tbl, outlst[[i + 1]], by = "name", all = TRUE)
    }
  }

  # Need order column from Total Column for Ordering properly across tables
  tbl <- tbl[order(tbl$order), ]

  # If outdata$display_subgroup_total = FALSE, remove that part
  if (!outdata$display_subgroup_total) {
    rm_tot <- names(outlst$Total) # Columns from Total Section
    rm_tot <- rm_tot[!rm_tot %in% c("name", "order")]

    tbl <- tbl[, -which(names(tbl) %in% rm_tot)]
  }

  outdata$tbl <- tbl
  outdata$display <- display
  outdata
}
