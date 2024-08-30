# Copyright (c) 2024 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
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

#' Create an empty table
#'
#' @inheritParams tlf_ae_specific
#'
#' @return r2rtf object for empty table with a title
#'
#' @noRd
#'
#' @examples
#' library(r2rtf)
#'
#' empty_table(
#'   title = "Participants With Adverse Events",
#'   orientation = "portrait",
#'   text_font_size = 8
#' )

empty_table <- function(title, orientation, text_font_size) {
  # Create an empty table to be displayed
  tbl <- data.frame(
    name = c(NA, "No data to report.", NA)
  )
  names(tbl) <- ""

  # Create an output of r2rtf object
  rtf <- tbl |>
    r2rtf::rtf_page(orientation = orientation) |>
    r2rtf::rtf_title(title) |>
    r2rtf::rtf_colheader(
      colheader = "|",
      text_font_size = text_font_size
    ) |>
    r2rtf::rtf_body(
      text_justification = "l",
      text_font_size = text_font_size
    )
}
