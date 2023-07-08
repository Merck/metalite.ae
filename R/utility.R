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

#' Create mock table
#'
#' @param df a data frame
#' @param mask a character string to mask te number
#' @param n a numeric value for the number of rows displayed in the mock.
#'
#' @noRd
to_mock <- function(df, mask = "x", n = 20) {
  if (is.null(df)) {
    return(df)
  }
  n <- min(c(n, nrow(df)), na.rm = TRUE)

  apply(df[1:n, ], 2, function(x) {
    gsub("[0-9]", mask, as.character(x))
  })
}

#' Convert to sentence case
#'
#' @param x A character vector.
#'
#' @noRd
#'
#' @examples
#' metalite.ae:::to_sentence("this is An Example")
to_sentence <- function(x) {
  gsub("(^[[:alpha:]])", "\\U\\1", tolower(x), perl = TRUE)
}

#' Save outputs for RTF generation.
#'
#' @param outdata a `outdata` object
#' @param path_outdata a character string for file path to save the outdata.
#' @param pat_outtable a character string for file path to save the RTF table.
#'
#' @noRd
rtf_output <- function(
    outdata,
    path_outdata,
    path_outtable) {
  if (!is.null(path_outdata)) {
    save(outdata, file = path_outdata)
    message("The outdata is saved in", normalizePath(path_outdata))
  }

  if (!is.null(path_outtable)) {
    outdata$rtf |>
      r2rtf::rtf_encode() |>
      r2rtf::write_rtf(file = path_outtable)
    message("The output is saved in", normalizePath(path_outtable))
  }

  invisible(outdata)
}