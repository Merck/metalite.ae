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
#' @param df A data frame.
#' @param mask A character string to mask the number.
#' @param n A numeric value for the number of rows displayed in the mock.
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
