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

#' Format percentage
#'
#' @param x A numeric vector.
#' @param digits Number of digits.
#' @param pre Text before the number.
#' @param post Text after the number.
#'
#' @return A numeric vector with the expected format.
#'
#' @export
#'
#' @examples
#' fmt_pct(c(1, 1.52, 0.3, 100))
fmt_pct <- function(x, digits = 1, pre = "(", post = ")") {
  x1 <- ifelse(is.na(x), x, formatC(x, digits = digits, format = "f"))

  x2 <- ifelse(is.na(x1), x1, paste0(pre, x1, post))

  ifelse(is.na(x2), x2, formatC(x2, format = "f"))
}

#' Format model estimator
#'
#' Formats mean sd/se to a format as x.x or x.x (x.xx) if both
#' mean and sd/sd are defined.
#'
#' The function assumes 1 column or 2 columns:
#' - If there is only 1 column, only represent mean.
#' - If there are 2 columns, represent mean (sd) or mean(se).
#' Decimals will understand the number will be formatted as x.x (x.xx).
#'
#' @param mean A numeric vector of mean value.
#' @param sd A numeric vector of sd value.
#' @param digits Digits of each column, i.e., format as x.x (x.xx).
#' @param width Width of each column.
#'
#' @return The same data frame with additional attributes for page features.
#'
#' @section Specification:
#' \if{latex}{
#'  \itemize{
#'    \item Check all argument types and possible values.
#'    \item Add attributes into \code{tbl}.
#'  }
#'  }
#' \if{html}{The contents of this section are shown in PDF user manual only.}
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' x <- datasets::iris |>
#'   summarise(
#'     mean = mean(Petal.Length),
#'     n = n(),
#'     sd = sd(Petal.Length)
#'   )
#' fmt_est(x$mean, x$sd)
fmt_est <- function(mean,
                    sd = rep(NA, length(mean)),
                    digits = c(1, 1),
                    width = c(4, 3) + digits) {
  .mean <- formatC(mean, digits = digits[1], format = "f", width = width[1])
  ifelse(is.na(sd), .mean, {
    .sd <- formatC(sd, digits = digits[2], format = "f", width = width[2])
    paste0(.mean, " (", .sd, ")")
  })
}

#' Format confidence interval
#'
#' @param lower A numeric value of lower value of CI.
#' @param upper A numeric value of upper value of CI.
#' @param digits Digits of each column, i.e., format as (x.x, x.x).
#' @param width Width of each column.
#'
#' @return A numeric vector with the expected format.
#'
#' @export
#'
#' @examples
#' fmt_ci(0.2356, 0.3871)
fmt_ci <- function(lower, upper, digits = 2, width = 3 + digits) {
  .lower <- formatC(lower, digits = digits, format = "f", width = width)
  .upper <- formatC(upper, digits = digits, format = "f", width = width)

  ifelse(is.na(lower), NA, paste0("(", .lower, ", ", .upper, ")"))
}

#' Format p-value
#'
#' @param p A numeric vector of p-values.
#' @param digits Digits of each column, i.e., format as x.xxx.
#' @param width Width of each column.
#'
#' @return A numeric vector with the expected format.
#'
#' @importFrom dplyr if_else
#'
#' @export
#'
#' @examples
#' fmt_pval(0.1234)
fmt_pval <- function(p, digits = 3, width = 3 + digits) {
  scale <- 10^(-1 * digits)
  p_scale <- paste0("<", scale)
  if_else(p < scale, p_scale,
    formatC(p, digits = digits, format = "f", width = width)
  )
}
