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

# Internal helper: round half away from zero.
#
# Rounds numeric values to a given number of decimal places, with decimal
# ties (e.g., 2.25 at `digits = 1`) rounded half away from zero
# (i.e., 2.25 becomes 2.3 and -2.25 becomes -2.3). This follows the
# `roundSAS` algorithm from `pharmaverse/tidytlg` and therefore differs
# from base R `round()`, which rounds ties to an even digit.
# Values that round to zero (including small negative values) return
# positive zero, so formatted output never shows negative zero.
round_half_away_from_zero <- function(x, digits = 0) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, function(col) round_half_away_from_zero(col, digits = digits))
    return(x)
  }

  d <- dim(x)
  dn <- dimnames(x)

  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z <- ifelse(!is.na(z) & z > 0, z * posneg, z)

  dim(z) <- d
  dimnames(z) <- dn

  z
}

# Internal helper: format a number with fixed decimals.
#
# Rounds with `round_half_away_from_zero()` (decimal ties go half away
# from zero) and formats with fixed decimal places. The result never
# displays negative zero: values rounding to zero format as `"0.0"`,
# not `"-0.0"`.
format_number <- function(x, digits = 1, width = NULL) {
  x <- round_half_away_from_zero(x, digits = digits)

  out <- if (is.null(width)) {
    formatC(x, digits = digits, format = "f")
  } else {
    formatC(x, digits = digits, format = "f", width = width)
  }

  # Belt-and-braces safeguard against negative zero strings (e.g., "-0.0",
  # including width-padded variants such as " -0.0").
  is_neg_zero <- !is.na(out) & grepl("^\\s*-0(\\.0*)?\\s*$", out)
  out[is_neg_zero] <- gsub("-", "", out[is_neg_zero], fixed = TRUE)

  out
}

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
  x1 <- ifelse(is.na(x), x, format_number(x, digits = digits))

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
#' @param sd A numeric vector of standard deviation value.
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
#' fmt_est(mean(iris$Petal.Length), sd(iris$Petal.Length))
#' fmt_est(mean(iris$Petal.Length), sd(iris$Petal.Length), digits = c(2, 3))
fmt_est <- function(mean,
                    sd = rep(NA, length(mean)),
                    digits = c(1, 1),
                    width = c(4, 3) + digits) {
  .mean <- ifelse(is.na(mean), mean, format_number(mean, digits = digits[1], width = width[1]))
  ifelse(is.na(sd),
    .mean,
    {
      .sd <- format_number(sd, digits = digits[2], width = width[2])
      paste0(.mean, " (", .sd, ")")
    }
  )
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
  .lower <- format_number(lower, digits = digits, width = width)
  .upper <- format_number(upper, digits = digits, width = width)

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
#' @export
#'
#' @examples
#' fmt_pval(c(0.1234, 0.00002))
fmt_pval <- function(p, digits = 3, width = 3 + digits) {
  scale <- 10^(-1 * digits)
  p_scale <- paste0("<", scale)
  ifelse(p < scale,
    p_scale,
    format_number(p, digits = digits, width = width)
  )
}
