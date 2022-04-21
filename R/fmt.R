#    Copyright (c) 2022 Merck Sharp & Dohme Corp. a subsidiary of Merck & Co., Inc., Kenilworth, NJ, USA.
#
#    This file is part of the metalite.ae program.
#
#    metalite.ae is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Format percentage
#'
#' @param x a numeric vector
#' @param digits number of digits
#' @param pre text before the number
#' @param post text after the number
#' @importFrom stats model.response pchisq pnorm qchisq sd
#' 
#' @examples
#' metalite.ae:::fmt_pct(c(1, 1.52, 0.3, 100))
fmt_pct <- function(x, digits = 1, pre = "(", post = ")") {
  x1 <- ifelse(is.na(x), x, formatC(x, digits = digits, format = "f"))

  x2 <- ifelse(is.na(x1), x1, paste0(pre, x1, post))

  ifelse(is.na(x2), x2, formatC(x2, format = "f"))
}

#' Format Model Estimator
#' @description
#' The Format Model Estimator function format mean sd/se to a format as x.x or x.x (x.xx)
#' if both mean and sd/se are defined.
#'
#'
#' The function assume 1 or 2 column.
#'   If there is only 1 column, only represent mean
#'   If there are 2 column, represent mean (sd) or mean(se)
#' Decimals will understand the number will be formated as x.x (x.xx)
#'
#' @param mean a numeric vector of mean value
#' @param sd a numeric vector of sd value
#' @param digits digits of each column, i.e. format as x.x (x.xx)
#' @param width width of each column
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
#' @return the same data frame \code{tbl} with additional attributes for page features
#'
#' @examples
#' library(dplyr) # required to run examples
#' data(iris)
#' x <- iris |>
#'   summarise(mean = mean(Petal.Length), n = n(), sd = sd(Petal.Length)) 
#' fmt_est(x$mean, x$sd)
#' @export
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

#' Format Confidence Interval
#'
#' @param lower a numeric value of lower value of CI
#' @param upper a numeric value of lower value of CI
#' @param digits digits of each column, i.e. format as (x.x, x.x)
#' @param width width of each column
#' @export
fmt_ci <- function(lower, upper, digits = 2, width = 3 + digits) {
  .lower <- formatC(lower, digits = digits, format = "f", width = width)
  .upper <- formatC(upper, digits = digits, format = "f", width = width)

  res <- ifelse(is.na(lower), NA, paste0("(", .lower, ", ", .upper, ")"))
}


#' Format P-Value
#'
#' @param p a numeric vector of p-values
#' @param digits digits of each column, i.e. format as x.xxx
#' @param width width of each column
#' @export
#'
fmt_pval <- function(p, digits = 3, width = 3 + digits) {
  scale <- 10^(-1 * digits)
  p_scale <- paste0("<", scale)
  if_else(p < scale, p_scale,
    formatC(p, digits = digits, format = "f", width = width)
  )
}
